(ns erp12.schema-inference.impl.util
  (:require [clojure.set :as set]
            [erp12.schema-inference.impl.ground :as g]
            [erp12.schema-inference.impl.typeclasses :as tc]))

(defn ground?
  "Checks if a given schema represents a ground type.
  A ground type is a simple, non-composite type (e.g., 'int?, 'string?).
  It verifies that the schema has only a :type key, the type is an identifier
  (like a keyword or symbol) or a class, and is not a schema variable (:s-var)."
  [{:keys [type] :as schema}]
  (and (= (count schema) 1)
       (or (ident? type) (class? type))
       (not= type :s-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti free-type-vars
  "Returns a set of free type variable symbols (e.g., 'T, 'U) within a given schema.
  Dispatch is based on the schema's :type. If the schema is a ground type,
  it dispatches on :ground."
  (fn [s] (if (ground? s) :ground (:type s))))

(defmethod free-type-vars :ground
  ;; "Ground types have no free type variables."
  [_] #{})

(defn- free-type-vars-ctor1 [{:keys [child]}] (free-type-vars child))

(defmethod free-type-vars :vector
  ;; "Free variables in a :vector schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))

(defmethod free-type-vars :set
  ;; "Free variables in a :set schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))

(defmethod free-type-vars :sequential
  ;; "Free variables in a :sequential schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))

(defmethod free-type-vars :maybe
  ;; "Free variables in a :maybe schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))

(defn- free-type-vars-ctorN
  [{:keys [children]}]
  (reduce #(set/union %1 (free-type-vars %2)) #{} children))

(defmethod free-type-vars :tuple
  ;; "Free variables in a :tuple schema are the union of free variables in its children schemas."
  [schema] (free-type-vars-ctorN schema))

(defmethod free-type-vars :cat
  ;; "Free variables in a :cat schema (category, typically for function inputs)
  ;are the union of free variables in its children schemas."
  [schema] (free-type-vars-ctorN schema))

(defmethod free-type-vars :map-of
  ;; "Free variables in a :map-of schema are the union of free variables
  ;in its key and value schemas."
  [{:keys [key value]}]
  (set/union (free-type-vars key) (free-type-vars value)))

(defmethod free-type-vars :=>
  ;; "Free variables in a function schema (:=>) are the union of free variables
  ;; in its input and output schemas."
  [{:keys [input output]}]
  (set/union (free-type-vars input) (free-type-vars output)))

(defmethod free-type-vars :s-var
  ;; "The free variable in a schema variable (:s-var) is the symbol of the
  ;; schema variable itself."
  [{:keys [sym]}] #{sym})

(defmethod free-type-vars :scheme
  ;; "Free variables in a scheme (:scheme) are those in its body,
  ;; excluding the scheme's own quantified variables (:s-vars)."
  [{:keys [s-vars body]}]
  (set/difference (free-type-vars body)
                  (set (map :sym s-vars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn free-type-vars-env
  "Computes the set of all free type variables present in an environment.
  An environment is a map of symbols to schemas. This function iterates through
  each schema in the environment and collects all unique free type variables."
  [env]
  (reduce #(set/union %1 (free-type-vars (val %2)))
          #{}
          env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti get-free-s-vars-defs
  "Returns a set of free type variable definitions (maps like {:sym 'a :typeclasses [...]})
  within a given schema. Dispatch is based on the schema's :type."
  (fn [s] (if (ground? s) :ground (:type s))))

(defmethod get-free-s-vars-defs :ground [_] #{})

(defn- get-free-s-vars-defs-ctor1 [{:keys [child]}] (get-free-s-vars-defs child))

(defmethod get-free-s-vars-defs :vector [schema] (get-free-s-vars-defs-ctor1 schema))
(defmethod get-free-s-vars-defs :set [schema] (get-free-s-vars-defs-ctor1 schema))
(defmethod get-free-s-vars-defs :sequential [schema] (get-free-s-vars-defs-ctor1 schema))
(defmethod get-free-s-vars-defs :maybe [schema] (get-free-s-vars-defs-ctor1 schema))

(defn- get-free-s-vars-defs-ctorN [{:keys [children]}]
  (reduce #(set/union %1 (get-free-s-vars-defs %2)) #{} children))

(defmethod get-free-s-vars-defs :tuple [schema] (get-free-s-vars-defs-ctorN schema))
(defmethod get-free-s-vars-defs :cat [schema] (get-free-s-vars-defs-ctorN schema))

(defmethod get-free-s-vars-defs :map-of [{:keys [key value]}]
  (set/union (get-free-s-vars-defs key) (get-free-s-vars-defs value)))

(defmethod get-free-s-vars-defs :=> [{:keys [input output]}]
  (set/union (get-free-s-vars-defs input) (get-free-s-vars-defs output)))

(defmethod get-free-s-vars-defs :s-var [s-var-def] #{(dissoc s-var-def :type)})

(defmethod get-free-s-vars-defs :scheme [{:keys [s-vars body]}]
  (set/difference (get-free-s-vars-defs body)
                  (set s-vars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @todo Consider generic substitution function (ie. clojure.walk/postwalk-replace) that replaces more than s-vars.

(defmulti substitute
  "Recursively replaces type variable symbols (e.g., 'T) in a given schema
  with their corresponding schemas from a substitution map (`subs`).
  The dispatch is based on the schema's :type. If the schema is a ground type,
  it dispatches on :ground."
  (fn [_ x] (if (ground? x) :ground (:type x))))

(defmethod substitute :ground
  ;; "For ground types, attempts to map the :type to a canonical ground type
  ;; predicate (e.g., :int -> 'int?) if present in `g/canonical-ground`.
  ;; Otherwise, the type is returned as is. No actual substitution of schema
  ;; variables occurs as ground types do not contain them."
  [_ schema]
  (update schema :type #(get g/canonical-ground % %)))

(defmethod substitute :=>
  ;; "Substitutes type variables in the input and output schemas of a function schema (:=>)."
  [subs {:keys [input output]}]
  {:type   :=>
   :input  (substitute subs input)
   :output (substitute subs output)})

(defmethod substitute :s-var
  ;; "For a schema variable (:s-var), if its symbol is found as a key in the
  ;; substitution map (`subs`), it is replaced with the corresponding schema.
  ;; Otherwise, the schema variable is returned unchanged."
  [subs s-var]
  (get subs (:sym s-var) s-var))

(defn- substitute-ctor1
  [subs {:keys [child] :as schema}]
  (assoc schema :child (substitute subs child)))

(defmethod substitute :vector
  ;; "Substitutes type variables in the child schema of a :vector schema."
  [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :set
  ;; "Substitutes type variables in the child schema of a :set schema."
  [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :sequential
  ;; "Substitutes type variables in the child schema of a :sequential schema."
  [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :maybe
  ;; "Substitutes type variables in the child schema of a :maybe schema."
  [subs schema] (substitute-ctor1 subs schema))

(defn- substitute-ctorN
  [subs {:keys [children] :as schema}]
  (assoc schema :children (mapv #(substitute subs %) children)))

(defmethod substitute :tuple
  ;; "Substitutes type variables in all children schemas of a :tuple schema."
  [subs schema] (substitute-ctorN subs schema))
(defmethod substitute :cat
  ;; "Substitutes type variables in all children schemas of a :cat schema."
  [subs schema] (substitute-ctorN subs schema))

(defmethod substitute :map-of
  ;; "Substitutes type variables in the key and value schemas of a :map-of schema."
  [subs {:keys [key value] :as map-of}]
  (assoc map-of
    :key (substitute subs key)
    :value (substitute subs value)))

(defmethod substitute :scheme
  ;; "Substitutes type variables in the body of a scheme (:scheme).
  ;; Crucially, the scheme's own quantified variables (:s-vars) are removed
  ;; from the substitution map (`subs`) before substituting into the body.
  ;; This prevents accidental substitution of locally bound type variables
  ;; if they happen to have the same symbols as free variables in `subs`."
  [subs {:keys [s-vars body] :as scheme}]
  (assoc scheme
    :body (substitute (apply dissoc subs (map :sym s-vars))
                      body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn substitute-env
  "Applies a substitution map (`subs`) to all schemas within an environment (`env`).
  The environment is a map of symbols to schemas. Returns a new environment
  with the substituted schemas."
  [subs env]
  (->> env
       (map (fn [[sym schema]] [sym (substitute subs schema)]))
       (into {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compose-substitutions
  "Combines 2 sets of type substitutions."
  [subs1 subs2]
  (into subs1
        (->> subs2
             (map (fn [[k v]]
                    [k (substitute subs1 v)]))
             (into {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti instantiate
  "Replaces universally quantified type variables in a type scheme with fresh,
  unique type variables (generated using gensym). This process makes a generic
  type scheme concrete for a specific use.
  For non-scheme types, it returns the schema unchanged.
  Dispatch is based on the schema's :type."
  :type)

(defmethod instantiate :scheme
  ;; "For a type scheme (:scheme), replaces its quantified type variables (:s-vars)
  ;; within its body with new, unique (gensym'd) schema variables.
  ;; This effectively creates a fresh copy of the scheme's body with new variables."
  [{:keys [s-vars body]}]
  (let [fresh-vars (mapv (fn [s-var]
                           (cond-> {:type :s-var :sym (gensym "s-")}
                             (:typeclasses s-var) (assoc :typeclasses (:typeclasses s-var))))
                         s-vars)
        subs (zipmap (map :sym s-vars) fresh-vars)]
    (substitute subs body)))

(defmethod instantiate :default
  ;; "Default case for instantiate: if the schema is not a type scheme,
  ;; it is returned unchanged as there are no quantified variables to instantiate."
  [schema] schema)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generalize
  "Takes an environment (`env`) and a schema, and produces a type scheme.
  It identifies all free type variables in the input `schema` that are *not*
  free in the `env`. These variables are then universally quantified (generalized)
  to form a new :scheme.
  If no such variables exist (i.e., all free variables in the schema are already
  free in the environment or the schema has no free variables), it returns the
  original schema without creating a scheme."
  [env schema]
  (let [schema-instance (instantiate schema) ; avoid calling instantiate on already instantiated schema
        env-free-vars-syms (free-type-vars-env env)
        schema-free-s-vars-defs (get-free-s-vars-defs schema-instance)
        s-var-defs-to-generalize (filter #(not (contains? env-free-vars-syms (:sym %)))
                                         schema-free-s-vars-defs)
        sorted-s-var-defs (sort-by :sym (vec s-var-defs-to-generalize))]
    (if (empty? sorted-s-var-defs)
      schema-instance
      {:type   :scheme
       :s-vars sorted-s-var-defs
       :body   schema-instance})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn satisfies-all-typeclasses?
  "Checks if a given schema satisfies a collection of typeclass keywords.
  - schema: The schema to check (e.g., {:type 'int?} or {:type :s-var ...}).
  - typeclass-keywords: A collection of keywords (e.g., [:number]) from an s-var that `schema` must satisfy."
  [schema typeclass-keywords]
  (if (or (empty? typeclass-keywords) (nil? typeclass-keywords))
    true ; No constraints to satisfy.
    (let [schema-type (:type schema)]
      (cond
        ;; Case 1: The schema being checked is another schematic variable.
        (= schema-type :s-var)
        (let [svar-to-check-typeclasses (set (:typeclasses schema))]
          (if (empty? svar-to-check-typeclasses)
            true ; The s-var being checked is unconstrained, so it satisfies any requirement.
            ;; Otherwise, the s-var being checked must have all the required typeclasses.
            (set/subset? (set typeclass-keywords) svar-to-check-typeclasses)))

        ;; Case 2: The schema being checked is a concrete type or a structured type.
        ;; It must satisfy every typeclass keyword listed in `typeclass-keywords`.
        :else
        (every?
         (fn [tc-keyword]
           (if-let [allowed-types (get tc/typeclasses tc-keyword)] ; Use tc/typeclasses
             (cond
               (ground? schema) (contains? allowed-types schema-type)
               ;; For other keywords like :vector, :map-of, :=> etc. (but not :s-var, handled above)
               (keyword? schema-type) (contains? allowed-types schema-type)
               (class? schema-type) (contains? allowed-types schema-type) ; Assumes direct match or appropriate setup in typeclasses map
               :else false) ; Schema type not applicable for this typeclass check.
             false)) ; tc-keyword not found in tc/typeclasses.
         typeclass-keywords)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most General Unifier

(defn- mgu-dispatch
  "Dispatch function for mgu. Dispatch is only on the types of the first two schemas a and b."
  [a b]
  (cond
    (and (= (:type a) :maybe)
         (= (:type b) :maybe))
    [:maybe :maybe]

    (= (:type a) :s-var) [:s-var :_]
    (= (:type b) :s-var) [:_ :s-var]
    :else [(:type a) (:type b)]))

(defn mgu-failure?
  "Checks if a given result `x` from an MGU computation indicates a failure.
  A failure is represented as a map containing an ::mgu-failure key."
  [x]
  (and (map? x) (some? (:mgu-failure x))))

(defmulti mgu
  "Computes the Most General Unifier (MGU) for two schemas, `a` and `b`,
  considering typeclass constraints (using global tc/typeclasses).
  The MGU is a substitution map that, when applied to both `a` and `b`,
  makes them identical. If no such substitution exists, it returns a map
  indicating an MGU failure (see `mgu-failure?`).
  Dispatch is based on a vector of the types of both input schemas,
  handling schema variables (:s-var) specially."
  mgu-dispatch)

;; Helper to call mgu and then a function on its non-failure result.
(defn- with-mgu
  [schema1 schema2 result-fn]
  (let [result (mgu schema1 schema2)]
    (if (mgu-failure? result)
      result
      (result-fn result))))

(defn- bind-var
  "Attempts to bind schematic variable s-var to schema.
  Performs occurs check and typeclass compatibility check (using global tc/typeclasses)."
  [{:keys [sym typeclasses] :as s-var} schema]
  (cond
    (= s-var schema) {}

    ;; Case: both are schematic vars -> merge them
    (= (:type schema) :s-var)
    (let [{other-sym :sym other-tcs :typeclasses} schema
          merged-tcs (set/union typeclasses other-tcs)
          ;; You can choose to keep one of the names or generate a fresh one
          new-sym (gensym "s")
          new-svar {:type :s-var :sym new-sym :typeclasses merged-tcs}]
      {sym new-svar
       other-sym new-svar})

    ;; Occurs check
    (contains? (free-type-vars schema) sym)
    {:mgu-failure :occurs-check
     :schema-1    s-var
     :schema-2    schema}

    ;; Typeclass check
    (and (not-empty typeclasses)
         (not (satisfies-all-typeclasses? schema typeclasses)))
    (let [violated-typeclasses (set (filterv #(not (satisfies-all-typeclasses? schema [%])) typeclasses))]
      {:mgu-failure       :typeclass-mismatch
       :s-var             s-var
       :schema            schema
       :missing-typeclasses violated-typeclasses})

    ;; Default: bind s-var to concrete schema
    :else {sym schema}))

(defmethod mgu [:s-var :_]
  ;; "Unifies a schema variable `a` with schema `b`."
  [a b] (bind-var a b))

(defmethod mgu [:_ :s-var]
  ;; "Unifies schema `a` with a schema variable `b`."
  [a b] (bind-var b a))

(defn- mgu-schema-ctor1
  [{a-type :type a-child :child :as a} {b-type :type b-child :child :as b}]
  (if (not= a-type b-type)
    {:mgu-failure :mismatched-schema-ctor
     :schema-1    a
     :schema-2    b}
    (mgu a-child b-child)))

(defmethod mgu [:vector :vector]
  [a b] (mgu-schema-ctor1 a b))

(defmethod mgu [:set :set]
  [a b] (mgu-schema-ctor1 a b))

(defmethod mgu [:sequential :sequential]
  [a b] (mgu-schema-ctor1 a b))

(defmethod mgu [:maybe :maybe]
  [a b] (mgu-schema-ctor1 a b))

(defn- mgu-schema-ctorN
  [{a-type :type a-children :children :as a}
   {b-type :type b-children :children :as b}]
  (cond
    (not= a-type b-type)
    {:mgu-failure :mismatched-schema-ctor
     :schema-1    a
     :schema-2    b}

    (not= (count a-children) (count b-children))
    {:mgu-failure :mismatched-arity
     :schema-1    a
     :schema-2    b}

    :else
    (->> (map vector a-children b-children)
         (reduce (fn [subs [a-child b-child]]
                   (if (mgu-failure? subs)
                     subs
                     (with-mgu (substitute subs a-child)
                               (substitute subs b-child)
                               #(compose-substitutions % subs))))
                 {}))))

(defmethod mgu [:tuple :tuple]
  [a b] (mgu-schema-ctorN a b))

(defmethod mgu [:cat :cat]
  [a b] (mgu-schema-ctorN a b))

(defmethod mgu [:map-of :map-of]
  [{a-key :key a-value :value} {b-key :key b-value :value}]
  (with-mgu a-key b-key
            (fn [key-subs]
              (with-mgu (substitute key-subs a-value)
                        (substitute key-subs b-value)
                        (fn [value-subs]
                          (compose-substitutions value-subs key-subs))))))

(defmethod mgu [:=> :=>]
  [{a-input :input a-output :output :as a} {b-input :input b-output :output :as b}]
  ;; @todo Support other function args (named, variatic) aside from :cat
  (if (or (not= (:type a-input) :cat)
          (not= (:type b-input) :cat))
    {:mgu-failure :non-positional-args
     :schema-1    a
     :schema-2    b}
    (with-mgu a-input b-input
              (fn [subs]
                (with-mgu (substitute subs a-output)
                          (substitute subs b-output)
                          #(compose-substitutions % subs))))))

(defmethod mgu :default
  ;; "Default unification rule: two schemas can unify if and only if they are identical."
  [a b]
  (if (= a b)
    {}
    {:schema-1    a
     :schema-2    b
     :mgu-failure :non-equal}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sub-schema

;; @todo Add support for 'any?

(defn- sub-schema?-sub-dispatch
  [{:keys [type] :as schema}]
  (cond
    (class? type) :class
    (ground? schema) :ground
    :else type))

(defn- sub-schema?-dispatch
  [sub sup]
  [(sub-schema?-sub-dispatch sub)
   (sub-schema?-sub-dispatch sup)])

(defmulti sub-schema?
  "Checks if the first schema (`sub`) is a sub-schema of the second schema (`sup`).
  A schema `A` is a sub-schema of `B` if any value described by `A` is also
  described by `B`. For example, `Integer` is a sub-schema of `Number`.
  Dispatch is based on a vector of categories for `sub` and `sup` schemas,
  determined by the `sub-schema?-sub-dispatch` helper function. This helper
  categorizes schemas as :class (for Java class types), :ground (for simple
  ground types), or by their specific :type keyword for other schema forms."
  sub-schema?-dispatch)

(defmethod sub-schema? :default
  ;; "Default behavior for sub-schema checking.
  ;; Currently throws an exception indicating that sub-schema checking is not
  ;; yet supported for the given combination of non-class schema types.
  ;; This typically means at least one of the schemas is not a Java class type,
  ;; and no specific sub-typing rule has been defined for their types."
  [sub sup]
  (throw (ex-info "sub-schema? not yet supported for non-class schemas."
                  {:sub sub :sup sup})))

(defmethod sub-schema? [:class :class]
  ;; "Checks if a class schema `sub-type` is a sub-schema of another class
  ;; schema `sup-type`. This is true if `sub-type` is the same as `sup-type`
  ;; or a subclass of `sup-type`, as determined by `clojure.core/supers`."
  [{sub-type :type} {sup-type :type}]
  (contains? (supers sub-type) sup-type))
