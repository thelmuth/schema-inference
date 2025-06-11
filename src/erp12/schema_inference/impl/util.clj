(ns erp12.schema-inference.impl.util
  (:require [clojure.set :as set]
            [erp12.schema-inference.impl.ground :as g]))

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
  "Ground types have no free type variables."
  [_] #{})
(defn- free-type-vars-ctor1 [{:keys [child]}] (free-type-vars child))
(defmethod free-type-vars :vector
  "Free variables in a :vector schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))
(defmethod free-type-vars :set
  "Free variables in a :set schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))
(defmethod free-type-vars :sequential
  "Free variables in a :sequential schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))
(defmethod free-type-vars :maybe
  "Free variables in a :maybe schema are those in its child schema."
  [schema] (free-type-vars-ctor1 schema))

(defn- free-type-vars-ctorN
  [{:keys [children]}]
  (reduce #(set/union %1 (free-type-vars %2)) #{} children))

(defmethod free-type-vars :tuple
  "Free variables in a :tuple schema are the union of free variables in its children schemas."
  [schema] (free-type-vars-ctorN schema))
(defmethod free-type-vars :cat
  "Free variables in a :cat schema (category, typically for function inputs)
  are the union of free variables in its children schemas."
  [schema] (free-type-vars-ctorN schema))

(defmethod free-type-vars :map-of
  "Free variables in a :map-of schema are the union of free variables
  in its key and value schemas."
  [{:keys [key value]}]
  (set/union (free-type-vars key) (free-type-vars value)))

(defmethod free-type-vars :=>
  "Free variables in a function schema (:=>) are the union of free variables
  in its input and output schemas."
  [{:keys [input output]}]
  (set/union (free-type-vars input) (free-type-vars output)))

(defmethod free-type-vars :s-var
  "The free variable in a schema variable (:s-var) is the symbol of the
  schema variable itself."
  [{:keys [sym]}] #{sym})

(defmethod free-type-vars :scheme
  "Free variables in a scheme (:scheme) are those in its body,
  excluding the scheme's own quantified variables (:s-vars)."
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

;; @todo Consider generic substitution function (ie. clojure.walk/postwalk-replace) that replaces more than s-vars.

(defmulti substitute
  "Recursively replaces type variable symbols (e.g., 'T) in a given schema
  with their corresponding schemas from a substitution map (`subs`).
  The dispatch is based on the schema's :type. If the schema is a ground type,
  it dispatches on :ground."
  (fn [_ x] (if (ground? x) :ground (:type x))))

(defmethod substitute :ground
  "For ground types, attempts to map the :type to a canonical ground type
  predicate (e.g., :int -> 'int?) if present in `g/canonical-ground`.
  Otherwise, the type is returned as is. No actual substitution of schema
  variables occurs as ground types do not contain them."
  [_ schema]
  (update schema :type #(get g/canonical-ground % %)))

(defmethod substitute :=>
  "Substitutes type variables in the input and output schemas of a function schema (:=>)."
  [subs {:keys [input output]}]
  {:type   :=>
   :input  (substitute subs input)
   :output (substitute subs output)})

(defmethod substitute :s-var
  "For a schema variable (:s-var), if its symbol is found as a key in the
  substitution map (`subs`), it is replaced with the corresponding schema.
  Otherwise, the schema variable is returned unchanged."
  [subs s-var]
  (get subs (:sym s-var) s-var))

(defn- substitute-ctor1
  [subs {:keys [child] :as schema}]
  (assoc schema :child (substitute subs child)))

(defmethod substitute :vector
  "Substitutes type variables in the child schema of a :vector schema."
  [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :set
  "Substitutes type variables in the child schema of a :set schema."
  [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :sequential
  "Substitutes type variables in the child schema of a :sequential schema."
  [subs schema] (substitute-ctor1 subs schema))
(defmethod substitute :maybe
  "Substitutes type variables in the child schema of a :maybe schema."
  [subs schema] (substitute-ctor1 subs schema))

(defn- substitute-ctorN
  [subs {:keys [children] :as schema}]
  (assoc schema :children (mapv #(substitute subs %) children)))

(defmethod substitute :tuple
  "Substitutes type variables in all children schemas of a :tuple schema."
  [subs schema] (substitute-ctorN subs schema))
(defmethod substitute :cat
  "Substitutes type variables in all children schemas of a :cat schema."
  [subs schema] (substitute-ctorN subs schema))

(defmethod substitute :map-of
  "Substitutes type variables in the key and value schemas of a :map-of schema."
  [subs {:keys [key value] :as map-of}]
  (assoc map-of
    :key (substitute subs key)
    :value (substitute subs value)))

(defmethod substitute :scheme
  "Substitutes type variables in the body of a scheme (:scheme).
  Crucially, the scheme's own quantified variables (:s-vars) are removed
  from the substitution map (`subs`) before substituting into the body.
  This prevents accidental substitution of locally bound type variables
  if they happen to have the same symbols as free variables in `subs`."
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
  "For a type scheme (:scheme), replaces its quantified type variables (:s-vars)
  within its body with new, unique (gensym'd) schema variables.
  This effectively creates a fresh copy of the scheme's body with new variables."
  [{:keys [s-vars body]}]
  (let [fresh-vars (repeatedly (count s-vars) (fn [] {:type :s-var :sym (gensym "s-")}))
        subs (zipmap (map :sym s-vars) fresh-vars)]
    (substitute subs body)))

(defmethod instantiate :default
  "Default case for instantiate: if the schema is not a type scheme,
  it is returned unchanged as there are no quantified variables to instantiate."
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
  (let [schema (instantiate schema)
        s-vars (sort (set/difference (free-type-vars schema) (free-type-vars-env env)))]
    (if (empty? s-vars)
      schema
      {:type   :scheme
       :s-vars (vec (map (fn [sym] {:sym sym}) s-vars))
       :body   schema})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most General Unifier

(defn- mgu-dispatch
  [{a-type :type :as a} {b-type :type :as b}]
  (cond
    (and (= a-type :maybe)
         (= b-type :maybe))
    [:maybe :maybe]

    (= a-type :s-var) [:s-var :_]
    (= b-type :s-var) [:_ :s-var]
    :else [a-type b-type]))

(defn mgu-failure?
  "Checks if a given result `x` from an MGU computation indicates a failure.
  A failure is represented as a map containing an ::mgu-failure key."
  [x]
  (and (map? x) (some? (:mgu-failure x))))

(defmulti mgu
  "Computes the Most General Unifier (MGU) for two schemas, `a` and `b`.
  The MGU is a substitution map that, when applied to both `a` and `b`,
  makes them identical. If no such substitution exists, it returns a map
  indicating an MGU failure (see `mgu-failure?`).
  Dispatch is based on a vector of the types of both input schemas,
  handling schema variables (:s-var) specially."
  mgu-dispatch)

(defn- with-mgu
  [schema1 schema2 fn]
  (let [result (mgu schema1 schema2)]
    (if (mgu-failure? result)
      result
      (fn result))))

(defn- bind-var
  [{:keys [sym] :as s-var} schema]
  (cond
    (= s-var schema) {}

    (contains? (free-type-vars schema) sym)
    {:mgu-failure :occurs-check
     :schema-1    s-var
     :schema-2    schema}

    :else {sym schema}))

(defmethod mgu [:s-var :_]
  "Unifies a schema variable `a` with schema `b`.
  It attempts to bind the variable `a` to `b`.
  Fails (occurs-check) if `a` is already free in `b`.
  If `a` and `b` are identical, returns an empty substitution."
  [a b] (bind-var a b))
(defmethod mgu [:_ :s-var]
  "Unifies schema `a` with a schema variable `b`.
  It attempts to bind the variable `b` to `a`.
  Fails (occurs-check) if `b` is already free in `a`.
  If `a` and `b` are identical, returns an empty substitution."
  [a b] (bind-var b a))

(defn- mgu-schema-ctor1
  [{a-type :type a-child :child :as a} {b-type :type b-child :child :as b}]
  (if (not= a-type b-type)
    {:mgu-failure :mismatched-schema-ctor
     :schema-1    a
     :schema-2    b}
    (mgu a-child b-child)))

(defmethod mgu [:vector :vector]
  "Unifies two :vector schemas by unifying their child schemas.
  Fails if the schema constructors themselves do not match (handled by mgu-schema-ctor1)."
  [a b] (mgu-schema-ctor1 a b))
(defmethod mgu [:set :set]
  "Unifies two :set schemas by unifying their child schemas.
  Fails if the schema constructors themselves do not match (handled by mgu-schema-ctor1)."
  [a b] (mgu-schema-ctor1 a b))
(defmethod mgu [:sequential :sequential]
  "Unifies two :sequential schemas by unifying their child schemas.
  Fails if the schema constructors themselves do not match (handled by mgu-schema-ctor1)."
  [a b] (mgu-schema-ctor1 a b))
(defmethod mgu [:maybe :maybe]
  "Unifies two :maybe schemas by unifying their child schemas.
  Fails if the schema constructors themselves do not match (handled by mgu-schema-ctor1)."
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
  "Unifies two :tuple schemas by unifying their children schemas element-wise.
  Fails if schema constructors or arity (number of children) do not match
  (handled by mgu-schema-ctorN)."
  [a b] (mgu-schema-ctorN a b))
(defmethod mgu [:cat :cat]
  "Unifies two :cat schemas by unifying their children schemas element-wise.
  Fails if schema constructors or arity (number of children) do not match
  (handled by mgu-schema-ctorN)."
  [a b] (mgu-schema-ctorN a b))

(defmethod mgu [:map-of :map-of]
  "Unifies two :map-of schemas.
  First, it unifies their key schemas. Then, it applies the resulting substitution
  to the value schemas and unifies them. The final MGU is the composition
  of substitutions from key and value unification."
  [{a-key :key a-value :value} {b-key :key b-value :value}]
  (with-mgu a-key b-key
            (fn [key-subs]
              (with-mgu (substitute key-subs a-value)
                        (substitute key-subs b-value)
                        (fn [value-subs]
                          (compose-substitutions value-subs key-subs))))))

(defmethod mgu [:=> :=>]
  "Unifies two function schemas (:=>).
  It requires both input schemas to be of type :cat. It first unifies the
  input schemas. Then, it applies the resulting substitution to the output
  schemas and unifies them. The final MGU is the composition of these substitutions.
  Fails if input types are not :cat or if unification at any step fails."
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
  "Default unification rule: two schemas can unify if and only if they are identical.
  This typically applies to ground types or other schemas not handled by more
  specific MGU methods. Returns an empty substitution for identical schemas,
  or an MGU failure if they are not equal."
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
  "Default behavior for sub-schema checking.
  Currently throws an exception indicating that sub-schema checking is not
  yet supported for the given combination of non-class schema types.
  This typically means at least one of the schemas is not a Java class type,
  and no specific sub-typing rule has been defined for their types."
  [sub sup]
  (throw (ex-info "sub-schema? not yet supported for non-class schemas."
                  {:sub sub :sup sup})))

(defmethod sub-schema? [:class :class]
  "Checks if a class schema `sub-type` is a sub-schema of another class
  schema `sup-type`. This is true if `sub-type` is the same as `sup-type`
  or a subclass of `sup-type`, as determined by `clojure.core/supers`."
  [{sub-type :type} {sup-type :type}]
  (contains? (supers sub-type) sup-type))
