(ns erp12.schema-inference.impl.algo_w
  (:require [clojure.datafy :refer [datafy]]
            [erp12.schema-inference.impl.util :as u]
            ;; [erp12.schema-inference.impl.typeclasses :as tc] ; No longer directly needed by algo-w
            [malli.core :as m]
            [malli.provider :as mp]))

(defmulti algo-w
  "Core of the schema inference algorithm (Algorithm W).
   Dispatches on the :op of the AST node.

   Parameters:
     - an AST node with an :op key
     - an environment, giving the typing context
   
   Returns: a map containing keys:
     - ::subs - a substitution — a mapping from type variables to types, which captures all the unifications made during inference.
     - ::schema - the schema/type of the expression, possibly containing type variables (which may be generalized later)."
  (fn [{:keys [op]} & _] op)) ; Dispatch only on op and env

(defn- algo-w-failure?
  [x]
  (and (map? x) (some? (::failure x))))

(defn infer-schema
  "Public entry point for schema inference.
  Calls `algo-w` to perform the inference and throws an exception on failure.
  Takes an AST node and an environment map as input.
  Returns the inferred schema for the AST node."
  [ast env]
  (let [{::keys [schema] :as result} (algo-w ast env)]
    (if (algo-w-failure? result)
      (throw (ex-info "Schema inference failure." result))
      schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambda Calc

;; @todo Propagate ::throw
;; @todo Provide more context info about failures.

(defmethod algo-w :LIT
  ;; "Handles literal values, converting them to their schema representation."
  [{:keys [type val]} _env]
  {::subs   {}
   ::schema (case type
              :class {:type Class}
              (m/ast (mp/provide [val]
                                 {::mp/map-of-threshold 0})))})

(defmethod algo-w :VAR
  [{:keys [sym]} env]
  (let [sym (symbol sym)]
    (if (contains? env sym)
      {::subs   {}
       ::schema (u/instantiate (get env sym))}
      {::failure {:var-not-found sym}})))

(defmethod algo-w :APP
  ;; "Handles function applications (invocations).
  ;; Infers schema of the function and arguments, then unifies the function's
  ;; input schema with the argument schemas to determine the output schema."
  [{:keys [fn args]} env]
  (let [s-var {:type :s-var :sym (gensym "s-")}
        {f-subs ::subs f-schema ::schema :as fn-result} (algo-w fn env)]
    (if (algo-w-failure? fn-result)
      fn-result
      (let [args-ti (loop [remaining-args args
                           env' (u/substitute-env f-subs env)
                           args-ti []]
                      (if (empty? remaining-args)
                        args-ti
                        (let [arg (first remaining-args)
                              {a-subs ::subs :as arg-ti} (algo-w arg env')]
                          (if (algo-w-failure? arg-ti)
                            arg-ti
                            (recur (rest remaining-args)
                                   (u/substitute-env a-subs env')
                                   (conj args-ti arg-ti))))))]
        (if (algo-w-failure? args-ti)
          args-ti
          (let [subs (->> args-ti
                          (map ::subs)
                          reverse
                          (reduce u/compose-substitutions {}))
                subs' (u/mgu (u/substitute subs f-schema)
                             {:type   :=>
                              :input  {:type     :cat
                                       :children (mapv ::schema args-ti)}
                              :output s-var})]
            (if (u/mgu-failure? subs')
              {::failure {:unification-failure subs'}}
              {::subs   (u/compose-substitutions subs' subs)
               ::schema (u/substitute subs' s-var)})))))))

(defmethod algo-w :ABS
  ;; "Handles abstractions (function definitions, e.g., `fn` forms).
  ;; Infers the schema of the function body within an environment
  ;; extended with type variables for the function parameters.
  ;; Returns a function schema (=>)."
  [{:keys [params body] :as ast} env]
  ;; @todo Support variadic functions.
  (when (some :variadic? params)
    (throw (ex-info "Variadic functions not supported." {:ast ast})))
  (let [param-names (map :name params)
        s-vars (vec (repeatedly (count params) #(hash-map :type :s-var :sym (gensym "s-"))))
        env' (into env (map vector param-names s-vars))
        {::keys [subs schema] :as result} (algo-w body env')]
    (if (algo-w-failure? result)
      result
      {::subs   subs
       ::schema {:type   :=>
                 :input  {:type     :cat
                          :children (mapv #(u/substitute subs %) s-vars)}
                 :output schema}})))

(defmethod algo-w :LET
  ;; "Handles let bindings (e.g., `let` forms).
  ;; Infers schemas for bindings sequentially, extending the environment for each.
  ;; Then infers the schema of the body using the fully extended environment."
  [{:keys [bindings body]} env]
  (loop [remaining bindings
         env' env
         subs {}]
    (if (empty? remaining)
      (let [{body-subs ::subs body-schema ::schema :as result} (algo-w body (u/substitute-env subs env'))]
        (if (algo-w-failure? result)
          result
          {::subs   (u/compose-substitutions body-subs subs)
           ::schema body-schema}))
      (let [{:keys [name init]} (first remaining)
            {local-subs ::subs local-schema ::schema :as result} (algo-w init env')]
        (if (algo-w-failure? result)
          result
          (let [env' (dissoc env' name) ; env' for next binding, not for generalize
                generalized-env (u/substitute-env local-subs env) ; env for generalize
                local-schema' (u/generalize generalized-env local-schema)]
            (recur (rest remaining)
                   (assoc env' name local-schema') ; use the correct env' for next binding
                   (u/compose-substitutions local-subs subs))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

(defmethod algo-w :binding
  ;; "Handles :binding AST nodes. This method should be unreachable
  ;; as :binding nodes are expected to be processed by their parent nodes (e.g., :LET)."
  [_env _op] (assert false "Should be unreachable."))

;(defmethod algo-w :case [ast env])
;(defmethod algo-w :case-test [ast env])
;(defmethod algo-w :case-then [ast env])

(defmethod algo-w :catch
  ;; "Handles :catch clauses within a :try expression.
  ;; Infers the schema of the catch clause's body.
  ;; Note: Exception type checking is a TODO."
  [{:keys [body]} env]
  (algo-w body env))

(defmethod algo-w :const
  ;; "Handles :const AST nodes, typically representing literal constants.
  ;; Delegates to the :LIT method by changing the :op of the AST node."
  [ast env]
  (algo-w (assoc ast :op :LIT) env))

(defmethod algo-w :def
  ;; "Handles :def expressions (defining vars).
  ;; Infers the schema of the init expression, if present.
  ;; The schema of a :def expression itself is always `var?`."
  [{:keys [name init]} env]
  {::subs   (if (nil? init)
              {}
              {name (infer-schema init env)})
   ::schema {:type 'var?}})

;(defmethod algo-w :deftype [ast env] {::subs })

(defmethod algo-w :do
  ;; "Handles :do expressions.
  ;; Infers the schema of the `ret` (return) expression within the :do block,
  ;; as this determines the overall schema of the :do form."
  [{:keys [ret]} env]
  (algo-w ret env))

(defmethod algo-w :fn
  ;; "Handles :fn expressions (function definitions).
  ;; Currently supports single-arity functions by delegating to the :ABS method.
  ;; Support for multiple arities (overloading) is a TODO."
  [{:keys [methods] :as ast} env]
  ;; @todo Support multiple methods (aka overloading).
  (if (= (count methods) 1)
    (algo-w (first methods) env)
    (throw (ex-info "Cannot infer schema of functions with multiple methods."
                    {:ast ast}))))

(defmethod algo-w :fn-method
  ;; "Handles individual method definitions within an :fn expression.
  ;; Delegates to the :ABS method for schema inference, treating each
  ;; method as a separate abstraction."
  [ast env]
  (algo-w (assoc ast :op :ABS) env))

;; @todo Support classes as ground types!
;; @todo Consider reflection instead of the type environment for interop ASTs. (JVM only).
;(defmethod algo-w :host-interop
;  [{:keys [target m-or-f]} env])

(defmethod algo-w :if
  ;;  "Handles :if expressions.
  ;;  Transforms the :if expression into an :APP (application) of the
  ;;  `clojure.core/if` var with the test, then, and else branches as arguments.
  ;;  The schema of `clojure.core/if` is expected to handle the unification
  ;;  of the `then` and `else` branch schemas."
  [{:keys [test then else]} env]
  (algo-w {:op   :APP
           :fn   {:op  :var
                  :var 'clojure.core/if}
           :args [test then else]}
          env))

(defmethod algo-w :import
  ;;  "Handles :import expressions.
  ;;  The schema of an :import expression is `nil?` as it does not produce a value.
  ;;  A TODO notes checking if the AST node is a symbol constant."
  [_ _env]
  ;; @todo Should this check the AST is a symbol constant?
  {::subs {} ::schema nil?})

;(defmethod algo-w :instance-call [ast env])
;(defmethod algo-w :instance-field [ast env])

(defmethod algo-w :instance?
  ;;  "Handles :instance? checks.
  ;;  The schema of an :instance? expression is always `boolean?`.
  ;;  This method assumes the Class argument is a constant in the AST.
  ;;  If it's a non-constant, an :invoke node would be used instead."
  [_ _env]
  ;; This AST node is used when a Class constant is in the AST, therefore we don't need to type check.
  ;; When a non-const AST is provided for the Class argument to instance?, an :invoke node will be used.
  {::subs {} ::schema 'boolean?})

(defmethod algo-w :invoke
  ;;  "Handles :invoke expressions (general function invocations).
  ;;  Delegates to the :APP method by changing the :op of the AST node."
  [ast env]
  (algo-w (assoc ast :op :APP) env))

;; @todo Implement record-like theory (HMaps, relational algebra, etc.)
;(defmethod algo-w :keyword-invoke [ast env])

(defmethod algo-w :let
  ;;  "Handles :let expressions (dispatching on the lowercase :let keyword).
  ;;  Delegates to the :LET method (uppercase keyword) for actual schema inference."
  [ast env]
  (algo-w (assoc ast :op :LET) env))

(defmethod algo-w :letfn
  ;;  "Handles :letfn expressions.
  ;;  Delegates to the :LET method for schema inference.
  ;;  A TODO notes that this currently has limitations with allowing
  ;;  ahead-of-definition use of functions defined in the letfn."
  [ast env]
  ;; @todo This incorrectly fails to allow ahead-of-definition use of functions.
  (algo-w (assoc ast :op :LET) env))

(defmethod algo-w :local
  ;;  "Handles :local AST nodes, representing local bindings or variables.
  ;;  Delegates to the :VAR method by creating a :VAR AST node with the local's name."
  [{:keys [name]} env]
  (algo-w {:op :VAR :sym name} env))

;(defmethod algo-w :loop [ast env])
;(defmethod algo-w :map [{:keys [keys vals}} env])
;(defmethod algo-w :method [ast env])
;(defmethod algo-w :new
;  [{:keys [class args]} env]
;  (let [fn-var (symbol (.getName String) "<init>")]
;    (algo-w {:op :APP
;             :fn {:op :var :var fn-var}
;             :args args}
;            (assoc env
;              fn-var {:type   :=>
;                      :input (-> class datafy :members
;                                 (get (symbol (.getName class)))
;                                 first :parameter-types
;                                 (map (fn [sym] (c/cls-name->schema (c/sym->cls sym)))))
;                      :output (c/cls-name->schema class)}))))

(defmethod algo-w :prim-invoke
  ;;  "Handles :prim-invoke expressions, representing invocations of primitive operations.
  ;;  Delegates to the :APP method for schema inference."
  [ast env]
  (algo-w (assoc ast :op :APP) env))

(defmethod algo-w :protocol-invoke
  ;;  "Handles :protocol-invoke expressions.
  ;;  Infers the schema of the protocol function and the target object.
  ;;  Checks if the target object's schema implements the protocol.
  ;;  If it does, it infers the schema for the remaining arguments using a
  ;;  'currying-ish' strategy, effectively applying the protocol method to the target
  ;;  and then to the subsequent arguments."
  [{:keys [target protocol-fn args]} env]
  (let [protocol (-> protocol-fn :meta :protocol deref :on-interface)
        {fn-schema ::schema fn-subs ::subs :as result} (algo-w protocol-fn env)]
    (if (algo-w-failure? result)
      result
      (let [env' (u/substitute-env fn-subs env)
            {target-schema ::schema target-subs ::subs :as result} (algo-w target env')]
        (cond
          (algo-w-failure? result)
          result

          ;; Use "currying-ish" strategy to check the rest of the args except the instance
          ;; of the protocol.
          (u/sub-schema? target-schema {:type protocol})
          (let [tmp-f (gensym "f-")]
            (algo-w {:op   :APP
                     :fn   {:op :local :name tmp-f}
                     :args args}
                    (assoc env'
                      tmp-f (u/substitute target-subs (update-in fn-schema [:input :children] rest)))))

          :else
          {::failure {:must-extend-protocol (.getName protocol)
                      :protocol-fn          protocol-fn
                      :got                  target-schema}})))))

(defmethod algo-w :quote
  ;;  "Handles :quote expressions.
  ;;  Infers the schema of the quoted expression itself."
  [{:keys [expr]} env]
  (algo-w expr env))

;(defmethod algo-w :recur [ast env])
;(defmethod algo-w :reify [ast env])
;(defmethod algo-w :set [{:keys [items]} env])

(defmethod algo-w :set!
  ;;  "Handles :set! expressions.
  ;;  Infers the schema of the value being set. The schema of the `set!` expression
  ;;  itself is the schema of this value."
  [{:keys [val]} env]
  (algo-w val env))

(defmethod algo-w :static-call
  ;;  "Handles :static-call expressions (Java static method calls).
  ;;  Transforms the static call into an :APP (application) of a var
  ;;  representing the static method. The var is constructed using the class
  ;;  and method name from the AST."
  [{:keys [class method] :as ast} env]
  (algo-w (assoc ast
            :op :APP
            :fn {:op  :var
                 :var (symbol (.getName class) (name method))})
          env))

(defmethod algo-w :static-field
  ;;  "Handles :static-field AST nodes (Java static field access).
  ;;  The schema is determined by reflecting on the field's type using `datafy`
  ;;  and `Class/forName`."
  [{:keys [class field]} _env]
  {::subs   {}
   ::schema {:type (-> (datafy class)
                       :members
                       (get field)
                       first
                       :type
                       name
                       Class/forName)}})

(defmethod algo-w :the-var
  ;;  "Handles :the-var special form.
  ;;  The schema of a :the-var expression is always `var?`."
  [_ _env]
  {::subs   {}
   ::schema {:type 'var?}})

(defmethod algo-w :throw
  ;;  "Handles :throw expressions.
  ;;  The schema of a :throw expression is the special keyword `::throw`.
  ;;  A TODO notes that type checking the thrown exception (confirming it's a
  ;;  subtype of Throwable) should be implemented."
  [_ _env]
  ;; @todo Should type check the `exception` child AST, and confirm subtype of Throwable.
  {::subs {} ::schema ::throw})

(defmethod algo-w :try
  ;;  "Handles :try expressions.
  ;;  Infers the schema of the main body of the :try expression.
  ;;  TODO: Type checking for :catch clauses (ensuring the body of the :catch
  ;;  unifies with the body of the :try, and that caught exceptions are subtypes
  ;;  of Throwable) should be implemented."
  [{:keys [body]} env]
  ;; @todo Type check the catch clauses.
  (algo-w body env))

(defmethod algo-w :var
  ;;  "Handles :var expressions, which refer to Clojure vars.
  ;;  Delegates to the :VAR method (uppercase keyword) by creating a
  ;;  :VAR AST node with the var's symbol."
  [{:keys [var]} env]
  (algo-w {:op :VAR :sym var} env))

;(defmethod algo-w :vector [{:keys [items]} env])

(defmethod algo-w :with-meta
  ;;  "Handles :with-meta expressions.
  ;;  Infers the schema of the underlying expression, as metadata does not affect
  ;;  the core schema of a value."
  [{:keys [expr]} env]
  (algo-w expr env))