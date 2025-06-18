(ns erp12.schema-inference.impl.algo_w-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.tools.analyzer.jvm :as ana]
            [erp12.schema-inference.impl.algo_w :refer [algo-w] :as a]
            [erp12.schema-inference.api :as schema-inf]) ; For api/infer-schema tests
  (:import (java.io PrintStream)))

;; Keep some empty vars to be used arbitrarily in test ASTs.
(declare f)

;; @todo Test type checker failures

(def test-env
  (let [inc-type {:type :scheme
                  :s-vars [{:sym 'a, :typeclasses #{:number}}]
                  :body {:type :=>
                         :input {:type :cat
                                 :children [{:type :s-var, :sym 'a}]}
                         :output {:type :s-var, :sym 'a}}}
        add-type {:type :scheme
                  :s-vars [{:sym 'a, :typeclasses #{:number}}]
                  :body {:type :=>
                         :input {:type :cat
                                 :children [{:type :s-var, :sym 'a} {:type :s-var, :sym 'a}]}
                         :output {:type :s-var, :sym 'a}}}]
    {'clojure.lang.Numbers/inc
     inc-type

     'clojure.core/inc
     inc-type

     'clojure.core/+
     add-type

     'clojure.lang.Numbers/add
     add-type

     'clojure.core/if
     {:type   :scheme
      :s-vars [{:sym 'a}]
      :body   {:type   :=>
               :input  {:type     :cat
                        :children [{:type 'boolean?}
                                   {:type :s-var :sym 'a}
                                   {:type :s-var :sym 'a}]}
               :output {:type :s-var :sym 'a}}}

     'clojure.core/map
     {:type   :scheme
      :s-vars [{:sym 'a} {:sym 'b}]
      :body   {:type   :=>
               :input  {:type     :cat
                        :children [{:type   :=>
                                    :input  {:type     :cat
                                             :children [{:type :s-var :sym 'a}]}
                                    :output {:type :s-var :sym 'b}}
                                   {:type  :vector
                                    :child {:type :s-var :sym 'a}}]}
               :output {:type  :vector
                        :child {:type :s-var :sym 'b}}}}}))

(deftest algo-w-const-test
  (is (= (algo-w (ana/analyze :a) test-env)
         {::a/subs   {}
          ::a/schema {:type 'keyword?}})))

(deftest algo-w-do-test
  (is (= (algo-w (ana/analyze '(do (println "!") 1)) test-env)
         {::a/subs   {}
          ::a/schema {:type 'int?}})))

(deftest algo-w-fn-test
  (testing "inc test"
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [x] (inc x))) test-env)]
      (is (nil? failure))
      (is (= {:type :=>
              :input {:type :cat
                      :children [{:type :s-var, ;:sym 's-148872,
                                  :typeclasses #{:number}}]}
              :output {:type :s-var, ;:sym 's-148872, ;; removed this, since will be a different s-var each time (also above)
                       :typeclasses #{:number}}}
             (update
              (update-in schema [:input :children 0] dissoc :sym)
              :output dissoc :sym)))
      (is (symbol? (-> schema :input :children first :sym)))
      (is (symbol? (-> schema :output :sym)))
      ;; make sure input and output :sym are the same s-var
      (is (= (-> schema :input :children first :sym) (-> schema :output :sym))) 
      (is (= (count subs) 4)))
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [] (inc 1))) test-env)]
      (is (nil? failure))
      (is (= schema {:type :=>
                     :input {:type :cat
                             :children []}
                     :output {:type 'int? :typeclasses #{:number}}}))
      (is (= (count subs) 2)))
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [] (inc 1.5))) test-env)]
      (is (nil? failure))
      (is (= schema {:type :=>
                     :input {:type :cat
                             :children []}
                     :output {:type 'double? :typeclasses #{:number}}}))
      (is (= (count subs) 2)))
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [] (inc "hi there"))) test-env)]
      (is (= {:unification-failure {:mgu-failure         :typeclass-mismatch
                                    :schema              {:type 'string?}
                                    :missing-typeclasses #{:number}
                                    :s-var               {:type :s-var :typeclasses #{:number}}}}
             (update-in failure [:unification-failure :s-var] dissoc :sym))) ;; Remove :sym since it's randomly generated
      (is (nil? schema))
      (is (nil? subs)))
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze `(fn [x#] (f (inc x#) 1)))
                  (assoc test-env
                         `f {:type   :scheme
                             :s-vars [{:sym 'a}]
                             :body   {:type   :=>
                                      :input  {:type     :cat
                                               :children [{:type :s-var :sym 'a}
                                                          {:type :s-var :sym 'a}]}
                                      :output {:type :s-var :sym 'a}}}))]
      (is (nil? failure))
      (is (= schema {:type   :=>
                     :input  {:type     :cat
                              :children [{:type 'int? :typeclasses #{:number}}]}
                     :output {:type 'int? :typeclasses #{:number}}}))
      (is (= (count subs) 8))))
  (testing "add test"
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [x y] (+ x y))) test-env)]
      (is (nil? failure))
      (is (= {:type :=>
              :input {:type :cat
                      :children [{:type :s-var, ;:sym 's-148872, 
                                  :typeclasses #{:number}}
                                 {:type :s-var, ;:sym 's-148872, 
                                  :typeclasses #{:number}}]}
              :output {:type :s-var, ;:sym 's-148872, 
                       :typeclasses #{:number}}}
             (-> schema ;; remove sym, since they are randomly generated
                 (update-in [:input :children 0] dissoc :sym)
                 (update-in [:input :children 1] dissoc :sym)
                 (update :output dissoc :sym))))
      (is (symbol? (-> schema :input :children first :sym)))
      (is (symbol? (-> schema :input :children second :sym)))
      (is (symbol? (-> schema :output :sym)))
      ;; make sure input and output :sym are the same s-var
      (is (= (-> schema :input :children first :sym)
             (-> schema :input :children second :sym)
             (-> schema :output :sym)))
      (is (= (count subs) 6)))
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [] (+ 1 2))) test-env)]
      (is (nil? failure))
      (is (= schema {:type :=>
                     :input {:type :cat
                             :children []}
                     :output {:type 'int?}}))
      (is (= (count subs) 2)))
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [] (+ 1.5 2.73))) test-env)]
      (is (nil? failure))
      (is (= schema {:type :=>
                     :input {:type :cat
                             :children []}
                     :output {:type 'double?}}))
      (is (= (count subs) 2)))
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze '(fn [x] (+ 1.5 x))) test-env)]
      (is (nil? failure))
      (is (= schema {:type :=>
                     :input {:type :cat
                             :children [{:type 'double? :typeclasses #{:number}}]}
                     :output {:type 'double? :typeclasses #{:number}}}))
      (is (= (count subs) 3))))
  (testing "nullary"
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze `((fn [] 1))) {})]
      (is (nil? failure))
      (is (= schema {:type 'int?}))
      (is (= (count subs) 1))))
  (testing "polymorphic"
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze `(fn [x# y#] (f x# y#)))
                  (assoc test-env
                         `f {:type   :scheme
                             :s-vars [{:sym 'a} {:sym 'b}]
                             :body   {:type   :=>
                                      :input  {:type     :cat
                                               :children [{:type :s-var :sym 'a}
                                                          {:type :s-var :sym 'b}]}
                                      :output {:type :s-var :sym 'b}}}))
          inputs (set (get-in schema [:input :children]))
          output (:output schema)]
      (is (nil? failure))
      ;; @todo Find better way to test. Meander?
      (is (= (:type schema) :=>))
      (is (= (count inputs) 2))
      (is (contains? inputs output))
      (is (every? #(= (:type %) :s-var) (cons output inputs)))
      (is (= 6 (count subs))))))

(deftest algo-w-if-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(if true 1 2)) test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 2)))
  (testing "failure"
    (let [{::a/keys [subs schema failure]}
          (algo-w (ana/analyze `(if true 1 "2")) test-env)]
      (is (= failure
             {:unification-failure {:mgu-failure :non-equal
                                    :schema-1    {:type 'int?}
                                    :schema-2    {:type 'string?}}}))
      (is (nil? schema))
      (is (nil? subs)))))

(deftest algo-w-import-test
  (is (= (algo-w (ana/analyze `(import 'clojure.lang.Keyword)) test-env)
         {::a/subs {} ::a/schema nil?})))

(deftest algo-w-instance?-test
  (is (= (algo-w (ana/analyze `(instance? String "")) test-env)
         {::a/subs {} ::a/schema 'boolean?})))

(deftest algo-w-invoke-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(map inc [0])) test-env)]
    (is (nil? failure))
    (is (= {:type :vector :child {:type 'int? :typeclasses #{:number}}} schema))
    (is (= 6 (count subs)))))

(deftest algo-w-let-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(let [f# inc
                                    a# 1]
                                (f# a#)))
                test-env)]
    (is (nil? failure))
    (is (= {:type 'int? :typeclasses #{:number}} schema))
    (is (= (count subs) 2))))

(deftest algo-w-letfn-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(letfn [(f# [x#] (inc x#))
                                      (g# [y#] (f# (f# y#)))]
                                (g# 0)))
                test-env)]
    (is (nil? failure))
    (is (= {:type 'int? :typeclasses #{:number}} schema))
    (is (= (count subs) 14)))
  ; @todo (testing "ahead-of-definition")
  )

;; Implicitly tested
;(deftest algo-w-local-test)

(deftest algo-w-prim-invoke-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze '((fn [^long x] x) 1)) test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 2))))

(defprotocol P
  (foo [_ x]))

(defrecord R [y]
  P
  (foo [_ x] (+ x y)))

(deftest algo-w-protocol-invoke-test
  (let [{::a/keys [subs schema failure] :as r}
        (algo-w (ana/analyze `(foo (->R 1) 2))
                (assoc test-env
                       `->R {:type   :=>
                             :input  {:type     :cat
                                      :children [{:type 'int?}]}
                             :output {:type R}}
                       `foo {:type   :=>
                             :input  {:type     :cat
                                      :children [{:type (:on-interface P)}
                                                 {:type 'int?}]}
                             :output {:type 'int?}}))]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 1))))

(deftest algo-w-quote-test
  (is (= (algo-w (ana/analyze `(quote (+ 1 2))) test-env)
         {::a/subs   {}
          ::a/schema {:type  :sequential
                      :child {:type 'some?}}})))

;(deftest algo-w-set!-test)

(deftest algo-w-static-call-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze '(inc 1)) test-env)]
    (is (nil? failure))
    (is (= {:type 'int? :typeclasses #{:number}} schema))
    (is (= (count subs) 2))))

(deftest algo-w-static-field-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze `System/out) test-env)]
    (is (nil? failure))
    (is (= schema {:type PrintStream}))
    (is (= (count subs) 0))))

(deftest algo-w-the-var-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze `(var +)) test-env)]
    (is (nil? failure))
    (is (= schema {:type 'var?}))
    (is (= (count subs) 0))))

;(deftest algo-w-throw-test)
;(deftest algo-w-try-test)

(deftest algo-w-var-test
  (let [{::a/keys [subs schema failure]} (algo-w (ana/analyze `clojure.core/inc) test-env)]
    (is (nil? failure))
    (is (= :=> (:type schema)))
    (is (= 1 (count (-> schema :input :children))))
    (is (= #{:number} (-> schema :input :children first :typeclasses)))
    (is (= :cat (-> schema :input :type)))
    (is (= #{:number} (-> schema :output :typeclasses)))
    (is (symbol? (-> schema :output :sym)))
    (is (= (count subs) 0))))

(deftest infer-schema-typeclass-tests
  (testing "api/infer-schema tests for typeclasses"
    (let [id-num-schema {:type   :scheme
                         :s-vars [{:sym 'a :typeclasses #{:number}}]
                         :body   {:type   :=>
                                  :input  {:type     :cat
                                           :children [{:type :s-var :sym 'a}]}
                                  :output {:type :s-var :sym 'a}}}]
      (testing "Application - Valid: id-num with int"
        (let [ast {:op :APP :fn {:op :VAR :sym 'id-num} :args [{:op :LIT :type :int :val 1}]}
              env {'id-num id-num-schema}
              result-schema (schema-inf/infer-schema ast env)]
          (is (= {:type 'int? :typeclasses #{:number}} result-schema))))

      (testing "Application - Invalid: id-num with string"
        (let [ast {:op :APP :fn {:op :VAR :sym 'id-num} :args [{:op :LIT :type :string :val "foo"}]}
              env {'id-num id-num-schema}]
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"Schema inference failure."
               (schema-inf/infer-schema ast env)))
          (try (schema-inf/infer-schema ast env)
               (catch clojure.lang.ExceptionInfo e
                 (let [ex-data (ex-data e)
                       unif-failure (get-in ex-data [::a/failure :unification-failure])]
                   (is (= :typeclass-mismatch (:mgu-failure unif-failure)))
                   ;; s-var 'a from id-num-schema might get a new gensym'd name after instantiation
                   (is (= #{:number} (:typeclasses (:s-var unif-failure))))
                   (is (= {:type 'string?} (:schema unif-failure)))
                   (is (= #{:number} (:missing-typeclasses unif-failure))))))))

    (testing "Let binding, generalization, and application with pre-defined scheme"
      (let [id-num-comparable-schema {:type   :scheme
                                      :s-vars [{:sym 'a :typeclasses #{:number :comparable}}]
                                      :body   {:type   :=>
                                               :input  {:type     :cat
                                                        :children [{:type :s-var :sym 'a}]}
                                               :output {:type :s-var :sym 'a}}}
            ;; AST for (let [f id-num-comparable] f)
            let-f-ast {:op :LET
                       :bindings [{:name 'f :init {:op :VAR :sym 'id-nc}}]
                       :body {:op :VAR :sym 'f}}
            env {'id-nc id-num-comparable-schema}
            inferred-f-type (schema-inf/infer-schema let-f-ast env)]

        (is (= :=> (:type inferred-f-type)))
        ;; for input
        (let [typeclasses (-> inferred-f-type :input :children first :typeclasses)]
          ;; The symbol might be different due to generalization, check only typeclasses
          (is (= #{:number :comparable} typeclasses)))
        (is (= :cat (-> inferred-f-type :input :type)))
        (is (= :s-var (-> inferred-f-type :input :children first :type)))
        ;; for output
        (let [typeclasses (-> inferred-f-type :output :typeclasses)]
          ;; The symbol might be different due to generalization, check only typeclasses
          (is (= #{:number :comparable} typeclasses)))

        ;; Test application of f (which is id-num-comparable)
        (let [app-env (assoc {} 'f inferred-f-type)
              app-int-ast {:op :APP :fn {:op :VAR :sym 'f} :args [{:op :LIT :type :int :val 1}]}
              app-double-ast {:op :APP :fn {:op :VAR :sym 'f} :args [{:op :LIT :type :double :val 1.5}]}
              app-bool-ast {:op :APP :fn {:op :VAR :sym 'f} :args [{:op :LIT :type :boolean :val true}]}
              app-vec-ast {:op :APP :fn {:op :VAR :sym 'f} :args [{:op :LIT :type :vector :val []}]}]

          (is (= {:type 'int? :typeclasses #{:comparable :number}} 
                 (schema-inf/infer-schema app-int-ast app-env)))
          (is (= {:type 'double? :typeclasses #{:comparable :number}} 
                 (schema-inf/infer-schema app-double-ast app-env)))

          ;; This should throw exception because booleans aren't in :number typeclass
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"Schema inference failure."
               (schema-inf/infer-schema app-bool-ast app-env)))

          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"Schema inference failure."
               (schema-inf/infer-schema app-vec-ast app-env)))
          (try (schema-inf/infer-schema app-vec-ast app-env)
               (catch clojure.lang.ExceptionInfo e
                 (let [ex-data (ex-data e)
                       unif-failure (get-in ex-data [::a/failure :unification-failure])]
                   (is (= :typeclass-mismatch (:mgu-failure unif-failure)))
                   (is (= #{:number :comparable} (:typeclasses (:s-var unif-failure))))
                   (is (= {:type :vector} (select-keys (:schema unif-failure) [:type])))))))))

    (testing "Error reporting for typeclass mismatch in infer-schema"
      (let [ast {:op :APP
                  :fn {:op :VAR :sym 'id-num}
                  :args [{:op :LIT :type :string :val "foo"}]}
            env {'id-num {:type   :scheme
                          :s-vars [{:sym 'a :typeclasses #{:number}}]
                          :body   {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type :s-var :sym 'a}]}
                                   :output {:type :s-var :sym 'a}}}}]
        (try
          (schema-inf/infer-schema ast env)
          (is false "Should have thrown an exception") ; Should not reach here
          (catch clojure.lang.ExceptionInfo e
            (let [ex-data (ex-data e)
                  failure-data (::a/failure ex-data)
                  unification-failure-data (:unification-failure failure-data)]
              (is (= :typeclass-mismatch (:mgu-failure unification-failure-data)))
              ;; The symbol 'a might be gensym'd, so we check typeclasses and type only for s-var
              (is (= {:type :s-var, :typeclasses #{:number}}
                     (select-keys (:s-var unification-failure-data) [:type :typeclasses])))
              (is (= {:type 'string?} (:schema unification-failure-data)))
              (is (= #{:number} (:missing-typeclasses unification-failure-data)))))))))))