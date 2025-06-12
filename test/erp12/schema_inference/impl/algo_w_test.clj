(ns erp12.schema-inference.impl.algo_w-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.tools.analyzer.jvm :as ana]
            [erp12.schema-inference.impl.algo_w :refer [algo-w] :as a]
            [erp12.schema-inference.impl.util :as u] ; For mgu tests
            [erp12.schema-inference.impl.typeclasses :as tc] ; For typeclasses map
            [erp12.schema-inference.api :as schema-inf]) ; For api/infer-schema tests
  (:import (java.io PrintStream)))

;; Keep some empty vars to be used arbitrarily in test ASTs.
(declare f)

;; @todo Test type checker failures

(def test-env
  {'clojure.lang.Numbers/inc
   {:type   :=>
    :input  {:type     :cat
             :children [{:type 'int?}]}
    :output {:type 'int?}}

   'clojure.core/inc
   {:type   :=>
    :input  {:type     :cat
             :children [{:type 'int?}]}
    :output {:type 'int?}}

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
                      :child {:type :s-var :sym 'b}}}}})

(deftest algo-w-const-test
  (is (= (algo-w (ana/analyze :a) test-env)
         {::a/subs   {}
          ::a/schema {:type 'keyword?}})))

(deftest algo-w-do-test
  (is (= (algo-w (ana/analyze '(do (println "!") 1)) test-env)
         {::a/subs   {}
          ::a/schema {:type 'int?}})))

(deftest algo-w-fn-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze '(fn [x] (inc x))) test-env)]
    (is (nil? failure))
    (is (= schema {:type   :=>
                   :input  {:type     :cat
                            :children [{:type 'int?}]}
                   :output {:type 'int?}}))
    (is (= (count subs) 2)))
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(fn [x#] (f (inc x#) 1)))
                (assoc test-env
                  `f {:type   :scheme
                      :s-vars [{:sym 'a}]
                      :body   {:type   :=>
                               :input  {:type     :cat
                                        :children [{:type :s-var :sym 'a}
                                                   {:type :s-var :sym 'a}]}
                               :output {:type :s-var :sym 'a}}})]
    (is (nil? failure))
    (is (= schema {:type   :=>
                   :input  {:type     :cat
                            :children [{:type 'int?}]}
                   :output {:type 'int?}}))
    (is (= (count subs) 4)))
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
      (is (= (count subs) 3)))))

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
    (is (= schema {:type :vector :child {:type 'int?}}))
    (is (= (count subs) 3))))

(deftest algo-w-let-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(let [f# inc
                                    a# 1]
                                (f# a#)))
                test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 1))))

(deftest algo-w-letfn-test
  (let [{::a/keys [subs schema failure]}
        (algo-w (ana/analyze `(letfn [(f# [x#] (inc x#))
                                      (g# [y#] (f# (f# y#)))]
                                (g# 0)))
                test-env)]
    (is (nil? failure))
    (is (= schema {:type 'int?}))
    (is (= (count subs) 6)))
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
                               :output {:type 'int?}}})]
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
    (is (= schema {:type 'int?}))
    (is (= (count subs) 1))))

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
    (is (= schema {:type   :=>
                   :input  {:type     :cat
                            :children [{:type 'int?}]}
                   :output {:type 'int?}}))
    (is (= (count subs) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typeclass Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest mgu-typeclass-tests
  (testing "Direct mgu tests for typeclasses"
    (let [s-var-num {:type :s-var :sym 'a :typeclasses [:number]}
          s-var-num-comp {:type :s-var :sym 'b :typeclasses [:number :comparable]}
          s-var-comparable {:type :s-var :sym 'c :typeclasses [:comparable]}
          s-var-any {:type :s-var :sym 'd}
          int-schema {:type 'int?}
          string-schema {:type 'string?}
          bool-schema {:type 'boolean?}]

      (testing "s-var with :typeclasses [:number] vs 'int?"
        (is (map? (u/mgu s-var-num int-schema)))
        (is (not (u/mgu-failure? (u/mgu s-var-num int-schema)))))

      (testing "s-var with :typeclasses [:number] vs 'string?"
        (let [result (u/mgu s-var-num string-schema)]
          (is (u/mgu-failure? result))
          (is (= (:mgu-failure result) :typeclass-mismatch))
          (is (= (:s-var result) s-var-num))
          (is (= (:schema result) string-schema))
          (is (= (:missing-typeclasses result) [:number]))))

      (testing "s-var a [:number] with s-var b [:number :comparable]"
        ;; b's constraints are a superset of a's constraints.
        ;; unifying a with b means 'a can be 'b.
        ;; The substitution should bind 'a to 'b, or 'b to 'a.
        ;; If 'a -> 'b, then 'a takes on 'b's constraints.
        ;; If 'b -> 'a, then 'b takes on 'a's constraints, which is a failure if b's constraints are stricter.
        ;; bind-var s-var schema: if s-var has TCs, schema must satisfy.
        ;; mgu s-var-num s-var-num-comp:
        ;;   bind-var s-var-num s-var-num-comp:
        ;;     s-var-num-comp satisfies [:number]? Yes, because (:typeclasses s-var-num-comp) includes :number.
        ;;     => { 'a  s-var-num-comp } - This is one possibility.
        ;; mgu s-var-num-comp s-var-num:
        ;;   bind-var s-var-num-comp s-var-num:
        ;;     s-var-num satisfies [:number :comparable]? No, s-var-num only has :number. Fails.
        ;; So, the order matters for which var gets bound.
        ;; mgu itself tries both orderings via dispatch [:s-var :_] and [:_ :s-var]
        ;; Let's assume s-var-num is 'a and s-var-num-comp is 'b
        ;; u/mgu a b -> bind-var a b -> {a b} is possible if b satisfies a's constraints.
        ;; u/mgu b a -> bind-var b a -> {b a} is possible if a satisfies b's constraints.
        (let [res1 (u/mgu s-var-num s-var-num-comp)]
          (is (not (u/mgu-failure? res1)))
          ;; expecting 'a -> s-var-num-comp OR 'b -> s-var-num.
          ;; If 'a -> s-var-num-comp, 'a effectively gets constraints [:number :comparable]
          ;; If 'b -> s-var-num, 'b effectively gets constraints [:number] - this would be from (mgu s_b s_a) -> (bind-var s_b s_a)
          ;; Let's trace bind-var: (bind-var S T)
          ;; (bind-var s-var-num s-var-num-comp): s-var-num-comp satisfies [:number] from s-var-num? Yes. -> {'a s-var-num-comp}
          (is (= res1 {'a s-var-num-comp}))
          )

        (let [res2 (u/mgu s-var-num-comp s-var-num)]
           ;; (bind-var s-var-num-comp s-var-num): s-var-num satisfies [:number :comparable] from s-var-num-comp? No.
          (is (u/mgu-failure? res2))
          (is (= (:mgu-failure res2) :typeclass-mismatch))
          (is (= (:missing-typeclasses res2) [:comparable]))))

      (testing "s-var a [:number :comparable] with s-var b [:number]"
        (let [result (u/mgu s-var-num-comp s-var-num)]
          (is (u/mgu-failure? result))
          (is (= (:mgu-failure result) :typeclass-mismatch))
          (is (= (:s-var result) s-var-num-comp))
          (is (= (:schema result) s-var-num))
          (is (= (:missing-typeclasses result) [:comparable]))))

      (testing "s-var a [:number] with s-var b [] (no typeclasses)"
        ;; s-var-any satisfies [:number]? Yes, because it's unconstrained.
        (let [result1 (u/mgu s-var-num s-var-any)]
          (is (not (u/mgu-failure? result1)))
          (is (= result1 {'a s-var-any})))

        ;; s-var-num satisfies []? Yes.
        (let [result2 (u/mgu s-var-any s-var-num)]
          (is (not (u/mgu-failure? result2)))
          (is (= result2 {'d s-var-num})))))

    (testing "Unifying s-var with typeclass against a concrete type not in typeclass"
      (let [s-var-counted {:type :s-var :sym 'a :typeclasses [:counted]}
            int-schema {:type 'int?}] ; int? is not in :counted
        (let [result (u/mgu s-var-counted int-schema)]
          (is (u/mgu-failure? result))
          (is (= :typeclass-mismatch (:mgu-failure result)))
          (is (= [:counted] (:missing-typeclasses result))))))

    (testing "Unifying s-var with multiple typeclasses against a concrete type satisfying only one"
      (let [s-var-num-comparable {:type :s-var :sym 'a :typeclasses [:number :comparable]}
            int-schema {:type 'int?}] ; int? is :number and :comparable
        (is (not (u/mgu-failure? (u/mgu s-var-num-comparable int-schema)))))

      ;; This test is tricky due to how satisfies-all-typeclasses? works for concrete types.
      ;; It checks if the concrete type (string?) satisfies *each* tc in s-var's list.
      ;; string? is :comparable, but not :number. So it fails for :number.
      (let [s-var-num-comparable {:type :s-var :sym 'a :typeclasses [:number :comparable]}
            string-schema {:type 'string?}] ; string? is :comparable but not :number
        (let [result (u/mgu s-var-num-comparable string-schema)]
          (is (u/mgu-failure? result))
          (is (= :typeclass-mismatch (:mgu-failure result)))
          (is (= [:number] (:missing-typeclasses result))))))

    (testing "s-var unification where target s-var's constraints are stricter (subset)"
      ;; s-var 'a' requires [:number]
      ;; s-var 'b' has constraints [:number :comparable] (stricter/more specific)
      ;; (mgu 'a 'b) -> bind 'a to 'b is okay because 'b satisfies [:number]
      (let [sva {:type :s-var :sym 'a :typeclasses [:number]}
            svb {:type :s-var :sym 'b :typeclasses [:number :comparable]}]
        (is (= (u/mgu sva svb) {'a svb})))

      ;; s-var 'a' requires [:number :comparable]
      ;; s-var 'b' has constraints [:number] (less specific)
      ;; (mgu 'a 'b) -> bind 'a to 'b: does 'b satisfy [:number :comparable]? No. Fails.
      (let [sva {:type :s-var :sym 'a :typeclasses [:number :comparable]}
            svb {:type :s-var :sym 'b :typeclasses [:number]}]
        (let [result (u/mgu sva svb)]
          (is (u/mgu-failure? result))
          (is (= :typeclass-mismatch (:mgu-failure result)))
          (is (= (:missing-typeclasses result) [:comparable])))))
  ))

(deftest infer-schema-typeclass-tests
  (testing "api/infer-schema tests for typeclasses"
    (let [id-num-schema {:type   :scheme
                         :s-vars [{:sym 'a :typeclasses [:number]}]
                         :body   {:type   :=>
                                  :input  {:type     :cat
                                           :children [{:type :s-var :sym 'a}]}
                                  :output {:type :s-var :sym 'a}}}]
      (testing "Application - Valid: id-num with int"
        (let [ast {:op :APP :fn {:op :VAR :sym 'id-num} :args [{:op :LIT :type :int :val 1}]}
              env {'id-num id-num-schema}
              result-schema (schema-inf/infer-schema ast env)]
          (is (= {:type 'int?} result-schema))))

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
                   (is (= [:number] (:typeclasses (:s-var unif-failure))))
                   (is (= {:type 'string?} (:schema unif-failure)))
                   (is (= [:number] (:missing-typeclasses unif-failure))))))))

    (testing "Let binding, generalization, and application with pre-defined scheme"
      (let [id-num-comparable-schema {:type   :scheme
                                      :s-vars [{:sym 'a :typeclasses [:number :comparable]}]
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

        (is (= :scheme (:type inferred-f-type)))
        (is (= 1 (count (:s-vars inferred-f-type))))
        (let [s-var-in-f (first (:s-vars inferred-f-type))]
          ;; The symbol might be different due to generalization, check only typeclasses
          (is (= (set [:number :comparable]) (set (:typeclasses s-var-in-f)))))

        ;; Test application of f (which is id-num-comparable)
        (let [app-env (assoc {} 'f inferred-f-type)
              app-int-ast {:op :APP :fn {:op :VAR :sym 'f} :args [{:op :LIT :type :int :val 1}]}
              app-bool-ast {:op :APP :fn {:op :VAR :sym 'f} :args [{:op :LIT :type :boolean :val true}]}
              app-vec-ast {:op :APP :fn {:op :VAR :sym 'f} :args [{:op :LIT :type :vector :val []}]}]

          (is (= {:type 'int?} (schema-inf/infer-schema app-int-ast app-env)))
          (is (= {:type 'boolean?} (schema-inf/infer-schema app-bool-ast app-env)))

          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"Schema inference failure."
               (schema-inf/infer-schema app-vec-ast app-env)))
          (try (schema-inf/infer-schema app-vec-ast app-env)
               (catch clojure.lang.ExceptionInfo e
                 (let [ex-data (ex-data e)
                       unif-failure (get-in ex-data [::a/failure :unification-failure])]
                   (is (= :typeclass-mismatch (:mgu-failure unif-failure)))
                   (is (= (set [:number :comparable]) (set (:typeclasses (:s-var unif-failure)))))
                   (is (= {:type :vector} (select-keys (:schema unif-failure) [:type])))))))))

    (testing "Error reporting for typeclass mismatch in infer-schema"
      (let [ast {:op :APP
                  :fn {:op :VAR :sym 'id-num}
                  :args [{:op :LIT :type :string :val "foo"}]}
            env {'id-num {:type   :scheme
                          :s-vars [{:sym 'a :typeclasses [:number]}]
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
              (is (= {:type :s-var, :typeclasses [:number]}
                     (select-keys (:s-var unification-failure-data) [:type :typeclasses])))
              (is (= {:type 'string?} (:schema unification-failure-data)))
              (is (= [:number] (:missing-typeclasses unification-failure-data)))))))))
))