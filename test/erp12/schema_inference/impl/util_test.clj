(ns erp12.schema-inference.impl.util-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [erp12.schema-inference.impl.util :as u]
            [clojure.set :as set]))

(deftest ground?-test
  (is (u/ground? {:type 'string?}))
  (is (not (u/ground? {:type  :vector
                       :child {:type 'int?}})))
  (is (not (u/ground? {:type   :=>
                       :input  {:type     :cat
                                :children [{:type 'int?}]}
                       :output {:type 'float?}})))
  (is (not (u/ground? {:type   :scheme
                       :s-vars [{:sym 'x}]
                       :body   {:type   :=>
                                :input  {:type     :cat
                                         :children [{:type 'int?}]}
                                :output {:type 'float?}}}))))

(deftest substitute-test
  (testing "substitute"
    (let [x->y #(u/substitute {'x {:type :s-var :sym 'y}} %)]
      (is (= {:type :s-var :sym 'y}
             (x->y {:type :s-var :sym 'x})))
      (is (= {:type :s-var :sym 'z}
             (x->y {:type :s-var :sym 'z})))
      (is (= {:type :s-var :sym 'y :typeclasses #{:number}}
             (x->y {:type :s-var :sym 'x :typeclasses #{:number}})))
      (is (= {:type :s-var :sym 'y :typeclasses #{:number :comparable}}
             (u/substitute {'x {:type :s-var :sym 'y :typeclasses #{:comparable}}}
                           {:type :s-var :sym 'x :typeclasses #{:number}})))
      (is (= {:type :s-var :sym 'y :typeclasses #{:number}}
             (u/substitute {'x {:type :s-var :sym 'y}}
                           {:type :s-var :sym 'x :typeclasses #{:number}})))
      (is (= {:type :s-var :sym 'y :typeclasses #{:number :comparable}}
             (u/substitute {'x {:type :s-var :sym 'y}}
                           {:type :s-var :sym 'x :typeclasses #{:number :comparable}})))

      (testing "tuple schema"
        (is (= {:type :tuple :children [{:type :s-var :sym 'y} {:type :s-var :sym 'y}]}
               (x->y {:type :tuple :children [{:type :s-var :sym 'x} {:type :s-var :sym 'x}]}))))
      (testing "function schema"
        (is (= {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'y}]}
                :output {:type :s-var :sym 'y}}
               (x->y {:type   :=>
                      :input  {:type     :cat
                               :children [{:type :s-var :sym 'x}]}
                      :output {:type :s-var :sym 'x}})))
        (is (= {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'y}]}
                :output {:type :s-var :sym 'z}}
               (x->y {:type   :=>
                      :input  {:type     :cat
                               :children [{:type :s-var :sym 'x}]}
                      :output {:type :s-var :sym 'z}})))
        (is (= {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'y} {:type :s-var :sym 't}]}
                :output {:type :s-var :sym 'y}}
               (u/substitute {'x {:type :s-var :sym 'y}
                              'w {:type :s-var :sym 'y}}
                             {:type   :=>
                              :input  {:type     :cat
                                       :children [{:type :s-var :sym 'x} {:type :s-var :sym 't}]}
                              :output {:type :s-var :sym 'w}})))
        (is (= {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'y} {:type :s-var :sym 't}]}
                :output {:type :s-var :sym 'u}}
               (u/substitute {'x {:type :s-var :sym 'y}
                              'w {:type :s-var :sym 'u}}
                             {:type   :=>
                              :input  {:type     :cat
                                       :children [{:type :s-var :sym 'x} {:type :s-var :sym 't}]}
                              :output {:type :s-var :sym 'w}}))))
      (testing "scheme"
        (is (= {:type   :scheme
                :s-vars [{:sym 'z}]
                :body   {:type :s-var :sym 'y}}
               (x->y {:type   :scheme
                      :s-vars [{:sym 'z}]
                      :body   {:type :s-var :sym 'x}})))
        ;; Occurs check - shouldn't be substituted
        (is (= {:type   :scheme
                :s-vars [{:sym 'x}]
                :body   {:type :s-var :sym 'x}}
               (x->y {:type   :scheme
                      :s-vars [{:sym 'x}]
                      :body   {:type :s-var :sym 'x}}))))
      (testing "typeclasses"
        (is (= {:type   :scheme
                :s-vars [{:sym 'z :typeclasses #{:number}}]
                :body   {:type :s-var :sym 'y}}
               (x->y {:type   :scheme
                      :s-vars [{:sym 'z :typeclasses #{:number}}]
                      :body   {:type :s-var :sym 'x}})))
        ;; Occurs check - shouldn't be substituted
        (is (= {:type   :scheme
                :s-vars [{:sym 'x :typeclasses #{:number}}]
                :body   {:type :s-var :sym 'x}}
               (x->y {:type   :scheme
                      :s-vars [{:sym 'x :typeclasses #{:number}}]
                      :body   {:type :s-var :sym 'x}})))))))

(substitute-test)

(deftest substitute-env-test
  (is {'a {:type   :scheme
           :s-vars [{:sym 'z}]
           :body   {:type  :vector
                    :child {:type :s-var :sym 'y}}}
       'b {:type   :scheme
           :s-vars [{:sym 'x}]
           :body   {:type  :set
                    :child {:type :s-var :sym 'x}}}}
      (u/substitute-env {'x {:type :s-var :sym 'y}}
                        {'a {:type   :scheme
                             :s-vars [{:sym 'z}]
                             :body   {:type  :vector
                                      :child {:type :s-var :sym 'x}}}
                         'b {:type   :scheme
                             :s-vars [{:sym 'x}]
                             :body   {:type  :set
                                      :child {:type :s-var :sym 'x}}}})))

(deftest compose-substitutions-test
  (is (= (u/compose-substitutions {} {})
         {}))
  (is (= (u/compose-substitutions {'a {:type :s-var, :sym 'b}}
                                  {'b {:type 'boolean?}})
         {'a {:sym 'b :type :s-var}
          'b {:type 'boolean?}}))
  (is (= (u/compose-substitutions {'x {:type 'string?}
                                   'y {:type 'int?}}
                                  {'y {:type :s-var :sym 'x}})
         {'x {:type 'string?}
          'y {:type 'string?}})))

(deftest free-type-vars-test
  (is (= #{'x} (u/free-type-vars {:type :s-var :sym 'x})))
  (is (= #{} (u/free-type-vars {:type 'string?})))
  (testing "function schemas"
    (is (= #{'x 'y} (u/free-type-vars {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}]}
                                       :output {:type :s-var :sym 'y}})))
    (is (= #{'x 'y} (u/free-type-vars {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}
                                                           {:type :s-var :sym 'y}]}
                                       :output {:type :s-var :sym 'x}}))))
  (is (= #{}
         (u/free-type-vars {:type :map-of :key {:type 'int?} :value {:type 'string?}})))
  (testing "scheme"
    (is (= #{'y}
           (u/free-type-vars {:type   :scheme
                              :s-vars [{:sym 'x}]
                              :body   {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}]}
                                       :output {:type :s-var :sym 'y}}})))
    (is (= #{}
           (u/free-type-vars {:type   :scheme
                              :s-vars [{:sym 'x} {:sym 'y}]
                              :body   {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'x}]}
                                       :output {:type :s-var :sym 'y}}})))))

(deftest free-type-vars-env-test
  (is (= (u/free-type-vars-env {'a {:type   :scheme
                                    :s-vars [{:sym 'z}]
                                    :body   {:type  :vector
                                             :child {:type :s-var :sym 'x}}}
                                'b {:type   :scheme
                                    :s-vars [{:sym 'x}]
                                    :body   {:type  :set
                                             :child {:type :s-var :sym 'x}}}})
         #{'x})))

(deftest instantiate-test
  (is (= (u/instantiate {:type 'int?})
         {:type 'int?}))
  (is (= (u/instantiate {:type :s-var :sym 'x})
         {:type :s-var :sym 'x}))
  (let [s (u/instantiate {:type   :scheme
                          :s-vars [{:sym 'x}]
                          :body   {:type  :vector
                                   :child {:type :s-var :sym 'x}}})]
    (is (= (:type s) :vector))
    (is (= (get-in s [:child :type]) :s-var))
    (is (str/starts-with? (name (get-in s [:child :sym])) "s-")))
  (testing "scheme with typeclasses"
    (let [scheme {:type   :scheme
                  :s-vars [{:sym 'x :typeclasses #{:number}} {:sym 'y :typeclasses #{:comparable}}]
                  :body   {:type     :tuple
                           :children [{:type :s-var :sym 'x} {:type :s-var :sym 'y}]}}
          instantiated (u/instantiate scheme)
          children (:children instantiated)
          s-var1 (first children)
          s-var2 (second children)]
      (is (= (:type instantiated) :tuple))
      (is (= (count children) 2))
      (is (= (:type s-var1) :s-var))
      (is (not= (:sym s-var1) 'x))             ; Fresh symbol
      (is (= (:typeclasses s-var1) #{:number}))
      (is (= (:type s-var2) :s-var))
      (is (not= (:sym s-var2) 'y))             ; Fresh symbol
      (is (= (:typeclasses s-var2) #{:comparable}))))
  (testing "scheme with s-var having no typeclasses"
    (let [scheme {:type   :scheme
                  :s-vars [{:sym 'z'}] ; No typeclasses
                  :body   {:type :s-var :sym 'z'}}
          instantiated (u/instantiate scheme)
          fresh-s-var-typeclasses (:typeclasses instantiated)]
      (is (= (:type instantiated) :s-var))
      (is (not= (:sym instantiated) 'z'))
      (is (or (nil? fresh-s-var-typeclasses) (empty? fresh-s-var-typeclasses))))))

(deftest generalize-test
  (let [env {'a {:type 'int?}
             'b {:type :s-var :sym 'x}}]
    (is (= {:type 'int?}
           (u/generalize env {:type 'int?})))
    (is (= {:type :s-var :sym 'x}
           (u/generalize env {:type :s-var :sym 'x})))
    (is (= {:type   :scheme
            :s-vars [{:sym 'y}]
            :body   {:type  :vector
                     :child {:type :s-var :sym 'y}}}
           (u/generalize env
                         {:type  :vector
                          :child {:type :s-var :sym 'y}}))))
  (testing "generalize schema with free s-vars having typeclasses"
    (let [env {'a {:type 'int?}} ; 'y and 'z are free in schema, not in env
          schema-to-generalize {:type     :tuple
                                :children [{:type :s-var :sym 'y :typeclasses #{:number}}
                                           {:type :s-var :sym 'z :typeclasses #{:comparable}}]}
          generalized (u/generalize env schema-to-generalize)
          s-vars-map (into {} (map (juxt :sym identity) (:s-vars generalized)))]
      (is (= (:type generalized) :scheme))
      (is (= (count (:s-vars generalized)) 2))
      (is (= (get-in s-vars-map ['y :typeclasses]) #{:number}))
      (is (= (get-in s-vars-map ['z :typeclasses]) #{:comparable}))
      ;; generalize sorts s-vars by sym, so we sort original schema's s-vars for comparison if needed
      (is (= (:body generalized) schema-to-generalize))))
  (testing "generalize schema where some s-vars with typeclasses are also in env"
    (let [env {'y {:type :s-var :sym 'y :typeclasses #{:number}}} ; 'y is in env
          schema-to-generalize {:type     :tuple
                                :children [{:type :s-var :sym 'y :typeclasses #{:number}}
                                           {:type :s-var :sym 'z :typeclasses #{:comparable}}]}
          generalized (u/generalize env schema-to-generalize)
          s-vars-map (into {} (map (juxt :sym identity) (:s-vars generalized)))]
      (is (= (:type generalized) :scheme))
      (is (= (count (:s-vars generalized)) 1))
      (is (nil? (get s-vars-map 'y))) ; 'y should not be generalized
      (is (= (get-in s-vars-map ['z :typeclasses]) #{:comparable}))
      (is (= (:body generalized) schema-to-generalize))))
  (testing "generalize a scheme (should instantiate first)"
    (let [env {}
          scheme-to-generalize {:type   :scheme
                                :s-vars [{:sym 'x :typeclasses #{:number}}]
                                :body   {:type :s-var :sym 'x}}
          generalized (u/generalize env scheme-to-generalize)]
      ;; Since 'x' is bound by the input scheme, after instantiation,
      ;; the new s-var (e.g., s-1) will be free relative to env and generalized.
      (is (= (:type generalized) :scheme))
      (is (= (count (:s-vars generalized)) 1))
      (let [generalized-s-var (first (:s-vars generalized))]
        ;; The s-var in the new scheme should be the fresh var from instantiation
        (is (not= (:sym generalized-s-var) 'x))
        (is (str/starts-with? (name (:sym generalized-s-var)) "s-"))
        (is (= (:typeclasses generalized-s-var) #{:number}))
        ;; The body of the new scheme should be the instantiated body of the input scheme
        (is (= (:type (:body generalized)) :s-var))
        (is (= (:sym (:body generalized)) (:sym generalized-s-var))) ; Body uses the fresh, generalized s-var
        (is (= (:typeclasses (:body generalized)) #{:number}))))))

(deftest mgu-test
  (testing "atomic types"
    (is (= (u/mgu {:type 'int?} {:type 'int?})
           {}))
    (is (= (u/mgu {:type 'int?} {:type 'string?})
           {:mgu-failure :non-equal
            :schema-1    {:type 'int?}
            :schema-2    {:type 'string?}})))

  (testing "s-var with concrete type"
    (is (= (u/mgu {:type 'int?} {:type :s-var :sym 'a})
           {'a {:type 'int?}}))
    (is (= (u/mgu {:type :s-var :sym 'a} {:type 'int?})
           {'a {:type 'int?}})))

  (testing "s-var with concrete type - typeclass compatibility"
    (is (= (u/mgu {:type :s-var :sym 'a :typeclasses #{:number}} {:type 'int?})
           {'a {:type 'int?}}))
    (let [s-var {:type :s-var :sym 'a :typeclasses #{:countable}}
          concrete-schema {:type 'int?} ; int? is not :countable by default test type->typeclasses
          result (u/mgu s-var concrete-schema)]
      (is (u/mgu-failure? result))
      (is (= (:mgu-failure result) :typeclass-mismatch))
      (is (= (:s-var result) s-var))
      (is (= (:schema result) concrete-schema))))

  (testing "s-var with s-var - identical symbols"
    (is (= (u/mgu {:type :s-var :sym 'a} {:type :s-var :sym 'a})
           {}))
    (is (= (u/mgu {:type :s-var :sym 'a :typeclasses #{:number}}
                  {:type :s-var :sym 'a :typeclasses #{:number}})
           {}))
    ;; If typeclasses differ for the same symbol, it's a conflict (or should be handled by specific logic not covered here)
    ;; For now, assuming mgu would signal a problem or one would absorb the other based on some rule.
    ;; Current mgu might bind 'a to 'a with merged TCs, then simplify to {} if TCs are equal.
    ;; If TCs are different, it might result in {'a s-var-with-merged-TCs}, which isn't {}
    ;; This edge case needs clarification based on u/mgu's exact behavior for identical syms with differing TCs.
    ;; For now, let's assume they must be identical if syms are identical for {} result.
    (is (thrown-with-msg?
         java.lang.IllegalArgumentException
         #"Duplicate key: a"
         (u/mgu {:type :s-var :sym 'a :typeclasses #{:number}}
                {:type :s-var :sym 'a :typeclasses #{:string}}))
        "Unifying s-vars with identical symbols but incompatible typeclasses should fail or clarify merging rules."))

  (testing "s-var with s-var - different symbols"
    (testing "no typeclasses"
      (let [sA {:type :s-var :sym 'a}
            sB {:type :s-var :sym 'b}
            result (u/mgu sA sB)]
        (is (= (count result) 2))
        (is (contains? result 'a))
        (is (contains? result 'b))
        (let [new-svar-a (get result 'a)
              new-svar-b (get result 'b)]
          (is (identical? new-svar-a new-svar-b))
          (is (= (:type new-svar-a) :s-var))
          (is (not (or (= (:sym new-svar-a) 'a) (= (:sym new-svar-a) 'b))))
          (is (str/starts-with? (name (:sym new-svar-a)) "s-"))
          (is (or (nil? (:typeclasses new-svar-a)) (empty? (:typeclasses new-svar-a)))))))

    (testing "different typeclasses"
      (let [sA {:type :s-var :sym 'a :typeclasses #{:TCA1 :TCA2}}
            sB {:type :s-var :sym 'b :typeclasses #{:TCB1}}
            result (u/mgu sA sB)]
        (is (= (count result) 2))
        (let [new-svar (get result 'a)]
          (is (identical? new-svar (get result 'b)))
          (is (= (:type new-svar) :s-var))
          (is (not (or (= (:sym new-svar) 'a) (= (:sym new-svar) 'b))))
          (is (str/starts-with? (name (:sym new-svar)) "s-"))
          (is (= (:typeclasses new-svar) (set/union #{:TCA1 :TCA2} #{:TCB1}))))))
    
    (testing "one typeclass superset of other typeclass"
      (let [sA {:type :s-var :sym 'a :typeclasses #{:TCA1 :TCA2 :TCA3}}
            sB {:type :s-var :sym 'b :typeclasses #{:TCA1 :TCA3}}
            result (u/mgu sA sB)]
        (is (= (count result) 2))
        (let [new-svar (get result 'a)]
          (is (identical? new-svar (get result 'b)))
          (is (= (:type new-svar) :s-var))
          (is (not (or (= (:sym new-svar) 'a) (= (:sym new-svar) 'b))))
          (is (str/starts-with? (name (:sym new-svar)) "s-"))
          (is (= (:typeclasses new-svar) #{:TCA1 :TCA2 :TCA3})))))
    
    (testing "one typeclass superset of other typeclass"
      (let [sA {:type :s-var :sym 'a :typeclasses #{:TCA1 :TC-COMMON :TCA3}}
            sB {:type :s-var :sym 'b :typeclasses #{:TC-COMMON :TCB}}
            result (u/mgu sA sB)]
        (is (= (count result) 2))
        (let [new-svar (get result 'a)]
          (is (identical? new-svar (get result 'b)))
          (is (= (:type new-svar) :s-var))
          (is (not (or (= (:sym new-svar) 'a) (= (:sym new-svar) 'b))))
          (is (str/starts-with? (name (:sym new-svar)) "s-"))
          (is (= (:typeclasses new-svar) #{:TCA1 :TC-COMMON :TCA3 :TCB})))))

    (testing "one with typeclasses, one without"
      (let [sA {:type :s-var :sym 'a :typeclasses #{:TCA}}
            sB {:type :s-var :sym 'b} ; no typeclasses
            result (u/mgu sA sB)]
        (is (= (count result) 2))
        (let [new-svar (get result 'a)]
          (is (identical? new-svar (get result 'b)))
          (is (= (:type new-svar) :s-var))
          (is (not (or (= (:sym new-svar) 'a) (= (:sym new-svar) 'b))))
          (is (= (:typeclasses new-svar) #{:TCA}))))
      (let [sA {:type :s-var :sym 'a} ; no typeclasses
            sB {:type :s-var :sym 'b :typeclasses #{:TCB}}
            result (u/mgu sA sB)]
        (is (= (count result) 2))
        (let [new-svar (get result 'a)]
          (is (identical? new-svar (get result 'b)))
          (is (= (:type new-svar) :s-var))
          (is (not (or (= (:sym new-svar) 'a) (= (:sym new-svar) 'b))))
          (is (= (:typeclasses new-svar) #{:TCB})))))

    (testing "overlapping typeclasses"
      (let [sA {:type :s-var :sym 'a :typeclasses #{:TC1 :TC-COMMON}}
            sB {:type :s-var :sym 'b :typeclasses #{:TC2 :TC-COMMON}}
            result (u/mgu sA sB)]
        (is (= (count result) 2))
        (let [new-svar (get result 'a)]
          (is (identical? new-svar (get result 'b)))
          (is (= (:type new-svar) :s-var))
          (is (not (or (= (:sym new-svar) 'a) (= (:sym new-svar) 'b))))
          (is (= (:typeclasses new-svar) #{:TC1 :TC2 :TC-COMMON}))))))
  
  (testing "occurs check"
    (is (= (u/mgu {:type :s-var :sym 'a}
                  {:type :vector :child {:type :s-var :sym 'a}})
           {:mgu-failure :occurs-check
            :schema-1    {:type :s-var :sym 'a}
            :schema-2    {:type  :vector
                          :child {:type :s-var :sym 'a}}}))
    
    (testing "occurs check with typeclasses"
      (let [s-var-tc {:type :s-var :sym 'a :typeclasses #{:number}}
            schema-tc {:type :vector :child {:type :s-var :sym 'a :typeclasses #{:number}}}
            result-tc (u/mgu s-var-tc schema-tc)]
        (is (u/mgu-failure? result-tc))
        (is (= (:mgu-failure result-tc) :occurs-check))
        ;; Adjusted keys:
        (is (= (:schema-1 result-tc) s-var-tc))
        (is (= (:schema-2 result-tc) schema-tc)))

      (let [s-var-outer {:type :s-var :sym 'a :typeclasses #{:number}}
            schema-inner-no-tc {:type :vector :child {:type :s-var :sym 'a}}
            result-inner-no-tc (u/mgu s-var-outer schema-inner-no-tc)]
        (is (u/mgu-failure? result-inner-no-tc))
        (is (= (:mgu-failure result-inner-no-tc) :occurs-check))
        ;; In this occurs check, 'a from s-var-outer is being unified with
        ;; schema-inner-no-tc. The 'a inside schema-inner-no-tc would be
        ;; substituted with s-var-outer if not for occurs check.
        ;; So, schema-1 is s-var-outer, and schema-2 is the structure.
        (is (= (:schema-1 result-inner-no-tc) s-var-outer))
        (is (= (:schema-2 result-inner-no-tc) schema-inner-no-tc)))))


  (testing "function types"
    ;; Case 1: Simple unification, a and b merge
    (let [fn-a {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'a :typeclasses #{:TA}}]}
                :output {:type :s-var :sym 'a :typeclasses #{:TA}}}
          fn-b {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'b :typeclasses #{:TB}}]}
                :output {:type :s-var :sym 'b :typeclasses #{:TB}}}
          result (u/mgu fn-a fn-b)
          ;; Expected: 'a and 'b map to a new s-var, say 's-new
          ;; result should be {'a s-new, 'b s-new}
          s-new-a (get result 'a)
          s-new-b (get result 'b)]
      (is (and s-new-a s-new-b))
      (is (identical? s-new-a s-new-b))
      (is (= (:type s-new-a) :s-var))
      (is (not (or (= (:sym s-new-a) 'a) (= (:sym s-new-a) 'b))))
      (is (= (:typeclasses s-new-a) (set/union #{:TA} #{:TB}))))

    ;; Case 2: More complex unification, a and b merge, then used
    (let [fn-x {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'x :typeclasses #{:TX}}
                                    {:type :s-var :sym 'x :typeclasses #{:TX}}]}
                :output {:type :s-var :sym 'x :typeclasses #{:TX}}}
          fn-y {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'y :typeclasses #{:TY}}
                                    {:type :s-var :sym 'y :typeclasses #{:TY}}]}
                :output {:type :s-var :sym 'y :typeclasses #{:TY}}}
          result (u/mgu fn-x fn-y)
          s-new-x (get result 'x)
          s-new-y (get result 'y)]
      (is (and s-new-x s-new-y))
      (is (identical? s-new-x s-new-y))
      (is (= (:type s-new-x) :s-var))
      (is (not (or (= (:sym s-new-x) 'x) (= (:sym s-new-x) 'y))))
      (is (= (:typeclasses s-new-x) (set/union #{:TX} #{:TY}))))

    ;; Case 3: Occurs check during function unification
    (let [fn-a {:type   :=>
                :input  {:type     :cat
                         :children [{:type :s-var :sym 'a :typeclasses #{:TA}}]}
                :output {:type :s-var :sym 'a :typeclasses #{:TA}}}
          fn-b-occurs {:type   :=>
                       :input  {:type     :cat
                                :children [{:type :s-var :sym 'b :typeclasses #{:TB}}]}
                       :output {:type  :vector
                                :child {:type :s-var :sym 'b :typeclasses #{:TB}}}}
          ;; mgu(fn-a, fn-b-occurs)
          ;; 1. Unify inputs: mgu(a,b) => {'a s-new, 'b s-new} where s-new has TCs from TA, TB
          ;;    env = {'a s-new, 'b s-new}
          ;; 2. Unify outputs (substituting from env):
          ;;    mgu( (subst env a), (subst env {:vector :child b}) )
          ;;    mgu( s-new, {:vector :child s-new} )
          ;;    This is an occurs check.
          result (u/mgu fn-a fn-b-occurs)]
      (is (u/mgu-failure? result))
      (is (= (:mgu-failure result) :occurs-check))
      ;; The occurs check happens when unifying the substituted output of fn-a (now s-new)
      ;; with the substituted output of fn-b-occurs (now {:vector :child s-new}).
      ;; The actual sym of s-new is generated internally, so we can't know it,
      ;; but we can check the structure and typeclasses.
      (let [schema1 (:schema-1 result)
            schema2 (:schema-2 result)]
        (is (= (:type schema1) :s-var))
        ;; We can't know the exact fresh symbol, so we check its properties if needed, or rely on typeclass check
        (is (= (:typeclasses schema1) (set/union #{:TA} #{:TB})))
        (is (= (:type schema2) :vector))
        (is (map? (:child schema2)))
        (is (= (:type (:child schema2)) :s-var))
        (is (= (:typeclasses (:child schema2)) (set/union #{:TA} #{:TB})))
        ;; Check if the s-vars involved in the occurs check are indeed the same *merged* s-var
        (is (= (:sym schema1) (:sym (:child schema2)))))))

  (testing "map types"
    (is (= (u/mgu {:type  :map-of
                   :key   {:type 'string?}
                   :value {:type :s-var, :sym 'v}}
                  {:type  :map-of
                   :key   {:type :s-var, :sym 'k}
                   :value {:type 'boolean?}})
           {'k {:type 'string?}
            'v {:type 'boolean?}})))
  
  (testing "tuple types"
    (is (= (u/mgu {:type     :tuple
                   :children [{:type :s-var, :sym 'a}
                              {:type 'int?}]}
                  {:type     :tuple
                   :children [{:type 'string?}
                              {:type :s-var, :sym 'b}]})
           {'a {:type 'string?}
            'b {:type 'int?}}))
    (is (u/mgu-failure? (u/mgu {:type     :tuple
                                :children [{:type :s-var, :sym 'a}
                                           {:type 'int?}
                                           {:type :s-var, :sym 'c}]}
                               {:type     :tuple
                                :children [{:type 'string?}
                                           {:type :s-var, :sym 'b}]}))))
  
  (testing "set types"
    (is (= (u/mgu {:type :set :child {:type :s-var, :sym 'a}}
                  {:type :set :child {:type 'int?}})
           {'a {:type 'int?}})))
  
  (testing "unification within structured types with typeclasses"
    (is (= (u/mgu {:type :vector :child {:type :s-var :sym 'a :typeclasses #{:number}}}
                  {:type :vector :child {:type 'int?}})
           {'a {:type 'int?}}))
    (let [s-var-child {:type :s-var :sym 'a :typeclasses #{:countable}}
          concrete-child {:type 'int?} ; int? is not :countable
          result (u/mgu {:type :vector :child s-var-child}
                        {:type :vector :child concrete-child})]
      (is (u/mgu-failure? result))
      (is (= (:mgu-failure result) :typeclass-mismatch))
      (is (= (:s-var result) s-var-child))
      (is (= (:schema result) concrete-child)))))

(deftest get-free-s-vars-defs-test
  (testing "simple s-var with typeclass"
    (is (= #{{:sym 'a :typeclasses #{:number}}}
           (u/get-free-s-vars-defs {:type :s-var :sym 'a :typeclasses #{:number}}))))
  (testing "nested schema with s-vars with typeclasses"
    (is (= #{{:sym 'a :typeclasses #{:number}} {:sym 'b :typeclasses #{:countable}}}
           (u/get-free-s-vars-defs {:type :vector :child {:type :tuple :children [{:type :s-var :sym 'a :typeclasses #{:number}} {:type :s-var :sym 'b :typeclasses #{:countable}}]}}))))
  (testing "function schema with s-vars with typeclasses in input and output"
    (is (= #{{:sym 'in :typeclasses #{:indexable}} {:sym 'out :typeclasses #{:callable}}}
           (u/get-free-s-vars-defs {:type   :=>
                                    :input  {:type     :cat
                                             :children [{:type :s-var :sym 'in :typeclasses #{:indexable}}]}
                                    :output {:type :s-var :sym 'out :typeclasses #{:callable}}}))))
  (testing "scheme with bound and free s-vars with typeclasses"
    (is (= #{{:sym 'c :typeclasses #{:comparable}}}
           (u/get-free-s-vars-defs {:type   :scheme
                                    :s-vars [{:sym 'x :typeclasses #{:number}}]
                                    :body   {:type :s-var :sym 'c :typeclasses #{:comparable}}})))
    (is (= #{{:sym 'a :typeclasses #{:number}} {:sym 'c :typeclasses #{:comparable}}}
           (u/get-free-s-vars-defs {:type   :scheme
                                    :s-vars [{:sym 'x :typeclasses #{:number}}]
                                    :body   {:type :tuple
                                             :children [{:type :s-var :sym 'a :typeclasses #{:number}}
                                                        {:type :s-var :sym 'x :typeclasses #{:number}}
                                                        {:type :s-var :sym 'c :typeclasses #{:comparable}}]}}))))
  (testing "schema with no free s-vars with typeclasses"
    (is (= #{{:sym 'a}}
           (u/get-free-s-vars-defs {:type :s-var :sym 'a})))
    (is (= #{}
           (u/get-free-s-vars-defs {:type   :scheme
                                    :s-vars [{:sym 'x :typeclasses #{:number}}]
                                    :body   {:type :s-var :sym 'x :typeclasses #{:number}}})))
    (is (= #{}
           (u/get-free-s-vars-defs {:type 'int?}))))
  (testing "schema with multiple distinct free s-vars with different typeclasses"
    (is (= #{{:sym 'a :typeclasses #{:number}} {:sym 'b :typeclasses #{:countable}} {:sym 'c :typeclasses #{:comparable}}}
           (u/get-free-s-vars-defs {:type :tuple
                                    :children [{:type :s-var :sym 'a :typeclasses #{:number}}
                                               {:type :s-var :sym 'b :typeclasses #{:countable}}
                                               {:type :s-var :sym 'c :typeclasses #{:comparable}}]})))))

(deftest satisfies-all-typeclasses?-test
  (testing "s-var schema"
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses #{:number :comparable}} #{:number :comparable}))
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses #{:number :comparable :countable}} #{:number :comparable}))
    (is (not (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses #{:number}} #{:number :comparable})))
    ;; Current impl: unconstrained s-var satisfies any requirement.
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a} #{:number}))
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses #{:number}} #{}))
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses #{:number}} nil)))
  (testing "concrete type schema"
    (is (u/satisfies-all-typeclasses? {:type 'int?} #{:number}))
    (is (u/satisfies-all-typeclasses? {:type 'int?} #{:number :comparable}))
    (is (not (u/satisfies-all-typeclasses? {:type 'int?} #{:countable})))
    (is (not (u/satisfies-all-typeclasses? {:type 'int?} #{:callable})))

    (is (not (u/satisfies-all-typeclasses? {:type 'string?} #{:number})))
    (is (not (u/satisfies-all-typeclasses? {:type 'string?} #{:callable})))
    (is (u/satisfies-all-typeclasses? {:type 'string?} #{:countable :comparable :indexable}))

    (is (not (u/satisfies-all-typeclasses? {:type 'int?} #{:number :countable})))
    (is (u/satisfies-all-typeclasses? {:type 'int?} #{}))
    (is (u/satisfies-all-typeclasses? {:type 'int?} nil)))
  (testing "structured type schema"
    (is (u/satisfies-all-typeclasses? {:type :vector :child {:type 'int?}} #{:countable}))
    (is (u/satisfies-all-typeclasses? {:type :set :child {:type 'string?}} #{:countable}))
    (is (u/satisfies-all-typeclasses? {:type :map-of :key {:type 'keyword?} :value {:type 'int?}} #{:countable}))
    ;; A vector of numbers is not itself a number.
    (is (not (u/satisfies-all-typeclasses? {:type :vector :child {:type 'int?}} #{:number}))))
  (testing "unknown typeclass keyword"
    ;; If an unknown typeclass is required, it cannot be satisfied.
    (is (not (u/satisfies-all-typeclasses? {:type 'int?} #{:unknown-typeclass})))
    (is (not (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses #{:number}} #{:unknown-typeclass})))))
