(ns erp12.schema-inference.impl.util-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [erp12.schema-inference.impl.util :as u]))

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
  (let [x->y #(u/substitute {'x {:type :s-var :sym 'y}} %)]
    (is (= {:type :s-var :sym 'y}
           (x->y {:type :s-var :sym 'x})))
    (is (= {:type :s-var :sym 'z}
           (x->y {:type :s-var :sym 'z})))
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
                    :output {:type :s-var :sym 'x}}))))
    (testing "scheme"
      (is (= {:type   :scheme
              :s-vars [{:sym 'z}]
              :body   {:type :s-var :sym 'y}}
             (x->y {:type   :scheme
                    :s-vars [{:sym 'z}]
                    :body   {:type :s-var :sym 'x}})))
      ;; Occurs check
      (is (= {:type   :scheme
              :s-vars [{:sym 'x}]
              :body   {:type :s-var :sym 'x}}
             (x->y {:type   :scheme
                    :s-vars [{:sym 'x}]
                    :body   {:type :s-var :sym 'x}}))))))

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
                  :s-vars [{:sym 'x :typeclasses [:number]} {:sym 'y :typeclasses [:comparable]}]
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
      (is (= (:typeclasses s-var1) [:number]))
      (is (= (:type s-var2) :s-var))
      (is (not= (:sym s-var2) 'y))             ; Fresh symbol
      (is (= (:typeclasses s-var2) [:comparable]))))
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
                                :children [{:type :s-var :sym 'y :typeclasses [:number]}
                                           {:type :s-var :sym 'z :typeclasses [:comparable]}]}
          generalized (u/generalize env schema-to-generalize)
          s-vars-map (into {} (map (juxt :sym identity) (:s-vars generalized)))]
      (is (= (:type generalized) :scheme))
      (is (= (count (:s-vars generalized)) 2))
      (is (= (get-in s-vars-map ['y :typeclasses]) [:number]))
      (is (= (get-in s-vars-map ['z :typeclasses]) [:comparable]))
      ;; generalize sorts s-vars by sym, so we sort original schema's s-vars for comparison if needed
      (is (= (:body generalized) schema-to-generalize))))
  (testing "generalize schema where some s-vars with typeclasses are also in env"
    (let [env {'y {:type :s-var :sym 'y :typeclasses [:number]}} ; 'y is in env
          schema-to-generalize {:type     :tuple
                                :children [{:type :s-var :sym 'y :typeclasses [:number]}
                                           {:type :s-var :sym 'z :typeclasses [:comparable]}]}
          generalized (u/generalize env schema-to-generalize)
          s-vars-map (into {} (map (juxt :sym identity) (:s-vars generalized)))]
      (is (= (:type generalized) :scheme))
      (is (= (count (:s-vars generalized)) 1))
      (is (nil? (get s-vars-map 'y))) ; 'y should not be generalized
      (is (= (get-in s-vars-map ['z :typeclasses]) [:comparable]))
      (is (= (:body generalized) schema-to-generalize))))
  (testing "generalize a scheme (should instantiate first)"
    (let [env {}
          scheme-to-generalize {:type   :scheme
                                :s-vars [{:sym 'x :typeclasses [:number]}]
                                :body   {:type :s-var :sym 'x}}
          generalized (u/generalize env scheme-to-generalize)
          original-body (:body scheme-to-generalize)]
      ;; Since 'x' is bound by the input scheme, after instantiation,
      ;; the new s-var (e.g., s-1) will be free relative to env and generalized.
      (is (= (:type generalized) :scheme))
      (is (= (count (:s-vars generalized)) 1))
      (let [generalized-s-var (first (:s-vars generalized))]
        ;; The s-var in the new scheme should be the fresh var from instantiation
        (is (not= (:sym generalized-s-var) 'x))
        (is (str/starts-with? (name (:sym generalized-s-var)) "s-"))
        (is (= (:typeclasses generalized-s-var) [:number]))
        ;; The body of the new scheme should be the instantiated body of the input scheme
        (is (= (:type (:body generalized)) :s-var))
        (is (= (:sym (:body generalized)) (:sym generalized-s-var))) ; Body uses the fresh, generalized s-var
        (is (= (:typeclasses (:body generalized)) [:number]))))))

(deftest mgu-test
  (testing "atomic types"
    (is (= (u/mgu {:type 'int?} {:type 'int?})
           {}))
    (is (= (u/mgu {:type 'int?} {:type 'string?})
           {:mgu-failure :non-equal
            :schema-1    {:type 'int?}
            :schema-2    {:type 'string?}})))
  (testing "s-vars"
    (is (= (u/mgu {:type :s-var :sym 'a}
                  {:type :s-var :sym 'b})
           {'a {:type :s-var :sym 'b}}))
    (is (= (u/mgu {:type 'int?}
                  {:type :s-var :sym 'a})
           {'a {:type 'int?}}))
    (is (= (u/mgu {:type :s-var :sym 'a}
                  {:type :s-var :sym 'a})
           {})))
  (testing "s-vars with typeclasses"
    (testing "s-var with concrete type - success"
      (is (= (u/mgu {:type :s-var :sym 'a :typeclasses [:number]} {:type 'int?})
             {'a {:type 'int?}}))
      (is (= (u/mgu {:type :s-var :sym 'a :typeclasses [:number :comparable]} {:type 'int?})
             {'a {:type 'int?}})))
    (testing "s-var with concrete type - failure (typeclass mismatch)"
      (let [s-var {:type :s-var :sym 'a :typeclasses [:countable]}
            concrete-schema {:type 'int?}
            result (u/mgu s-var concrete-schema)]
        (is (u/mgu-failure? result))
        (is (= (:mgu-failure result) :typeclass-mismatch))
        (is (= (:s-var result) s-var))
        (is (= (:schema result) concrete-schema))))
    (testing "s-var with s-var - success"
      (let [s-var-a {:type :s-var :sym 'a :typeclasses [:number]}
            s-var-b {:type :s-var :sym 'b :typeclasses [:number :comparable]}]
        ;; We expect 'a to be bound to 'b as 'b is more constrained or at least equally.
        (is (= (u/mgu s-var-a s-var-b) {'a s-var-b})))
      (let [s-var-a {:type :s-var :sym 'a :typeclasses [:number :comparable]}
            s-var-b {:type :s-var :sym 'b :typeclasses [:number]}]
        ;; We expect 'b to be bound to 'a as 'a is more constrained.
        (is (= (u/mgu s-var-a s-var-b) {'b s-var-a})))
      (let [s-var-unconstrained {:type :s-var :sym 'a}
            s-var-constrained {:type :s-var :sym 'b :typeclasses [:number]}]
        ;; Unconstrained 'a' should be bound to constrained 'b'.
        (is (= (u/mgu s-var-unconstrained s-var-constrained) {'a s-var-constrained}))
        ;; Order reversed, 'a' (now constrained) should bind 'b' (unconstrained)
        (is (= (u/mgu s-var-constrained s-var-unconstrained)
               {'b s-var-constrained}))))
    (testing "s-var with s-var - failure (typeclass mismatch)"
      (let [s-var-a {:type :s-var :sym 'a :typeclasses [:number]}
            s-var-b {:type :s-var :sym 'b :typeclasses [:countable]}
            result (u/mgu s-var-a s-var-b)]
        (is (u/mgu-failure? result))
        (is (= (:mgu-failure result) :typeclass-mismatch))
        ;; bind-var(s-var-a, s-var-b) is called, s-var-a's typeclasses are checked against s-var-b
        ;; satisfies-all-typeclasses? for s-var schema (s-var-b) checks if s-var-b's typeclasses
        ;; are a superset of s-var-a's. They are not.
        (is (= (:s-var result) s-var-a))
        (is (= (:schema result) s-var-b)))
      (let [s-var-a {:type :s-var :sym 'a :typeclasses [:number]}
            s-var-b {:type :s-var :sym 'b :typeclasses [:number :countable]} ; b is more specific
            result (u/mgu s-var-a s-var-b)] ; This should succeed, 'a gets bound to 'b
        (is (= result {'a s-var-b})))
      (let [s-var-a {:type :s-var :sym 'a :typeclasses [:number :countable]} ; a is more specific
            s-var-b {:type :s-var :sym 'b :typeclasses [:number]}
            result (u/mgu s-var-a s-var-b)] ; This should succeed, 'b gets bound to 'a
        (is (= result {'b s-var-a}))))
    (testing "occurs check with typeclasses"
      (let [s-var {:type :s-var :sym 'a :typeclasses [:number]}
            schema {:type :vector :child {:type :s-var :sym 'a :typeclasses [:number]}}
            result (u/mgu s-var schema)]
        (is (u/mgu-failure? result))
        (is (= (:mgu-failure result) :occurs-check))
        (is (= (:s-var result) s-var)))
      (let [s-var {:type :s-var :sym 'a :typeclasses [:number]}
            schema {:type :vector :child {:type :s-var :sym 'a }} ; s-var in schema has no TCs
            result (u/mgu s-var schema)]
        ;; This should still be an occurs check. The typeclasses on the outer s-var don't prevent it.
        ;; The inner s-var 'a would be bound to the outer 'a which has typeclasses.
        (is (u/mgu-failure? result))
        (is (= (:mgu-failure result) :occurs-check))
        (is (= (:s-var result) {:type :s-var :sym 'a})) ;; s-var from occurs check is the one in the structure
        (is (= (:schema result) s-var)))
        ))
  (testing "function types"
    (is (= (u/mgu {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'a}]},
                   :output {:type :s-var, :sym 'a}}
                  {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'b}]},
                   :output {:type :s-var, :sym 'b}})
           {'a {:type :s-var :sym 'b}}))
    (is (= (u/mgu {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'a}
                                       {:type :s-var, :sym 'a}]},
                   :output {:type :s-var, :sym 'a}}
                  {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'b}
                                       {:type :s-var, :sym 'b}]},
                   :output {:type :s-var, :sym 'b}})
           {'a {:type :s-var :sym 'b}}))
    (is (= (u/mgu {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'a}]},
                   :output {:type :s-var, :sym 'a}}
                  {:type   :=>,
                   :input  {:type     :cat,
                            :children [{:type :s-var, :sym 'b}]},
                   :output {:type  :vector
                            :child {:type :s-var, :sym 'b}}})
           {:mgu-failure :occurs-check
            :schema-1    {:type :s-var, :sym 'b}
            :schema-2    {:type  :vector
                          :child {:type :s-var, :sym 'b}}})))
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
    (is (= (u/mgu {:type :vector :child {:type :s-var :sym 'a :typeclasses [:number]}}
                  {:type :vector :child {:type 'int?}})
           {'a {:type 'int?}}))
    (let [s-var-child {:type :s-var :sym 'a :typeclasses [:countable]}
          concrete-child {:type 'int?}
          result (u/mgu {:type :vector :child s-var-child}
                        {:type :vector :child concrete-child})]
      (is (u/mgu-failure? result))
      (is (= (:mgu-failure result) :typeclass-mismatch))
      ;; The failure is due to the children, so s-var and schema should reflect that.
      (is (= (:s-var result) s-var-child))
      (is (= (:schema result) concrete-child)))))

(deftest get-free-s-vars-defs-test
  (testing "simple s-var with typeclass"
    (is (= #{{:sym 'a :typeclasses [:number]}}
           (u/get-free-s-vars-defs {:type :s-var :sym 'a :typeclasses [:number]}))))
  (testing "nested schema with s-vars with typeclasses"
    (is (= #{{:sym 'a :typeclasses [:number]} {:sym 'b :typeclasses [:countable]}}
           (u/get-free-s-vars-defs {:type :vector :child {:type :tuple :children [{:type :s-var :sym 'a :typeclasses [:number]} {:type :s-var :sym 'b :typeclasses [:countable]}]}}))))
  (testing "function schema with s-vars with typeclasses in input and output"
    (is (= #{{:sym 'in :typeclasses [:map]} {:sym 'out :typeclasses [:vector]}}
           (u/get-free-s-vars-defs {:type   :=>
                                    :input  {:type     :cat
                                             :children [{:type :s-var :sym 'in :typeclasses [:map]}]}
                                    :output {:type :s-var :sym 'out :typeclasses [:vector]}}))))
  (testing "scheme with bound and free s-vars with typeclasses"
    (is (= #{{:sym 'c :typeclasses [:comparable]}}
           (u/get-free-s-vars-defs {:type   :scheme
                                    :s-vars [{:sym 'x :typeclasses [:number]}]
                                    :body   {:type :s-var :sym 'c :typeclasses [:comparable]}})))
    (is (= #{{:sym 'a :typeclasses [:number]} {:sym 'c :typeclasses [:comparable]}}
           (u/get-free-s-vars-defs {:type   :scheme
                                    :s-vars [{:sym 'x :typeclasses [:number]}]
                                    :body   {:type :tuple
                                             :children [{:type :s-var :sym 'a :typeclasses [:number]}
                                                        {:type :s-var :sym 'x :typeclasses [:number]}
                                                        {:type :s-var :sym 'c :typeclasses [:comparable]}]}}))))
  (testing "schema with no free s-vars with typeclasses"
    (is (= #{{:sym 'a}}
           (u/get-free-s-vars-defs {:type :s-var :sym 'a})))
    (is (= #{}
           (u/get-free-s-vars-defs {:type   :scheme
                                    :s-vars [{:sym 'x :typeclasses [:number]}]
                                    :body   {:type :s-var :sym 'x :typeclasses [:number]}})))
    (is (= #{}
           (u/get-free-s-vars-defs {:type 'int?}))))
  (testing "schema with multiple distinct free s-vars with different typeclasses"
    (is (= #{{:sym 'a :typeclasses [:number]} {:sym 'b :typeclasses [:string]} {:sym 'c :typeclasses [:boolean]}}
           (u/get-free-s-vars-defs {:type :tuple
                                    :children [{:type :s-var :sym 'a :typeclasses [:number]}
                                               {:type :s-var :sym 'b :typeclasses [:string]}
                                               {:type :s-var :sym 'c :typeclasses [:boolean]}]})))))

(deftest satisfies-all-typeclasses?-test
  (testing "s-var schema"
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses [:number :comparable]} [:number :comparable]))
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses [:number :comparable :countable]} [:number :comparable]))
    (is (not (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses [:number]} [:number :comparable])))
    ;; Current impl: unconstrained s-var satisfies any requirement.
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a} [:number]))
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses [:number]} []))
    (is (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses [:number]} nil)))
  (testing "concrete type schema"
    (is (u/satisfies-all-typeclasses? {:type 'int?} [:number]))
    (is (u/satisfies-all-typeclasses? {:type 'int?} [:number :comparable]))
    (is (not (u/satisfies-all-typeclasses? {:type 'int?} [:countable])))
    (is (not (u/satisfies-all-typeclasses? {:type 'int?} [:callable])))

    (is (not (u/satisfies-all-typeclasses? {:type 'string?} [:number])))
    (is (not (u/satisfies-all-typeclasses? {:type 'string?} [:callable])))
    (is (u/satisfies-all-typeclasses? {:type 'string?} [:countable :comparable :indexable]))

    (is (not (u/satisfies-all-typeclasses? {:type 'int?} [:number :countable])))
    (is (u/satisfies-all-typeclasses? {:type 'int?} []))
    (is (u/satisfies-all-typeclasses? {:type 'int?} nil)))
  (testing "structured type schema"
    (is (u/satisfies-all-typeclasses? {:type :vector :child {:type 'int?}} [:countable]))
    (is (u/satisfies-all-typeclasses? {:type :set :child {:type 'string?}} [:countable]))
    (is (u/satisfies-all-typeclasses? {:type :map-of :key {:type 'keyword?} :value {:type 'int?}} [:countable]))
    ;; A vector of numbers is not itself a number.
    (is (not (u/satisfies-all-typeclasses? {:type :vector :child {:type 'int?}} [:number]))))
  (testing "unknown typeclass keyword"
    ;; If an unknown typeclass is required, it cannot be satisfied.
    (is (not (u/satisfies-all-typeclasses? {:type 'int?} [:unknown-typeclass])))
    (is (not (u/satisfies-all-typeclasses? {:type :s-var :sym 'a :typeclasses [:number]} [:unknown-typeclass])))))
