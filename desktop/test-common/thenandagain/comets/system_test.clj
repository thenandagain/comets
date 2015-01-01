(ns thenandagain.comets.system-test
  (:require [clojure.test :refer :all]
            [thenandagain.comets.system :refer :all]))

(defrecord TestComponent [v])
(defrecord OtherTestComponent [v])
(defrecord UnusedTestComponent [v])

(deftest ces
  (testing "I can add components to entities"
    (let [e-id (create-entity)
          result (-> (create-system)
                     (add-component e-id (->TestComponent :foo))
                     (get-components e-id))]
      (is (= (count result) 1))
      (is (= (-> result
                 first
                 :v) :foo))))

  (testing "I can get all entities with a particular component"
    (let [e-id1  (create-entity)
          e-id2  (create-entity)
          system (-> (create-system)
                     (add-component e-id1 (->TestComponent :foo))
                     (add-component e-id1 (->OtherTestComponent :foo))
                     (add-component e-id2 (->TestComponent :foo)))
          result-foo (entities-with-component system TestComponent)
          result-baz (entities-with-component system OtherTestComponent)
          result-empty (entities-with-component system UnusedTestComponent)]

      (is (= (count result-foo)   2))
      (is (= (count result-baz)   1))
      (is (= (count result-empty) 0))))

  (testing "I can get a single component from an entity"
    (let [e-id  (create-entity)
          system (-> (create-system)
                     (add-component e-id (->TestComponent :foo)))
          found (get-component system e-id TestComponent)
          not-found (get-component system e-id UnusedTestComponent :FUBAR)]
      (is (= (:v found) :foo))
      (is (= not-found :FUBAR))))

  (testing "I can update an entity's component"
    (let [e-id  (create-entity)
          result (-> (create-system)
                     (add-component e-id (->TestComponent :foo))
                     (update-component e-id TestComponent (fn [v] (->TestComponent :bar)))
                     (get-component e-id TestComponent))]

      (is (= (:v result) :bar))))

  (testing "I can remove a component"
    (let [e-id (create-entity)
          components (-> (create-system)
                         (add-component e-id (->TestComponent :foo))
                         (add-component e-id (->OtherTestComponent :bar))
                         (remove-component e-id OtherTestComponent)
                         (all-components))]
      (is (= (count components) 1))
      (is (= (class (first components)) TestComponent))))
  )

