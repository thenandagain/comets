(ns thenandagain.comets.system-test
  (:require [clojure.test :refer :all]
            [thenandagain.comets.system :refer :all]))

(deftest ces
  (testing "I can add components to entities"
    (let [e-id (create-entity)
          result (-> (create-system)
                     (add-component e-id :foo :bar)
                     (get-components e-id))]
      (is (contains? result :foo))
      (is (= (:foo result) :bar))))

  (testing "I can get all entities with a particular component"
    (let [e-id1  (create-entity)
          e-id2  (create-entity)
          system (-> (create-system)
                     (add-component e-id1 :foo :bar)
                     (add-component e-id1 :baz :bat)
                     (add-component e-id2 :foo :bar))
          result-foo (entities-with-component system :foo)
          result-baz (entities-with-component system :baz)
          result-empty (entities-with-component system :herp)]

      (is (= (count result-foo)   2))
      (is (= (count result-baz)   1))
      (is (= (count result-empty) 0))))

  (testing "I can get a single component from an entity"
    (let [e-id  (create-entity)
          system (-> (create-system)
                     (add-component e-id :foo :bar))
          found (get-component system e-id :foo)
          not-found (get-component system e-id :baz :FUBAR)]
      (is (= found :bar))
      (is (= not-found :FUBAR))))

  (testing "I can update an entity's component"
    (let [e-id  (create-entity)
          result (-> (create-system)
                     (add-component e-id :foo :bar)
                     (update-component e-id :foo (fn [v] :bat))
                     (get-component e-id :foo))]

      (is (= result :bat)))))

