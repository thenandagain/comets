(ns thenandagain.comets.utils-test
  (:require [clojure.test :refer :all]
            [thenandagain.comets.utils :refer :all]
            [play-clj.math :refer [circle polygon vector-2]]))

(deftest polygon-segments-test
  (testing "I can get polygon line segments"
    (let [p (polygon (float-array [0.0 10.0, 8.0 -10.0, -8.0 -10.0]))
          segments (polygon-segments p)]
      (is (= (count segments) 3))
      #_(is (= segments
             [[(vector-2 0.0 10.0)
               (vector-2 -8.0 -10.0)]
              [(vector-2 0.0 10.0)
               (vector-2 8.0 -10.0)
              [(vector-2 8.0 -10.0)
               (vector-2 -8.0 -10.0)]]])))))

(deftest polygon-overlaps-circle-test
  (testing "polygon and circle overlapping"
    (let [p (polygon (float-array [-1 0, 0 1, 1 0]))
          c (circle 0 0 1)]
      (is (polygon-overlaps-circle p c))))
  
  (testing "polygon and circle not overlapping"
    (let [p (polygon (float-array [-1 0, 0 1, 1 0]))
          c (circle 10 10 1)]
      (is (not (polygon-overlaps-circle p c))))))
