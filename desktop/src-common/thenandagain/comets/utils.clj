(ns thenandagain.comets.utils
  (:require [play-clj.math :as math])
  (:import [com.badlogic.gdx.math Circle Polygon]))

(defn polygon-segments [p]
  (let [vertices (math/polygon! p :get-transformed-vertices)
        v-count (count vertices)
        initial-segment [[(math/vector-2 (nth vertices 0) (nth vertices 1))
                          (math/vector-2 (nth vertices (- v-count 2)) (nth vertices (- v-count 1)))]]]
    (loop [segments initial-segment
           vertices vertices]
      (if (>= (count vertices) 4)
        (recur (conj segments [(math/vector-2 (nth vertices 0) (nth vertices 1))
                               (math/vector-2 (nth vertices 2) (nth vertices 3))])
               (nthrest vertices 2))
        segments))))

(defn polygon-overlaps-circle [^Polygon p ^Circle c]
  (let [segments (polygon-segments p)]
    (loop [remaining segments]
      (if-let [s (first remaining)]
        (if (math/intersector! :intersect-segment-circle
                               (first s) (second s)
                               (math/vector-2 (. c x) (. c y))
                               (Math/pow (. c radius) 2))
          true
          (recur (rest remaining)))
        false))))

(defn circle-overlaps-circle [^Circle c1 ^Circle c2]
  (math/intersector! :overlaps c1 c2))
