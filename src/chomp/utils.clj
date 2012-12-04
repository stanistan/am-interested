(ns chomp.utils
  (:use utils.string utils.fn))

(defn find-in
  "Finds a map in a vector which has key given the value."
  [coll key value]
  (letfn [(map-has-val [m v] (when (= (get m key) v) m))
          (check [a b] (or (map-has-val b value) a))]
    (reduce check nil coll)))

(defn index-of
  [v val]
  (let [i (.indexOf v val)]
    (when (not= -1 i) i)))

(defn vectorify
  [s]
  (if (sequential? s) s [s]))

(def read-byte
  (comp byte read-string str))

(defn zero-padd-byte
  [length n]
  (-> (str n)
    (str-padd 0 length)
    ((partial map read-byte))))
