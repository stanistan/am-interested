(ns chomp.utils)

(defn find-in
  "Finds a map in a vector which has key given the value."
  [coll key value]
  (letfn [(map-has-val [m v] (when (= (m key) v) m))
          (check [a b] (or (map-has-val b value) a))]
    (reduce check nil coll)))

(defn index-of
  [v val]
  (let [i (.indexOf v val)]
    (when (not= -1 i) i)))
