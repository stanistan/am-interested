(ns utils.string)

(defn repeat-str
  [s n]
  (apply str (repeat n s)))

(defn str-padd
  [to-padd with l]
  (let [c (count to-padd)
        left (- l c)]
    (str (repeat-str with left) to-padd)))
