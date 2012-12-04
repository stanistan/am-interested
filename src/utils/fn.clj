(ns utils.fn)

(defn apply-if
  "Applies f to val if (pred val) is truthy. Otherwise,
  returns val."
  [pred f val]
  (if (pred val) (f val) val))
