(ns socklet.utils)

(defn repeatedly-call
  "Repeatedly call function f. Put this in a future."
  [f]
  (loop [] (f) (recur)))
