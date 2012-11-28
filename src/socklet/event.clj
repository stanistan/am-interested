(ns socklet.event)

(defn listen-for
  [f & args]
  (future
    (loop []
      (apply f args)
      (recur))))
