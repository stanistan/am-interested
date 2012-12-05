(ns socklet.event
  (:require [socklet.utils :as utils]))

(defn listen-for
  [f & args]
  (future
    (utils/repeatedly-call #(apply f args))))
