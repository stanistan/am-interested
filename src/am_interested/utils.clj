(ns am-interested.utils
  (:require [clojure.walk :as walk]
            [clojure.string :as string]))

(defn map-keys
  "Recursively maps a function onto the keys of a map."
  [f m]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (into {} (map (fn [[k v]] [(f k) v]) x))
                     x))
                 m))

(defn string->keyword
  [k]
  (if (string? k)
    (keyword (string/replace k " " "-"))
    k))

(defn keyword->string
  [k]
  (if (keyword? k)
    (string/replace (name k) "-" " ")
    k))

(def keywordize-keys (partial map-keys string->keyword))
(def stringify-keys (partial map-keys keyword->string))
