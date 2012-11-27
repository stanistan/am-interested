(ns am-interested.utils
  (:require [clojure.walk :as walk]
            [clojure.string :as string]))

(defn maybe-apply
  "Applies f to val if (pred val) is truthy. Otherwise,
  returns val."
  [pred f val]
  (if (pred val) (f val) val))

(defn map-keys
  "Recursively maps a function onto the keys of a map."
  [f m]
  (let [mapper (fn [[k v]] [(f k) v])
        walker (fn [x]
                 (maybe-apply map? #(into {} (map mapper %)) x))]
    (walk/postwalk walker m)))

(defn string->keyword
  [k]
  (maybe-apply string? #(keyword (string/replace % " " "-")) k))

(defn keyword->string
  [k]
  (maybe-apply keyword? #(string/replace (name %) "-" " ") k))

(def keywordize-keys (partial map-keys string->keyword))
(def stringify-keys (partial map-keys keyword->string))
