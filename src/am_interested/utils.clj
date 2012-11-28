(ns am-interested.utils
  (:require [clojure.walk :as walk]
            [clojure.string :as string]
            [am-interested.config :as config]))

(defn apply-if
  "Applies f to val if (pred val) is truthy. Otherwise,
  returns val."
  [pred f val]
  (if (pred val) (f val) val))

(defn map-keys
  "Recursively maps a function onto the keys of a map."
  [f m]
  (let [mapper (fn [[k v]] [(f k) v])
        walker (fn [x]
                 (apply-if map? #(into {} (map mapper %)) x))]
    (walk/postwalk walker m)))

(defn string->keyword
  [k]
  (apply-if string? #(keyword (string/replace % " " "-")) k))

(defn keyword->string
  [k]
  (apply-if keyword? #(string/replace (name %) "-" " ") k))

(def keywordize-keys (partial map-keys string->keyword))
(def stringify-keys (partial map-keys keyword->string))

(defn repeat-str
  [s n]
  (apply str (repeat n s)))

(defn str-padd
  [to-padd with l]
  (let [c (count to-padd)
        left (- l c)]
    (str (repeat-str with left) to-padd)))

(defn unicode-string
  [byte]
  (let [hexstr (apply str (take-last 2 (Integer/toHexString byte)))]
    (str (read-string (str "\\u" (str-padd hexstr "0" 4))))))

(defn hash-digest [^String type ^bytes data]
  (.digest (java.security.MessageDigest/getInstance type)
           data))

(def sha1 (partial hash-digest "sha1"))

(defn encode-in [type s]
  (java.net.URLEncoder/encode s type))

(defn decode-in [type s]
  (java.net.URLDecoder/decode s type))

(def str-encode (partial encode-in (config/consts :str-encoding)))
(def str-decode (partial decode-in (config/consts :str-encoding)))

(def iso-encode (partial encode-in "ISO-8859-1"))
(def utf-encode (partial encode-in "UTF-8"))

(def iso-decode (partial decode-in "ISO-8859-1"))
(def utf-decode (partial decode-in "UTF-8"))

(defn gen-id []
  (apply str (take 20
                   (concat (str (System/currentTimeMillis))
                           (str (rand))))))
