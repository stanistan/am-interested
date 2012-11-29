(ns chomp.chomp)

(defn find-in
  "Finds a map in a vector which has key given the value."
  [coll key value]
  (letfn [(map-has-val [m v] (when (= (m key) v) m))
          (check [a b] (or (map-has-val b value) a))]
    (reduce check nil coll)))

(defn plural?
  [s]
  (= (last s) \s))

(defn singularize
  [s]
  (if (plural? s) (apply str (butlast s)) s))

(defn parse-prefix
  [s]
  (when-let [prefix (when (not (empty? s)) (read-string s))]
    (if (integer? prefix)
      prefix
      (keyword prefix))))

(defn key-info
  [k]
  (let [length (parse-prefix (namespace k))
        type (name k)]
    {:length (if (plural? type) length 1)
     :type (keyword (singularize type))}))

(defn prep-conf
  [c]
  (let [c (if (sequential? c) c [c])
        s (first (filter symbol? c))
        k (first (filter keyword? c))]
    (assoc (key-info k) :name (keyword s))))

(defn named?
  [m]
  (every? :name m))

(defn bit-struct*
  [& bit-specs]
  (let [s (mapv prep-conf bit-specs)]
    {:named? (named? s)
     :data s}))

(defmacro bit-struct
  [n & bit-specs]
  (let [info (apply bit-struct* bit-specs)]
    `(def ~n ~info)))

; [len :10/bytes]


; { :named
;   :data [{:type :byte
;           :length 10
;           :name :len
;           :index}
;          {:name :1}]}

; (chomp/bit-struct handshake
;   [len :byte]
;   [protocol :len/bytes]
;   [reserved :8/bytes]
;   [payload :bytes])

; (->handshake 5 "BitTorrent protocol" (reserved-bytes) payload)

; (map->handshake {:len 5
;                  :protocol "BitTorrent protocol"
;                  :reserved (reserved-bytes)
;                  :payload payload})
