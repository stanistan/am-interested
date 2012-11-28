(ns chomp.chomp)

(defn find-in
  "Finds a map in a vector which has key given the value."
  [coll key value]
  (letfn [(map-has-val? [m v] (= (m key) v))
          (check [a b] (or a (if (map-has-val? b value) b) a))]
    (or (reduce check false coll) {})))

(defn plural?
  [s]
  (= (last s) \s))

(defn singularize
  [s]
  (if (plural? s) (apply str (butlast s)) s))

(defn parse-prefix
  [s]
  (let [prefix (if ((complement empty?) s) (or (read-string s) nil) nil)]
    (cond (integer? prefix) prefix
          (nil? prefix) nil
          :else (keyword prefix))))

(defn key-info
  [k]
  (let [[length type] [(parse-prefix (namespace k)) (name k)]]
    {:length (if (plural? type) length 1)
     :type (keyword (singularize type))}))

(defn prep-conf
  [c]
  (let [c (if (sequential? c) c [c])
        s (first (filter symbol? c))
        k (first (filter keyword? c))]
    (merge (key-info k)
           (if (nil? s) {}
               {:name (keyword s)}))))

(defn named?
  [m]
  (every? #((complement nil?) (:name %)) m))

(defn bit-struct*
  [& ms]
  (let [s (into [] (map prep-conf ms))]
    {:named? (named? s)
     :data s}))

(defmacro bit-struct
  [n & args]
  (let [info (apply bit-struct* (into [] args))]
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
