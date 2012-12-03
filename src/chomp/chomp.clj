(ns chomp.chomp
  (:require [chomp.match :as match]
            [chomp.utils :as utils]))

(defprotocol Byteable
  (to-bytes [this] "Convert to an array of bytes."))

(extend-protocol Byteable
  (Class/forName "[B")  ;ByteArray
  (to-bytes [s] s)

  String
  (to-bytes [s] (.getBytes s))

  Long
  (to-bytes [l] (byte-array [(byte l)])))

(defmulti convert-to-type
  (fn [spec data]
    (:type spec)))

(defmethod convert-to-type :byte
  [spec data]
  (to-bytes data))

(defrecord BitStruct [data named?])

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
  [bit-spec]
  (match/destruct (utils/vectorify bit-spec)

    [[symbol? name] [keyword? spec] [keyword? cast]]
    (assoc (key-info spec) :name (keyword name) :cast cast)

    [[symbol? name] [keyword? spec]]
    (assoc (key-info spec) :name (keyword name))

    [[keyword? spec]]
    (key-info spec)))

(defn named?
  [m]
  (every? :name m))

(defn bit-struct*
  [& bit-specs]
  (let [s (mapv prep-conf bit-specs)]
    (->BitStruct s (named? s))))

(defn valid-length?
  [matched specified given]
  (cond (integer? specified) (= specified given)
        (keyword? specified) (= (first (:value (utils/find-in matched :name specified)))
                                given)
        (nil? specified) true
        :else false))

(defn match-pair
  [matched [spec data]]
  (let [converted (convert-to-type spec data)]
    (when (valid-length? matched (:length spec) (count converted))
      (conj matched (assoc spec :value converted)))))

(defn encode*
  [specs data]
  (let [pairs (map vector (:data specs) data)]
    (loop [matched [] pairs pairs]
      (if (seq pairs)
        (if-let [matched (match-pair matched (first pairs))]
          (recur matched (rest pairs))
          (throw (Exception. (str "encoding failed: " (first pairs)))))
        matched))))

(defn encode
  [specs & data]
  (let [values (encode* specs data)]
    (byte-array (apply concat (map :value values)))))

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
;   [len :byte :]
;   [protocol :len/bytes]
;   [reserved :8/bytes]
;   [payload :bytes])

; (->handshake 5 "BitTorrent protocol" (reserved-bytes) payload)

; (map->handshake {:len 5
;                  :protocol "BitTorrent protocol"
;                  :reserved (reserved-bytes)
;                  :payload payload})
