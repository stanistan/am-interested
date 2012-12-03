(ns chomp.chomp
  (:require [chomp.match :as match]
            [chomp.utils :as utils]
            [chomp.cast :as cast]))

;; Casting and conversion helpers.........................................................

(def Bytes (Class/forName "[B"))

(cast/defcast Bytes String
  :forward (fn [bs] (apply str (map (comp char int) bs)))
  :backward (fn [s] (.getBytes s)))

(cast/defcast Bytes Long
  :forward (fn [bs] (int (first bs)))
  :backward (fn [l] (byte-array [(byte l)])))

(defn type->bytes
  [data]
  (cast/cast-to Bytes data))

(defn bytes->type
  [type bytes]
  (cast/cast-to type bytes))

(defmulti convert-to-type
  (fn [spec data]
    (:type spec)))

(defmethod convert-to-type :byte
  [_ data]
  (cast/cast-to Bytes data))

;; Spec/Struct typing.....................................................................

(defrecord Spec [name length type cast])

(defn spec [& {:keys [name length type cast]}]
  (->Spec name length type cast))

(defrecord BitStruct [specs named? castable?])

;; Helpers................................................................................

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
    (spec :length (if (plural? type) length 1)
          :type (keyword (singularize type)))))

(defn prep-conf
  [bit-spec]
  (let [named? #(or (symbol? %) (keyword? %))]
    (match/destruct (utils/vectorify bit-spec)

      [[named? name] [keyword? spec] [symbol? cast]]
      (assoc (key-info spec) :name (keyword name) :cast (eval cast))

      [[named? name] [keyword? spec]]
      (assoc (key-info spec) :name (keyword name))

      [[keyword? spec]]
      (key-info spec))))

(defn bit-struct*
  [& bit-specs]
  (let [s (mapv prep-conf bit-specs)]
    (->BitStruct s (every? :name s) (every? :cast s))))

(defmacro bit-struct
  [n & bit-specs]
  (let [struct (apply bit-struct* bit-specs)]
    `(def ~n ~struct)))

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
  (let [pairs (map vector (:specs specs) data)]
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

(declare decode decode*)

(defn take-bytes
  "Returns [taken rest] or nil if length is invalid."
  [bytes length decoded & [from-end?]]
  (let [[taker dropper] (if from-end? [take-last drop-last] [take drop])]
    (cond (integer? length) [(taker length bytes) (dropper length bytes)]
          (keyword? length) (recur bytes
                                   (:value (utils/find-in decoded :name length))
                                   decoded
                                   from-end?)
          (nil? length) [bytes []])))

(defn forward-take
  [decoded [{:keys [length cast] :as spec} & specs] bytes]
  (let [[taken rest] (take-bytes bytes length decoded)
        casted (cast/cast-to cast (byte-array taken))
        decoded (conj decoded (assoc spec :value casted))]
    [decoded specs rest]))

(defn reverse-take
  [decoded specs bytes]
  (let [{:keys [length cast] :as spec} (last specs)
        specs (butlast specs)
        [taken rest] (take-bytes bytes length decoded :from-end)
        casted (cast/cast-to cast (byte-array taken))]
    (concat decoded (decode* specs rest) [(assoc spec :value casted)])))

(defn decode*
  "decode* takes a sequence of Specs, not a bit-struct"
  [specs bytes]
  (let [error (fn [specs bytes]
                (Exception. (str "decoding failed: "
                                 {:specs specs :bytes bytes})))]
    (loop [decoded [] specs specs bytes bytes]
      (cond (and (empty? specs) (empty? bytes))
            decoded

            (or (empty? specs) (empty? bytes))
            (throw (error specs bytes))

            :else
            (let [length (-> specs first :length)
                  [decoded specs bytes] (if (and (not= 1 (count specs)) (nil? length))
                                          (reverse-take decoded specs bytes)
                                          (forward-take decoded specs bytes))]
              (recur decoded specs bytes))))))

(defn decode
  "Accepts a bit-struct (specs) and a series of bytes and returns
  a vector or a map if map? is truthy and the struct is named."
  [struct bytes & [map?]]
  (when-not (:castable? struct)
    (throw (Exception. (str "decoding failed: bit-struct not castable."))))
  (decode* (:specs struct) bytes))


;; (decode handshake (encode handshake 5 "foooo" "other thing" "last thing"))
;; => [5 "foooo" "other thing" "last thing"]

; [len :10/bytes]


; { :named
;   :specs [{:type :byte
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
