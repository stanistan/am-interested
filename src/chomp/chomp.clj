(ns chomp.chomp
  (:require [chomp.match :as match]
            [chomp.utils :as utils]
            [chomp.cast :as cast])
  (:import [java.nio ByteBuffer]))

;; Casting and conversion helpers.........................................................

(def Bytes (Class/forName "[B"))

(cast/defcast Bytes ByteBuffer
  :forward (fn [bytes] (ByteBuffer/wrap (utils/pad-with-zeros 4 bytes)))
  :backward (fn [buffer] (.array buffer)))

(cast/defcast Bytes String
  :forward (fn [bs] (apply str (map (comp char int) bs)))
  :backward (fn [s] (.getBytes s)))

(cast/defcast ByteBuffer String
  :forward [Bytes String]
  :backward [Bytes ByteBuffer])

(cast/defcast ByteBuffer Long
  :forward (fn [bb] (long (.getInt bb)))
  :backward (fn [l] (.rewind (.putInt (java.nio.ByteBuffer/allocate 4) l))))

(cast/defcast Bytes Long
  :forward [ByteBuffer Long]
  :backward [ByteBuffer Bytes])

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
  (let [named? (some-fn symbol? keyword?)
        type? (some-fn symbol? #(= Class (type %)))]
    (match/destruct (utils/vectorify bit-spec)

      [[named? name] [keyword? spec] [type? cast]]
      (assoc (key-info spec) :name (keyword name) :cast (eval cast))

      [[named? name] [keyword? spec]]
      (assoc (key-info spec) :name (keyword name) :cast Bytes)

      [[keyword? spec] [type? cast]]
      (assoc (key-info spec) :cast (eval cast))

      [[keyword? spec]]
      (assoc (key-info spec) :cast Bytes))))

(defn bitstruct
  [& bit-specs]
  (let [s (mapv prep-conf bit-specs)]
    (if (every? identity s)
      (->BitStruct s (every? :name s) (every? :cast s))
      (throw (Exception. (str "Invalid bit-specs: " bit-specs))))))

(defn find-length
  [specs name]
  (when-let [val (:value (utils/find-in specs :name name))]
    (cast/cast-to Long val)))

(defn valid-length?
  [matched specified given]
  (cond (integer? specified) (= specified given)
        (keyword? specified) (= given (find-length matched specified))
        (nil? specified) true
        :else false))

(defn match-pair
  [matched [spec data]]
  (let [converted (convert-to-type spec data)]
    (when (valid-length? matched (:length spec) (count converted))
      (conj matched (assoc spec :value converted)))))

(defn encode*
  [struct data]
  (let [pairs (map vector (:specs struct) data)]
    (loop [matched [] pairs pairs]
      (if (seq pairs)
        (if-let [matched (match-pair matched (first pairs))]
          (recur matched (rest pairs))
          (throw (Exception. (str "encoding failed: " (first pairs)))))
        matched))))

(defn encode
  [struct & data]
  (let [data (match/destruct data
               [[map? data]]
               (let [names (map :name (:specs struct))]
                 (mapv data names))

               [& data]
               data)]
    (let [values (encode* struct data)]
      (byte-array (apply concat (map :value values))))))

(declare decode decode*)

(defn take-bytes
  "Returns [taken rest] or nil if length is invalid."
  [bytes length decoded from-end?]
  (let [[taker dropper] (if from-end? [take-last drop-last] [take drop])]
    (cond (keyword? length) (recur bytes (find-length decoded length) decoded from-end?)
          (integer? length) [(taker length bytes) (dropper length bytes)]
          (nil? length) [bytes []])))

(defn take-spec
  [specs & [from-end?]]
  (let [fns (if from-end? [last butlast] [first rest])]
    (mapv #(% specs) fns)))

(defn decode-taker
  [decoded specs bytes & [from-end?]]
    (let [;; specs and bytes
          [spec rest-specs] (take-spec specs from-end?)
          [taken rest-bytes] (take-bytes bytes (:length spec) decoded from-end?)
          nspec (assoc spec :value (byte-array taken))
          ;; continuation
          take-forward (fn [nspec specs bytes]
                            [(conj decoded nspec) specs bytes])
          take-reverse (fn [nspec specs bytes]
                            [(concat decoded (decode* specs bytes) [nspec] [] [])])
          continue (if from-end? take-reverse take-forward)]
      (continue nspec rest-specs rest-bytes)))

(defn error
  [& s]
  (Exception. (apply str s)))

(def decode-error (partial error "decoding failed: "))

(defn spec-with-value
  [spec bytes]
  (->> bytes
       (cast/cast-to (:cast spec))
       (assoc spec :value)))

(defn cast-all
  [specs]
  (mapv (fn [spec] (spec-with-value spec (:value spec))) specs))

(defn decode*
  "decode* takes a sequence of Specs, not a bitstruct"
  [specs bytes]
  (loop [decoded [] specs specs bytes bytes]
    (cond (and (empty? specs) (empty? bytes))
          (cast-all decoded)

          (or (empty? specs) (empty? bytes))
          (throw (decode-error {:specs specs :bytes bytes}))

          :else (let [length (-> specs first :length)
                      reverse? (and (not= 1 (count specs)) (nil? length))
                      [decoded specs bytes] (decode-taker decoded specs bytes reverse?)]
                  (recur decoded specs bytes)))))

(defn decode
  "Accepts a bitstruct (specs) and a series of bytes and returns
  a vector or a map if map? is truthy and the struct is named."
  [struct bytes & [map?]]
  (letfn [(name-val-pairs [maps] (map (fn [{n :name v :value}] [n v]) maps))]
    (if (and map? (not (:named? struct)))
      (throw (decode-error "cannot return map unless bitstruct is named."))
      (let [decoded (decode* (:specs struct) bytes)]
        (if map?
          (into {} (name-val-pairs decoded))
          (mapv :value decoded))))))

(def with-prefix (comp symbol str))

(defmacro defbitstruct
  [n & bit-specs]
  (let [struct (apply bitstruct bit-specs)]
    `(do
       (def ~n ~struct)
       (def ~(with-prefix "encode-" n) ~(partial encode struct))
       (def ~(with-prefix "decode-" n) ~(partial decode struct)))))
