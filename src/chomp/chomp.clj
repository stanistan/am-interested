(ns chomp.chomp
  (:require [chomp.match :as match]
            [chomp.utils :as utils]
            [chomp.cast :as cast])
  (:import [java.nio ByteBuffer]))

;; Casting and conversion helpers.........................................................

(def Bytes (Class/forName "[B"))

(defn num->buffer
  [l num]
  (.rewind (.put (ByteBuffer/allocate l) num)))

(defn buffer->num
  [cast buffer]
  (let [bytes (utils/pad-with-zeros 4 (.array buffer))]
    (cast (.getInt (ByteBuffer/wrap bytes)))))

(cast/defcast Bytes ByteBuffer
  :forward (fn [bytes] (ByteBuffer/wrap bytes))
  :backward (fn [buffer] (.array buffer)))

(cast/defcast Bytes String
  :forward (fn [bytes] (apply str (map (comp char int) bytes)))
  :backward (fn [string] (.getBytes string)))

(cast/defcast ByteBuffer String
  :forward [Bytes String]
  :backward [Bytes ByteBuffer])

(cast/defcast ByteBuffer Short
  :forward (fn [buffer] (short (.getShort buffer)))
  :backward (fn [short] (.rewind (.putShort (ByteBuffer/allocate 2) short))))

(cast/defcast ByteBuffer Integer
  :forward (fn [buffer] (int (.getInt buffer)))
  :backward (fn [int] (.rewind (.putInt (ByteBuffer/allocate 4) int))))

(cast/defcast Bytes Integer
  :forward [ByteBuffer Integer]
  :backward [ByteBuffer Bytes])

(cast/defcast Bytes Short
  :forward [ByteBuffer Short]
  :backward [ByteBuffer Bytes])

(cast/defcast Byte Integer
  :forward (fn [byte] (int byte))
  :backward (fn [int] (byte int)))

(cast/defcast Byte Short
  :forward (fn [byte] (short byte))
  :backward (fn [short] (byte short)))

(cast/defcast Bytes Byte
  :forward first
  :backward (fn [byte] (byte-array [byte])))

(cast/defcast Integer Short
  :forward short
  :backward int)

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
  [spec data]
  (cast/cast-to Bytes (cast/cast-to (:cast spec) data)))

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

(defn error
  [& s]
  (Exception. (apply str s)))

(def decode-error (partial error "decoding failed: "))

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
      (assoc (key-info spec) :name (keyword name) :cast ByteBuffer)

      [[keyword? spec] [type? cast]]
      (assoc (key-info spec) :cast (eval cast))

      [[keyword? spec]]
      (assoc (key-info spec) :cast ByteBuffer))))

(defn bitstruct
  [& bit-specs]
  (let [s (mapv prep-conf bit-specs)]
    (if (every? identity s)
      (->BitStruct s (every? :name s) (every? :cast s))
      (throw (Exception. (str "Invalid bit-specs: " bit-specs))))))

(defn str-to-fn
  "Takes an arithmetic expression string and creates a function from it.
    Examples:
      -10 =>  #(- % 10)
      +1  =>  #(+ % 1)
      *2  =>  #(* % 2)
    Supports basic arithmetic functions."
  [[f & n :as s]]
  (let [f (symbol (str f))
        n (read-string (apply str n))
        ariths ['+ '- '* '/]]
    (when-not (utils/index-of ariths f)
      (throw (error "first piece needs to be an arithmetic fn: " ariths)))
    (when-not (integer? n)
      (throw (error "not a number: " n)))
    (eval `(fn [v#] (~f v# ~n)))))

(defn split-length
  [k]
  (let [[n trans] [(namespace k) (name k)]]
    (if n [(keyword n) (str-to-fn trans)] [k identity])))

(defn find-length
  [specs name]
  (let [[name trans] (split-length name)]
    (when-let [val (:value (utils/find-in specs :name name))]
      (trans (cast/cast-to Integer val)))))

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
      (cast/cast-to ByteBuffer (byte-array (apply concat (map :value values)))))))

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
      (let [bytes (if (instance? ByteBuffer bytes) (.array bytes) bytes)
            decoded (decode* (:specs struct) bytes)]
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
