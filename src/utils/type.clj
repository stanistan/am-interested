(ns utils.type
  (:import [java.nio ByteBuffer]))

(defn buffer
  [m]
  (ByteBuffer/wrap (byte-array (map byte m))))

(defn buff-count
  [buffer]
  (count (if (instance? ByteBuffer buffer) (.array buffer) buffer)))
