(ns am-interested.peer-protocol
  (:use [chomp.chomp :only [defbitstruct]])
  (:require [am-interested.config :as config]
            [chomp.chomp :as chomp]))

(def pstr (config/consts :protocol))
(def pstrlen (long (count pstr))) ;; count returns an Integer.
(def reserved-zeros (byte-array (repeat 8 (byte 0))))

(defn handle-struct
  [struct & [defaults]]
  (fn [& [data]]
    ;; Data is a map, or it is nil and relies on defaults
    (if (or (map? data) (nil? data))
      (chomp/encode struct (merge defaults data))
      (chomp/decode struct data))))

(defbitstruct handshake-msg
  [pstrlen :byte Byte]
  [pstr :pstrlen/bytes String]
  [reserved :8/bytes chomp/Bytes]
  [info-hash :20/bytes String]
  [peer-id :20/bytes String])

(def handshake (handle-struct handshake-msg {:pstrlen pstrlen
                                             :pstr pstr
                                             :reserved reserved-zeros}))

(defbitstruct keep-alive-msg
  [len :4/bytes Long])

(def keep-alive (handle-struct keep-alive-msg {:len 0}))

(defbitstruct simple-id
  [len :4/bytes Long]
  [id :byte Byte])

(defn handle-simple-id
  [id]
  (handle-struct simple-id {:len 1 :id id}))

(def choke (handle-simple-id 0))
(def unchoke (handle-simple-id 1))
(def interested (handle-simple-id 2))
(def not-interested (handle-simple-id 3))

(defbitstruct have-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [piece-index :4/bytes Long])

(def have (handle-struct have-msg {:len 5 :id 4}))

(defbitstruct bitfield-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [bitfield :len/-1/bytes])

(defn bitfield
  [data]
  (if (map? data)
    (let [{field :field} data]
      (encode-bitfield-msg (inc (count field)) 5 field))
    (decode-bitfield-msg data)))

(defbitstruct request-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [index :4/bytes Long]
  [begin :4/bytes Long]
  [length :4/bytes Long])

(def request (handle-struct request-msg {:len 13 :id 6}))

(defbitstruct piece-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [index :4/bytes Long]
  [begin :4/bytes Long]
  [block :len/-9/bytes chomp/Bytes])

(defn piece
  [data]
  (if (map? data)
    (let [{:keys [index begin block]} data]
      (encode-piece-msg (+ 9 (count block)) 7 index begin block))
    (decode-piece-msg data)))

(def cancel (handle-struct request-msg {:len 13 :id 8}))

(defbitstruct port-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [listen-port :2/bytes Long])

(def port (handle-struct port-msg {:len 3 :id 9}))

(def messages
  {:handshake handshake
   :keep-alive keep-alive
   :choke choke
   :unchoke unchoke
   :interested interested
   :not-interested not-interested
   :have have
   :bitfield bitfield
   :request request
   :piece piece
   :cancel cancel
   :port port})
