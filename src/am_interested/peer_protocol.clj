(ns am-interested.peer-protocol
  (:use chomp.chomp)
  (:require [am-interested.config :as config]))

(def pstr (config/consts :protocol))
(def pstrlen (count pstr))

(defbitstruct handshake-msg
  [pstrlen :byte Byte]
  [pstr :pstrlen/bytes String]
  [reserved :8/bytes]
  [info-hash :20/bytes String]
  [peer-id :20/bytes String])

(defn handshake
  [m]
  (encode-handshake-msg (merge {:pstrlen pstrlen :pstr pstr} m)))

(defbitstruct keep-alive-msg
  [len :4/bytes Long])

(def keep-alive #(encode-keep-alive-msg 0))

(defbitstruct simple-id
  [len :4/bytes Long]
  [id :byte Byte])

(def choke #(encode-simple-id 1 0))
(def unchoke #(encode-simple-id 1 1))
(def interested #(encode-simple-id 1 2))
(def not-interested #(encode-simple-id 1 3))

(defbitstruct have-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [piece-index :4/bytes Long])

(defn have
  [index]
  (encode-have-msg 5 4 index))

(defbitstruct bitfield-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [bitfield :len/-1/bytes])

(defn bitfield
  [field]
  (encode-bitfield-msg (inc (count field)) 5 field))

(defbitstruct request-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [index :4/bytes Long]
  [begin :4/bytes Long]
  [length :4/bytes Long])

(def request (partial encode-request-msg 13 6))

(defbitstruct piece-msg
  [len :4/bytes Long]
  [id :byte Byte]
  [index :4/bytes Long]
  [begin :4/bytes Long]
  [block :len/-9/bytes Bytes])

(defn piece
  [index begin block]
  (encode-piece-msg (+ 9 (count block)) 7 index begin block))

(def cancel (partial encode-request-msg 13 8))

(defbitstruct port-msg
  [len :4/bytes Long]
  [id :byte Bytes]
  [listen-port :2/bytes Long])

(defn port
  [port]
  (encode-port-msg 3 (byte-array [(byte 9)]) port))

(def messages
  {:keep-alive keep-alive
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
