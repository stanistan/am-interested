(ns socklet.socket
  (:refer-clojure :exclude [read])
  (:import [java.net ServerSocket Socket])
  (:require [socklet.event :as event]
            [socklet.utils :as utils]))

(defn create-server
  [port]
  (ServerSocket. port))

(defn close-socket
  [socket]
  (.close socket))

(defn accept
  [server]
  (.accept server))

(defn create-socket
  [host port]
  (Socket. host port))

(defn get-streams
  "Returns a [input output] stream pair."
  [socket]
  [(.getInputStream socket) (.getOutputStream socket)])

(defn read-available
  [input-stream]
  (.available input-stream))

(defn write
  "Writes a string to an output stream."
  [stream string]
  (.write stream (.getBytes string)))

(defn read
  "Reads a byte from an input stream."
  [stream]
  (.read stream))

(defn read-byte-stream
  "Reads a stream of bytes into a byte array until no more
  bytes are available to be read."
  [input]
  (loop [bytes []]
    (if (not= 0 (read-available input))
      (let [b (read input)]
        (recur (conj bytes b)))
      (if (empty? bytes)
        (recur bytes)
        bytes))))

(defn read-and-handle-byte-stream
  [input f]
  (f (read-byte-stream input)))

(def listen
  (partial event/listen-for read-and-handle-byte-stream))

(defn server-listen
  [server handler]
  (future
    (utils/repeatedly-call #(handler (accept server)))))
