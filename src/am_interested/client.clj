(ns am-interested.client
  (:use utils.string utils.fn)
  (:require [am-interested.utils :as utils]
            [bencode.bencode :as bencode]
            [clj-http.client :as request]
            [am-interested.config :as config]
            [clojure.string :as string]))

(def peer-id (utils/gen-id))

(defn tracker-request
  "Makes a request to a tracker given a map of query-params. Exceptions
  are not thrown from the request. Req/res will be encoded and decoded using
  ISO-8859-1 instead of UTF-8."
  [url query-params]
  (with-redefs [clj-http.util/url-encode utils/str-encode
                clj-http.util/url-decode utils/str-decode]
    (request/get url {:query-params (utils/stringify-keys query-params)
                      :throw-exceptions false
                      :as (config/consts :str-encoding)})))

(defn get-info-hash
  [minfo]
  (->> (get-in minfo [:original :info])
       (bencode/encode)
       (utils/sha1)
       (map utils/unicode-string)
       (apply str)))

(defn get-total-length
  [minfo]
  (reduce + (map :length (get-in minfo [:normalized :info :files]))))

(defn torrent->tracker-params
  "Does not handle optional keys atm."
  [minfo event]
  (let [info-hash (get-info-hash minfo)]
    {:info_hash info-hash
     :peer_id peer-id
     :port (config/opts :port)
     :event (name event)
     :uploaded 0
     :downloaded 0
     :left (get-total-length minfo)
     :compact 1}))

(defn to-ip-and-port
  [s]
  (let [[ip [a b]] (partition-all 4 (map int s))]
    {:ip (string/join "." (reverse ip))
     :port (+ b (* 256 a))}))

(defn prep-peers
  [peers]
  (apply-if (complement vector?) #(mapv to-ip-and-port (partition 6 %)) peers))

(defn request-torrent-info
  "Given the minfo decoded from a .torrent file, makes a request
  to the tracker for torrent info."
  [minfo event]
  (-> (get-in minfo [:normalized :announce])
      (tracker-request (torrent->tracker-params minfo event))
      (:body)
      (bencode/decode)
      (utils/keywordize-keys)
      (update-in [:peers] prep-peers)))
