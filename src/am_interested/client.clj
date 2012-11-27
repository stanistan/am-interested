(ns am-interested.client
  (:require [am-interested.utils :as utils]
            [bencode.bencode :as bencode]
            [clj-http.client :as request]))

(defn tracker-request
  "Makes a request to a tracker given a map of query-params. Exceptions
  are not thrown from the request. Req/res will be encoded and decoded using
  ISO-8859-1 instead of UTF-8."
  [url query-params]
  (with-redefs [clj-http.util/url-encode utils/iso-encode
                clj-http.util/url-decode utils/iso-decode]
    (request/get url {:query-params (utils/stringify-keys query-params)
                      :throw-exceptions false
                      :as "ISO-8859-1"})))

(defn get-info-hash
  [metainfo]
  (->> (:info metainfo)
       (bencode/encode)
       (utils/sha1)
       (map utils/unicode-string)
       (apply str)))

(defn torrent->tracker-params
  "Does not handle optional keys atm."
  [metainfo-map]
  (let [info-hash (get-info-hash metainfo-map)]
    {:info_hash info-hash
     :peer_id (utils/gen-id)
     :port 6881
     :uploaded 0
     :downloaded 0
     :left (get-in metainfo-map [:info :length])
     :compact 1}))

(defn request-torrent-info
  "Given the metainfo decoded from a .torrent file, makes a request
  to the tracker for torrent info."
  [metainfo]
  (tracker-request (:announce metainfo)
                   (torrent->tracker-params metainfo)))
