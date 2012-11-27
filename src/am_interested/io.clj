(ns am-interested.io
  (:require [bencode.bencode :as bencode]
            [am-interested.utils :as utils]))

(defn read-torrent
  [path-to-file]
  (-> (bencode/file-stream path-to-file)
    (bencode/decode)
    (utils/keywordize-keys)))
