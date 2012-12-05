(ns am-interested.minfo)

(defn clean-info
  [fileinfo]
  (let [n (:name fileinfo)]
    (-> fileinfo
        (dissoc :name :piece-length :pieces)
        (assoc :path [n]))))

(defn normalize-minfo
  [{:keys [info] :as metainfo}]
  (if-not (:files info)
    (assoc metainfo :info {:pieces (:pieces info)
                           :piece-length (:piece-length info)
                           :name "./"
                           :files [(clean-info info)]})
    metainfo))

(defrecord MetaInfo [normalized original])

(defn metainfo
  [info]
  (->MetaInfo (normalize-minfo info) info))
