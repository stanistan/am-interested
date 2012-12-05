(ns am-interested.minfo-test
  (:use am-interested.minfo midje.sweet))

(def single
  {:info
    {:name "abcdefg"
     :length 100
     :pieces "somepieces"
     :piece-length 10}})

(def multiple
  {:info
    {:name "./"
     :pieces "somepieces"
     :piece-length 10
     :files [{:path ["abcdefg"] :length 100}]}})

(fact "about normalizing file info"
  (normalize-minfo single) => multiple
  (normalize-minfo multiple) => multiple)
