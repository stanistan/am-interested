(ns am-interested.utils-test
  (:use [am-interested.utils]
        [midje.sweet]))

(fact "about map-keys fns"
      (keywordize-keys {"foo bar" {"baz" 1}}) => {:foo-bar {:baz 1}}
      (stringify-keys {:foo-bar {:baz 1}}) => {"foo bar" {"baz" 1}})
