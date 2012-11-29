(ns chomp.chomp-test
  (:use [midje.sweet]
        [chomp.chomp]))

(fact "singularize removes the trailing S from words"
  (singularize "facts") => "fact"
  (singularize "fact") => "fact")

(fact "parse-prefix does checks on namespace of the keyword"
  (parse-prefix "") => nil
  (parse-prefix "123") => 123
  (parse-prefix "abc") => :abc
  (parse-prefix "$0") => :$0
  (parse-prefix "0") => 0)

(fact "key info parses keywords into more a data structure"
  (key-info :abcs) => {:length nil :type :abc}
  (key-info :byte) => {:length 1 :type :byte}
  (key-info :1/bytes) => {:length 1 :type :byte}
  (key-info :2/bytes) => {:length 2 :type :byte}
  (key-info :$0/bytes) => {:length :$0 :type :byte}
  (key-info :name/bytes) => {:length :name :type :byte})

(fact "about prep-conf, this sets up the data structure with a name"
  (prep-conf :8/bytes) => {:length 8 :type :byte :name nil}
  (prep-conf [:8/bytes]) => {:length 8 :type :byte :name nil}
  (prep-conf ['reserved :8/bytes]) => {:length 8 :type :byte :name :reserved})

(fact "named? makes sure every map in a seq has a key :name"
  (named? [{:name true} {:name "a"}]) => true
  (named? []) => true
  (named? [{}{:name "something"}]) => false)

(fact "bit-struct works"
  (bit-struct handshake
    [len :byte]
    [protocol :len/bytes]
    [reserved :8/bytes]
    [payload :bytes])

  (:named? handshake) => true
  (:data handshake) => vector?
  (:data handshake) => [{:type :byte :name :len :length 1}
                        {:type :byte :name :protocol :length :len}
                        {:type :byte :name :reserved :length 8}
                        {:type :byte :name :payload :length nil}])
