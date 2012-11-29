(ns chomp.chomp-test
  (:use [midje.sweet]
        [chomp.chomp]))

(defn vectorize
  [check-on]
  (fn [byte-array] (= (vec check-on) (vec byte-array))))

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
  (prep-conf :8/bytes) => {:length 8 :type :byte}
  (prep-conf [:8/bytes]) => {:length 8 :type :byte}
  (prep-conf ['reserved :8/bytes]) => {:length 8 :type :byte :name :reserved}
  (prep-conf ['reserved :8/bytes :string]) => {:length 8 :type :byte :name :reserved :cast :string}
  (prep-conf 'a) => nil
  (prep-conf ['n "abc"]) => nil
  (prep-conf ['a 'b]) => nil
  (prep-conf ['a 'b 'c]) => nil)

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

(fact "Byteable protocol"
  (to-bytes "abcde") => (vectorize (byte-array (map byte [97 98 99 100 101])))
  (to-bytes 1) => (vectorize (byte-array [(byte 1)]))
  (to-bytes 23) => (vectorize (byte-array [(byte 23)]))
  (to-bytes (byte-array [(byte 23)])) => (vectorize (byte-array [(byte 23)])))
