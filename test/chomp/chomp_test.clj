(ns chomp.chomp-test
  (:use [midje.sweet]
        [chomp.chomp]))

(defn vectorize
  [check-on]
  (fn [sequence] (= (vec check-on) (vec sequence))))

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
  (key-info :abcs) => (spec :length nil :type :abc)
  (key-info :byte) => (spec :length 1 :type :byte)
  (key-info :1/bytes) => (spec :length 1 :type :byte)
  (key-info :2/bytes) => (spec :length 2 :type :byte)
  (key-info :$0/bytes) => (spec :length :$0 :type :byte)
  (key-info :name/bytes) => (spec :length :name :type :byte))

(fact "about prep-conf, this sets up the data structure with a name"
  (prep-conf :8/bytes) => (spec :length 8 :type :byte :cast Bytes)
  (prep-conf [:8/bytes]) => (spec :length 8 :type :byte :cast Bytes)
  (prep-conf ['reserved :8/bytes]) => (spec :length 8 :type :byte :name :reserved :cast Bytes)
  (prep-conf ['reserved :8/bytes 'String]) => (spec :length 8 :type :byte :name :reserved :cast String)
  (prep-conf [:a :8/bytes 'String]) => (spec :length 8 :type :byte :name :a :cast String)
  (prep-conf 'a) => nil
  (prep-conf ['n "abc"]) => nil
  (prep-conf ['a 'b]) => nil
  (prep-conf ['a 'b 'c]) => nil
  (prep-conf [:a :8/bytes :string]) => nil)

(def handshake (bitstruct
                [:len :byte Long]
                [:protocol :len/bytes String]
                [:reserved :8/bytes Bytes]
                [:payload :bytes Bytes]))

(fact "bitstruct works"
  (:named? handshake) => true
  (:specs handshake) => vector?
  (:specs handshake) => [(spec :type :byte :name :len :length 1 :cast Long)
                         (spec :type :byte :name :protocol :length :len :cast String )
                         (spec :type :byte :name :reserved :length 8 :cast Bytes)
                         (spec :type :byte :name :payload :length nil :cast Bytes)])

(fact "about casting to bytes protocol"
  (type->bytes "abcde") => (vectorize (byte-array (map byte [97 98 99 100 101])))
  (type->bytes 1) => (vectorize (byte-array [(byte 1)]))
  (type->bytes 23) => (vectorize (byte-array [(byte 23)]))
  (type->bytes (byte-array [(byte 23)])) => (vectorize (byte-array [(byte 23)])))

(fact "about casting to things from bytes"
  (bytes->type String (type->bytes "hello")) => "hello"
  (bytes->type Long (type->bytes 10)) => 10
  (bytes->type Bytes (byte-array (repeat 8 (byte 0))))
  => (vectorize (repeat 8 0)))

(fact "valid-length? makes sure that the given length satisfies the constraints"
  (def matched-stub [{:name :len :value (byte-array [(byte 3)])}])
  (valid-length? [] 3 3) => true
  (valid-length? [] 1 2) => false
  (valid-length? [] nil 10) => true
  (valid-length? [] "abc" :foo) => false
  (valid-length? [] 'a :b) => false
  (valid-length? matched-stub :len 3) => true
  (valid-length? matched-stub :len 4) => false
  (valid-length? matched-stub :foo 4) => false)

(fact "encode"
  (encode handshake 19 "BitTorrent protocol" (byte-array (repeat 8 (byte 0))) "foo")
  => (vectorize [19 66 105 116 84 111 114 114 101 110 116 32 112 114 111 116 111 99 111 108 0 0 0 0 0 0 0 0 102 111 111])

  (encode handshake 18 "BitTorrent protocol" (byte-array (repeat 8 (byte 0))) "foo")
  => (throws))

(fact "decode"
  (def enc (bitstruct
            [:len :byte Long]
            [:protocol :len/bytes String]
            [:payload :bytes String]
            [:eom :byte Long]))

  (def data [10 "abcdeabcde" "the rest of the thing" 1])

  (decode enc (apply encode enc data))
  => data

  (decode enc (apply encode enc data) :map)
  => {:len 10
      :protocol "abcdeabcde"
      :payload "the rest of the thing"
      :eom 1}

  (def unnamed (bitstruct [:byte Long]))
  (decode unnamed (encode unnamed 1) :as-map) => (throws))

(def foobar-struct (bitstruct
                    [:id :byte Long]
                    [:payload :bytes String]
                    [:eom :byte Long]))

(defbitstruct foobar
  [id :byte Long]
  [payload :bytes String]
  [eom :byte Long])

(fact "defbitstruct"
  foobar => foobar-struct
  (encode-foobar 1 "payload" 0) => (vectorize (encode foobar-struct 1 "payload" 0))
  (decode-foobar (encode-foobar 1 "payload" 0)) => [1 "payload" 0]
  (decode-foobar (encode-foobar 1 "payload" 0) :as-map)
  => {:id 1 :payload "payload" :eom 0})
