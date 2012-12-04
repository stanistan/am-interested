(ns chomp.utils-test
  (:use midje.sweet chomp.utils))

(fact "about find-in, it looks for maps in vectors based on a key value match"
  (find-in [{:a 1}] :a 1) => {:a 1}
  (find-in [{:a 1 :b 2} {:a 2}] :a 1) => {:a 1 :b 2})

(fact "index-of returns the index or nil"
  (index-of [1 2 3 4] 2) => 1
  (index-of [:a :b :c] :d) => nil)

(fact "padding bytes"
  (map int (zero-padd-byte 4 4)) => [0 0 0 4]
  (map int (zero-padd-byte 4 10)) => [0 0 1 0]
  (map int (zero-padd-byte 4 101)) => [0 1 0 1]
  (map int (zero-padd-byte 4 15)) => [0 0 1 5])
