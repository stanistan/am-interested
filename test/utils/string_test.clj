(ns utils.string-test
  (:use midje.sweet utils.string))

(fact "about padding strings"
  (str-padd "abc" 0 10) => "0000000abc"
  (str-padd "1" 0 1) => "1")
