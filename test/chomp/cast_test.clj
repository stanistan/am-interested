(ns chomp.cast-test
  (:use chomp.cast midje.sweet))

(fact "about simple cast-to"
      (defcast String Long
        :forward (fn [s] (read-string s))
        :backward (fn [i] (str i)))

      (cast-to String 1) => "1"
      (cast-to String "1") => "1"
      (cast-to Long "1") => 1
      (cast-to Long 1) => 1)

(defrecord Foo [x])

(fact "about record cast-to"

      (defcast Foo String
        :forward (fn [f] (:x f))
        :backward (fn [s] (->Foo s)))

      (cast-to Foo "foo") => (->Foo "foo")
      (cast-to Foo (->Foo "foo")) => (->Foo "foo")
      (cast-to String (->Foo "foo")) => "foo"
      (cast-to String "foo") => "foo")

;; Midje cannot capture compile-time exceptions thrown from macros.
;; All expansion happens at the same time.
;; (fact "about cast assertion"
;;       (defcast String Long) => (throws))
