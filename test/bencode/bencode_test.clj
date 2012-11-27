(ns bencode.bencode-test
  (:refer-clojure :exclude [num list])
  (:use [bencode.bencode]
        [bencode.test-util]
        [midje.sweet]))

(def byte? (partial instance? java.lang.Byte))

(fact "sequence of bytes from a string"
      (byteseq "ascii") => (allfn seq? (partial every? byte?)))

(fact "about simple unfold"
      (unfold #(= % :stop-unfold)
              (fn [xs]
                (if (seq xs)
                  [(first xs) (rest xs)]
                  :stop-unfold))
              [1 2 3])
      => [[1 2 3] ()])

(fact "about read-chars"
      (read-chars [\1 \3 \3 \7]) => 1337
      (read-chars [\- \1 \3 \3 \7]) => -1337
      (read-chars [\a \b \c]) => 'abc)

(fact "about with-sign"
      (with-sign [\1]) => [\+ [\1]]
      (with-sign [\- \1]) => [\- [\1]])

(fact "about digit parsing"
      (digit [\1 \f \o \o]) => [\1 [\f \o \o]]
      (digit [\1]) => [\1 nil]
      (digit [\f \o \o]) => nil?)

(fact "about number parsing"
      (num [\1 \2 \3 \f \o \o]) => [123 [\f \o \o]]
      (num [\- \1 \2 \3 \f \o \o]) => [-123 [\f \o \o]]
      (num [\f \1 \2 \3]) => nil?)

(fact "about integer decoding"
      (decode "i123e") => 123
      (decode "i1337ei123e") => 1337
      (decode "i-1337e") => -1337)

(fact "about poorly-formed integer decoding"
      (decode "i123xe") => nil?)

(fact "about list decoding"
      (decode "li1ei2ei3ee") => [1 2 3]
      (decode "li-1ei-2ei-3ee") => [-1 -2 -3]
      (decode "li1ei2ei3eexxx") => [1 2 3]
      (decode "ld1:ai0eee") => [{"a" 0}])

(fact "about poorly-formed list decoding"
      (decode "li1ei2ei3exe") => nil?)

(fact "about string decoding"
      (decode "3:foo") => "foo"
      (decode "3:foobar") => "foo")

(fact "about poorly-formed string decoding"
      (decode "3x:foo") => nil?)

(fact "about dictionary decoding"
      (decode "d1:ai0e1:bi1ee") => {"a" 0 "b" 1}
      (decode "d1:ali0ei1ei2eee") => {"a" [0 1 2]})

(fact "about string encoding"
      (-encode "foobar") => (map char->byte "6:foobar")
      (-encode "foo bar") => (map char->byte "7:foo bar"))

(fact "about integer encoding"
      (-encode 1337) => (map char->byte "i1337e")
      (-encode -123) => (map char->byte "i-123e"))

(fact "about list encoding"
      (-encode [1 2 3]) => (map char->byte "li1ei2ei3ee")
      (-encode [1 [2 [3]]]) => (map char->byte "li1eli2eli3eeee")
      (-encode ["foo" "bar"]) => (map char->byte "l3:foo3:bare"))

(fact "about map encoding"
      (-encode {"a" 1}) => (map char->byte "d1:ai1ee")
      (-encode {:a 1}) => (map char->byte "d1:ai1ee")
      (-encode {:a {:b {:c "foo"}}}) => (map char->byte "d1:ad1:bd1:c3:fooeee"))
