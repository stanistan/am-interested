(ns am-interested.peer-protocol-test
  (:use midje.sweet
        am-interested.peer-protocol
        utils.type))

(defn str-repeat
  [n s]
  (apply str (repeat n s)))

(defmacro protocol-fact
  [doc form _arrow assertion]
  (let [[test-fn _] form]
    `(fact ~doc
       (let [encoded# ~form]
         (~test-fn encoded#) => ~assertion))))

(protocol-fact "about handshake"
  (handshake {:info-hash (str-repeat 20 "a")
              :peer-id (str-repeat 20 "b")})
  => [19 "BitTorrent protocol" reserved-zeros (str-repeat 20 "a") (str-repeat 20 "b")])

(protocol-fact "about keep-alive"
  (keep-alive) => [0])

(protocol-fact "about choke"
  (choke) => [1 (byte 0)])

(protocol-fact "about unchoke"
  (unchoke) => [1 (byte 1)])

(protocol-fact "about interested"
  (interested) => [1 (byte 2)])

(protocol-fact "about not-interested"
  (not-interested) => [1 (byte 3)])

(protocol-fact "about have"
  (have {:piece-index 10}) => [5 (byte 4) 10])

(protocol-fact "about bitfield"
  (bitfield {:bitfield (buffer [1 0 1 0])})
  => [5 (byte 5) (buffer [1 0 1 0])])

(protocol-fact "about request"
  (request {:index 10
            :begin 11
            :length 12})
  => [13 (byte 6) 10 11 12])

(protocol-fact "about piece"
  (piece {:index 10
          :begin 11
          :block (buffer [1 0 1 0])})
  => [13 (byte 7) 10 11 (buffer [1 0 1 0])])

(protocol-fact "about cancel"
  (cancel {:index 10
           :begin 11
           :length 12})
  => [13 (byte 8) 10 11 12])

(protocol-fact "about port"
  (port {:listen-port 6881}) => [3 (byte 9) 6881])
