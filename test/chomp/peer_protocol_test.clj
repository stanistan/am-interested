(ns am-interested.peer-protocol-test
  (:use midje.sweet
        am-interested.peer-protocol))

(defn str-repeat
  [n s]
  (apply str (repeat n s)))

(fact "about handshake"
      (handshake {:info-hash (str-repeat 20 "a")
                  :peer-id (str-repeat 20 "b")}))
