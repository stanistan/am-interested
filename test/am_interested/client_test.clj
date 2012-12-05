(ns am-interested.client-test
  (:use [midje.sweet]
        [am-interested.client]))

(def default-peer
  {:ip "97.98.99.100" :port 25958})

(fact "about to-ip-and-port"
  (to-ip-and-port [1 2 3 4 0 0]) => {:ip "1.2.3.4" :port 0}
  (to-ip-and-port [1 2 3 4 2 3]) => {:ip "1.2.3.4" :port 515}
  (to-ip-and-port [\a \b \c \d \e \f]) => default-peer)

(fact "about prep-peers"
  (prep-peers []) => []
  (prep-peers {}) => []
  (prep-peers "abcdef") => [default-peer]
  (prep-peers "abcdefabcdef") => [default-peer default-peer])
