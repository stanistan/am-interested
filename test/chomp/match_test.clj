(ns chomp.match-test
  (:use midje.sweet chomp.match))

(fact "destruct"
  (destruct [:a]
    [thing]
    thing)
  => :a

  (destruct [:a]
    [[symbol? thing]]
    :found-symbol

    [[keyword? thing]]
    :found-keyword

    [thing]
    thing)
  => :found-keyword

  (destruct [1]
    [[#(< 1 %) thing]]
    :less-than-one

    [[#(> 1 %) thing]]
    :greater-than-one

    [[#(= 1 %) thing]]
    :is-one)
  => :is-one

  (destruct ['a :foo "string"]
    [[keyword? a] [keyword? b] c]
    :fail-1

    [any-1 [string? b] c]
    :fail-2

    [a [keyword? b] [keyword? c]]
    :fail-3

    [any-1 any-2 any-3]
    :pass)
  => :pass

  (destruct [{:a 1 :b 2}]
    [{:keys [a b]}]
    [a b])
  => [1 2]

  (destruct ['a :foo "string"]
    [[symbol? thing]]
    thing

    [[symbol? a] [keyword? b]]
    [:second a b]

    [[symbol? a] [keyword? b] [keyword? c]]
    [:third a b c]

    [[symbol? a] [keyword? b] [string? c]]
    :should-be)
  => :should-be

  (defn test-destruct
    [& c]
    (destruct c
      [[symbol? thing]]
      thing

      [[symbol? a] [keyword? b]]
      [:second a b]

      [[symbol? a] [keyword? b] [keyword? c]]
      [:third a b c]

      [[symbol? a] [keyword? b] [string? c]]
      :should-be))

  (test-destruct "a") => nil
  (test-destruct 'foo) => 'foo
  (test-destruct 'foo :bar) => [:second 'foo :bar]
  (test-destruct 'foo 'bar) => nil
  (test-destruct 'foo :bar :baz) => [:third 'foo :bar :baz]
  (test-destruct :foo :bar :baz) => nil
  (test-destruct 'a :b "c") => :should-be

  ; (destruct {:a 1 :b :k}
  ;   {:keys [[number? a] [keyword? b]]}
  ;   [a b])
  ; => [1 :k]

  (destruct [1 2 3 4]
    [[#(= 2 %) a] b c d]
    :two

    [[#(= 3 %) a] b & rest]
    :three

    [[#(= 1 %) a] & rest]
    [:one rest])
  => [:one [2 3 4]])
