(ns chomp.utils-test
  (:use midje.sweet chomp.utils))

(fact "about find-in, it looks for maps in vectors based on a key value match"
  (find-in [{:a 1}] :a 1) => {:a 1}
  (find-in [{:a 1 :b 2} {:a 2}] :a 1) => {:a 1 :b 2})

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
  (test-destruct 'a :b "c") => :should-be)
