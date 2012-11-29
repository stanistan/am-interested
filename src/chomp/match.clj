(ns chomp.match
  (:require [chomp.utils :as utils]))

(defn make-count-check
  [sym bindings]
  (if-let [i (utils/index-of bindings '&)]
    `(>= (count ~sym) ~i)
    `(= (count ~sym) ~(count bindings))))

(defn make-checks
  [sym bindings]
  (let [reducer (fn [acc form]
                    (if (vector? form)
                      (conj acc (into () (reverse form)))
                      acc))
        checks (reduce reducer () bindings)
        count-check (make-count-check sym bindings)]
    `(and ~@checks ~count-check)))

(defn get-syms
  [bindings]
  (mapv #(if (vector? %) (second %) %) bindings))

(defn destructuring-let
  [c forms]
  (let [sym (gensym "init_")
        forms (partition 2 forms)
        domod (fn domod
                  [[[bindings body :as form] & rest]]
                  (when form
                    (let [checks (make-checks sym bindings)
                          syms (get-syms bindings)]
                      `(let [~syms ~sym]
                        (if ~checks
                          ~body
                          ~(domod rest))))))]
    `(let [~sym ~c]
      ~(domod forms))))

(defmacro destruct
  [c & forms]
  (destructuring-let c forms))
