(ns chomp.utils)

(defn find-in
  "Finds a map in a vector which has key given the value."
  [coll key value]
  (letfn [(map-has-val [m v] (when (= (m key) v) m))
          (check [a b] (or (map-has-val b value) a))]
    (reduce check nil coll)))

(defn make-checks
  [sym bindings]
  (let [reducer (fn [acc form]
                    (if (vector? form)
                      (conj acc (into () (reverse form)))
                      acc))
        checks (reduce reducer () bindings)
        count-check `(= (count ~sym) ~(count bindings))]
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
