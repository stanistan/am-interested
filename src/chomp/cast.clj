(ns chomp.cast)

(defmulti cast-to*
  (fn [cast data] [cast (type data)]))

(defn cast-to [cast data]
  (if (= (type data) cast)
    data
    (cast-to* cast data)))

(defn make-cast-method
  [first second fn]
  `(defmethod cast-to* [~first ~second]
     [_# data#]
     (~fn data#)))

(defmacro defcast
  "(defcast String Long
     :forward (fn [s] (read-string s))
     :backward (fn [i] (str i)))

   (cast-to String 1) => \"1\"
   (cast-to String \"1\") => \"1\"
   (cast-to Long \"1\") => 1
   (cast-to Long 1) => 1"
  [first second & {:keys [forward backward]}]
  (when-not (and forward backward)
    (throw (Exception. "defcast requires :forward and :backward.")))
  `(do
     ~(make-cast-method first second backward)
     ~(make-cast-method second first forward)))