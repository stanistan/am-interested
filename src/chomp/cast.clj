(ns chomp.cast)

(defmulti cast-to*
  (fn [cast data] [cast (type data)]))

(defn cast-to [cast data]
  (if (instance? cast data)
    data
    (cast-to* cast data)))

(defn make-cast-method
  [first second fn]
  `(defmethod cast-to* [~first ~second]
     [_# data#]
     (let [fn# ~fn
           reducer# (fn [acc# cast#]
                      (cast-to cast# acc#))
           thread-casts# (fn [casts#]
                           (reduce reducer# data# fn#))]
       (if (fn? fn#)
         (fn# data#)
         (thread-casts# data#)))))

(defmacro defcast
  "(defcast String Integer
     :forward (fn [s] (read-string s))
     :backward (fn [i] (str i)))

   (cast-to String 1) => \"1\"
   (cast-to String \"1\") => \"1\"
   (cast-to Integer \"1\") => 1
   (cast-to Integer 1) => 1"
  [first second & {:keys [forward backward]}]
  (when-not (and forward backward)
    (throw (Exception. "defcast requires :forward and :backward.")))
  `(do
     ~(make-cast-method first second backward)
     ~(make-cast-method second first forward)))
