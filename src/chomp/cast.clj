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

(defn make-casts
  [first second {:keys [forward backward]}]
  `(do
     ~(when backward (make-cast-method first second backward))
     ~(when forward (make-cast-method second first forward))))

(defn handle-through
  [first second through]
  (when through
    {:forward (vec (concat through [second]))
     :backward (vec (concat (reverse through) [first]))}))

(defmacro defcast
  "(defcast String Integer
     :forward (fn [s] (read-string s))
     :backward (fn [i] (str i)))

   (cast-to String 1) => \"1\"
   (cast-to String \"1\") => \"1\"
   (cast-to Integer \"1\") => 1
   (cast-to Integer 1) => 1"
  [first second & {:keys [forward backward through] :as casts}]
  (when-not (or forward backward through)
    (throw (Exception. "defcast requires :forward, :backward, or :through.")))
  (let [casts (or (handle-through first second through) casts)]
    (make-casts first second casts)))
