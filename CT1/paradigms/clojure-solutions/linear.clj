(defn tensor-combiner [operation]
  (fn t-op [& tensors]
    (if (every? number? tensors)
      (apply operation tensors)
      (apply mapv t-op tensors))))

(def v+ (tensor-combiner +))
(def v- (tensor-combiner -))
(def v* (tensor-combiner *))
(def vd (tensor-combiner /))

(def m+ (tensor-combiner +))
(def m- (tensor-combiner -))
(def m* (tensor-combiner *))
(def md (tensor-combiner /))

(def t+ (tensor-combiner +))
(def t- (tensor-combiner -))
(def t* (tensor-combiner *))
(def td (tensor-combiner /))

(defn v*s [v scalar] (mapv #(* % scalar) v))
(defn m*s [mat scalar] (mapv #(v*s % scalar) mat))
;; :NOTE: можно было сделать общую функцию для v*s, m*s и m*v

(defn scalar [vec1 vec2] (reduce + (map * vec1 vec2)))
(defn vect [[x1 x2 x3] [y1 y2 y3]]
  [(- (* x2 y3) (* x3 y2))
   (- (* x3 y1) (* x1 y3))
   (- (* x1 y2) (* x2 y1))])

(defn transpose [mat] (apply mapv vector mat))
(defn m*v [mat vec] (mapv #(scalar % vec) mat))
(defn m*m [a b]
  (let [cols (transpose b)]
    (mapv (fn [row] (mapv #(scalar row %) cols)) a)))