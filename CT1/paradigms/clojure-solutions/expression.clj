(defn constant [value]
  (constantly value))
(defn variable [name]
  (fn [vars] (get vars name)))
(defn operation [f]
  (fn [& args]
    (fn [vars] (apply f (map #(% vars) args)))))
(defn safe-divide
  ([x] (/ (double x)))
  ([x & more] (reduce (fn [a b] (/ (double a) (double b))) (double x) more)))

(def add (operation +))
(def subtract (operation -))
(def multiply (operation *))
(def divide (operation safe-divide))
(defn negate [a]
  (multiply (constant -1) a))

(def atan (operation #(Math/atan (double %))))
(def atan2 (operation (fn [y x] (Math/atan2 (double y) (double x)))))
(def arcTan atan)
(def arcTan2 atan2)

(defn check-args [expected args]
  (if (= (count args) expected)
    args nil))

(def operations
  {'+      (fn [args] (apply add args))
   '-      (fn [args]
             (cond
              (empty? args) (constant 0)
              (= 1 (count args)) (negate (first args))
              :else (apply subtract args)))
   '*      (fn [args] (apply multiply args))
   '/      (fn [args] (apply divide args))
   'negate (fn [args] (check-args 1 args) (negate (first args)))
   'atan   (fn [args] (check-args 1 args) (atan (first args)))
   'atan2  (fn [args] (check-args 2 args) (apply atan2 args))})

(defn parse [expr]
  (cond
   (number? expr) (constant expr)
   (symbol? expr) (variable (str expr))
   (sequential? expr)
   (let [op-sym (first expr)
         args (map parse (rest expr))
         op-fn (get operations op-sym)]
     (if op-fn
       (op-fn args)
       (constant Double/NaN)))
   :else (constant Double/NaN)))

(defn parseFunction [s]
  (parse (read-string s)))