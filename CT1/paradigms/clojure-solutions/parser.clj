; This file should be placed in clojure-solutions
; You may use it via (load-file "parser.clj")

(defn -return "Creates result" [value tail] [value tail])
(def -valid? "Checks whether result is valid" boolean)
(def -value "Returns result's value" first)
(def -tail "Returns result's tail" second)


(defn _empty
  "Creates parser that returns value and leaves input untouched"
  [value] (partial -return value))

(defn _char
  "Returns first character of input iff it matches predicate"
  [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))

(defn _map
  "Applies function to the result"
  [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))

(defn _combine
  "Combines results of parsers a and b using function f"
  [f a b]
  (fn [input]
    (let [ar ((force a) input)]
      (if (-valid? ar)
        ((_map (partial f (-value ar)))
         ((force b) (-tail ar)))))))

(defn _either
  "Returns first valid result of parsers a and b"
  [a b]
  (fn [input]
    (let [ar ((force a) input)]
      (if (-valid? ar) ar ((force b) input)))))

(defn _parser
  "Parses whole input using specified parser"
  [parser]
  (fn [input]
    (-value ((_combine (fn [v _] v) parser (_char #{\u0001})) (str input \u0001)))))
(mapv (_parser (_combine str (_char #{\a \b}) (_char #{\x}))) ["ax" "ax~" "bx" "bx~" "" "a" "x" "xa"])



(defn +char
  "Returns first character of input iff it is among specified chars"
  [chars]
  (_char (set chars)))

(defn +char-not
  "Returns first character of input iff it is not among specified chars"
  [chars]
  (_char (comp not (set chars))))

(defn +map
  "Applies f to parser result"
  [f parser]
  (comp (_map f) parser))

(def +ignore
  "Returns 'ignore"
  (partial +map (constantly 'ignore)))

(defn iconj
  "Adds value to the coll if it is not 'ignore"
  [coll value]
  (if (= value 'ignore) coll (conj coll value)))

(defn +seq
  "Returns result of multiple consecutive parsers as vector"
  [& parsers]
  (reduce (partial _combine iconj) (_empty []) parsers))

(defn +seqf
  [f & parsers]
  "Applies function to the result of consecutive parsers"
  (+map (partial apply f) (apply +seq parsers)))

(defn +seqn
  "Takes nth of the results of consecutive parsers"
  [n & parsers]
  (apply +seqf (fn [& vs] (nth vs n)) parsers))

(defn +or
  "Returns result of the first successful parser"
  [parser & parsers]
  (reduce (partial _either) parser parsers))

(defn +opt
  "Returns result of the parser if it is successful and nil otherwise"
  [parser]
  (+or parser (_empty nil)))

(defn +star
  "Collects results of zero-or-more applications of parser"
  [parser]
  (letfn [(rec [] (+or (+seqf cons parser (delay (rec))) (_empty ())))]
    (rec)))

(defn +plus
  "Collects results of one-or-more applications of parser"
  [parser]
  (+seqf cons parser (+star parser)))

(defn +str
  "Converts result of parser to string"
  [parser]
  (+map (partial apply str) parser))

(def +parser
  "Parses whole input using specified parser"
  _parser)


(defn- +rules
  "Helper function for defparser"
  [defs]
  (cond
    (empty? defs) ()
    (seq? (first defs)) (let [[[name args body] & tail] defs]
                          (cons
                            {:name name :args args :body body}
                            (+rules tail)))
    :else (let [[name body & tail] defs]
            (cons
              {:name name :args [] :body body :plain true}
              (+rules tail)))))

(defmacro defparser
  "Defines parser"
  [name & defs]
  (let [
        rules ((fn rec [defs]
                 (cond
                   (empty? defs) ()
                   (seq? (first defs)) (let [[[name args body] & tail] defs]
                                         (cons
                                           {:name name :args args :body body}
                                           (rec tail)))
                   :else (let [[name body & tail] defs]
                           (cons
                             {:name name :args [] :body body :plain true}
                             (rec tail)))))
               defs)
        plain (set (map :name (filter :plain rules)))]
    (letfn [(rule [{name :name, args :args, body :body}] `(~name ~args ~(convert body)))
            (convert [value]
              (cond
                (seq? value) (map convert value)
                (char? value) `(+char ~(str value))
                (contains? plain value) `(~value)
                :else value))]
      `(def ~name
         (letfn ~(mapv rule rules)
           (+parser (~(:name (last rules)))))))))
