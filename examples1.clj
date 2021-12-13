(ns clojure.examples.hello
    (:gen-class))

(defn func
    [x]
    (cond
        (number? x) (* x 2)
        (empty? x) nil
        (or (list? x) (vector? x)) (* (count x) 2)
        :else true
        )
    )

(defn test1
    [x]
    (let [y (+ x x)]
         (println y)
         (Math/pow y y))
    )

(defn vectar
    [v]
    (def x (get v 0))
    (def y (get v 2))
    (+ x y)
    )

(defn vv
    [v]
    (def f1 (nth v 0 nil))
    (def f3 (nth v 2 nil))
    (if (not-any? nil? [f1 f3])
        (+ f1 f3)
        )
    )

(defn func1
    [v]
    (conj v "<3")
    )

(defn func2
    []
    (def cities {:name "lalalala", :place ["somewhere"]})
    (def cities1 {:name "hoho", :place ["somewhere" "where"]})
    (count (get cities1 :place))
    )

(defn func3
    [book newAuthor]
    (let [newBook (conj (get book :authors) newAuthor)]
        (println newBook)
    )
    )

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler" :birth-year 1947 :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer" :authors [friedman, felleisen]})

(def books [cities wild-seed embassytown little-schemer])

(def authors #{china, felleisen, octavia, friedman})

(defn func4
    []
    (let [authorNames 
          (fn [book]
              (map :name (:authors book))
              )
          ]
         (set (apply concat (map authorNames books)))
         )
    )
;(println (func4))

(defn func7
    []
    (def livingAuthors (ref ()))
    (doseq [k authors]
           (println k)
           (if (nil? (:death-year k))
               (dosync
                   (alter livingAuthors conj k)
                   )
               )
           )
    (println "Living authors:" @livingAuthors)
    )
;(func7)

(defn func5
    [v]
    (let [v2 (fn[x] (nth x 1 nil))]
         (map v2 v)
         )
    )
;(println (func5 [[1 2] [3 4] [5 6] ["a" "b"] [7]]))

(defn func6
    [se elem]
    (if (contains? se elem)
        (do
            (disj se elem)
        )
        (do
            (conj se elem)
        )
        )
    )
;(println (func6 (set [1 2 3 4]) 2))

(defn sum-f
    [f g x]
    (+ (f x) (g x))
    )

(defn less-than
    [n]
    (fn [k] (< k n))
    )

(defn set->predicate
    [x]
    (fn [a-set] 
        (cond
            (empty? a-set) false
            (= (first a-set) x) true
            :else (recur (rest a-set))
            )
        )
    )
;(println (filter (set->predicate 2) [0 2 4 6]))

(defn pred-and
    [pred1 pred2]
    (fn [li] 
        (and (pred1 li) (pred2 li))
        )
    )
;(println (filter (pred-and (complement nil?) empty?) [[] '() nil {} #{}]))

(defn blank?
    [s]
    (or (nil? s) (empty? s))
    )

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler" :birth-year 1947 :death-year 2006})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def authors #{china, octavia, kay, dick, zelazny})

(def cities {:title "The City and the City" :authors #{china} :awards #{:locus, :world-fantasy, :hugo}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def lord-of-light {:title "Lord of Light", :authors #{zelazny} :awards #{:hugo}})
(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})
(def ysabel {:title "Ysabel", :authors #{kay}, :awards #{:world-fantasy}})
(def scanner-darkly {:title "A Scanner Darkly" :authors #{dick}})

(def books #{cities, wild-seed, lord-of-light, deus-irae, ysabel, scanner-darkly})

(defn has-award?
    [book award]
    (contains? (get book :awards) award)
    )
;(println (has-award? ysabel :world-fantasy))

(defn product
    [x]
    (if (empty? x)
        1
        (* (first x) (product (rest x)))
        )
    )

(defn seq-max
    [seq1 seq2]
    (< (count seq1) (count seq2))
    seq1
    seq2
    )

(defn ff
    [x]
    (* x 6)
    )
;(println (map ff [1 2 3 4]))
;(println (group-by #(if (even? %) :even :odd) [0 1 2 3 4 5]))

;(println (seq-max [2 4] [1 2 3]))

(defn sequence-contains?
    [x seqq]
    (cond
        (empty? seqq) false
        (= x (first seqq)) true
        :else (sequence-contains? x (rest seqq))
        )
    )
;(println (sequence-contains? 4 [1 2 3]))

(defn power
    [k n]
    (cond
        (= 0 n) 0
        (= 1 n) k
        :else (* k (power k (- n 1)))
        )
    )
;(println (power 2 3))

(defn sum-pan
    [a-seq]
    (let [sum-helper (fn [acc a-seq]
                         (if (empty? a-seq)
                             acc
                             (recur (+ acc (first a-seq)) (rest a-seq))
                             )
                         )]
          (sum-helper 0 a-seq)
        )
    )
(println (sum-pan [0 1 2 3 4]))

(defn minus
    ([x] (- x))
    ([x y] (- x y))
    )
(println (minus 1))
(println (minus 8 1))

(defn max1
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
        (reduce max1 (max1 x y) more)
        )
    )
(println (max1 1 2 3 5))