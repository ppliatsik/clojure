(ns clojure.examples.hello
    (:require [clojure.string :refer [capitalize]])
(:gen-class))

(def buyer (ref 100))
(def merchant (ref 0))
(def items (ref ()))
(def products {"pen" 25, "backpack" 40, "book" 35, "pencil" 10})

(defn printInfo
    []
    (println "Buyer:" @buyer)
    (println "Merchant:" @merchant)
    (println "Items:" @items)
    )

(defn buy
    [product]
    (def itemPrice (get products product))
    (if (nil? itemPrice)
        (println "Unknown product:" product)
        (do
            (println (capitalize product) "price:" itemPrice)
            (if (<= itemPrice @buyer)  
                (dosync 
                    (ref-set buyer (- @buyer itemPrice))
                    (ref-set merchant (+ @merchant itemPrice))
                    (alter items conj product)
                    )
                (println "Not enough money")
                )
            )
        )
    (printInfo)
    )

(printInfo)

(buy "pen")
(buy "backpack")
(buy "book")
(buy "pencil")
(buy "unknown")

(let [userInput (read-line)]
     (buy userInput)
     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cars {"bmw" 10000, "opel" 15000, "nissan" 20000})
(def budget 13000)
(def carsCanBuy (ref {}))

(defn checkCoupon
    [couponName]
    (def coupon {"20p" 0.8})
    (get coupon couponName)
    )

(defn buy
    [couponName]
    (def discount (checkCoupon couponName))
    (if (nil? discount)
        (do
            (doseq [[k v] cars]
                (if (>= budget v)
                    (dosync
                        (alter carsCanBuy conj {k v})
                        )
                    )
                )
            (println "No discount")
            )
        (do
            (doseq [[k v] cars]
                (def price (* discount v))
                (if (>= budget price)
                    (dosync
                        (alter carsCanBuy conj {k price})
                        )
                    )
                )
            (println "Discount")
            )
        )
    )

(buy "20p")
(println @carsCanBuy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn suit
    [card]
    
    (let [[_ snd] card]
       snd)
    )

(defn rank
    [card]
    
    (let [[first second] card]
         (cond
             (Character/isDigit first) first
             (= first \T) 10
             (= first \J) 11
             (= first \Q) 12
             (= first \K) 13
             (= first \A) 14
             :else nil
             )
         )
    )

(defn get-frequencies
    [hand]
    (vals (frequencies (let [h (fn [x] (rank x))]
         (map h hand)
         )
     )
    )
    )

(defn get-keys-frequencies
    [hand]
    (keys (frequencies (let [h (fn [x] (rank x))]
         (map h hand)
         )
     )
    )
    )

(defn get-frequencies-shape
    [hand]
    (vals (frequencies (let [h (fn [x] (suit x))]
         (map h hand)
         )
     )
    )
    )

(defn pair?
    [hand]

    (def p (get (frequencies (get-frequencies hand)) 2))
    (cond
        (or (nil? p) (> p 1)) false
        :else true
        )
    )

(defn two-pair?
    [hand]

    (def p (get (frequencies (get-frequencies hand)) 2))
    (cond
        (or (nil? p) (< p 2)) false
        :else true
        )
    )

(defn three-of-a-kind?
    [hand]
    (def p1 (get (frequencies (get-frequencies hand)) 3))
    (cond
        (nil? p1) false
        :else true
        )
    )

(defn four-of-a-kind?
    [hand]
    (def p2 (get (frequencies (get-frequencies hand)) 4))
    (cond
        (nil? p2) false
        :else true
        )
    )

(defn full-house?
    [hand]
    (and (pair? hand) (three-of-a-kind? hand))
    )

(defn flush?
    [hand]
    (def p3 (get (frequencies (get-frequencies-shape hand)) 5))
    (cond
        (nil? p3) false
        :else true
        )
    )

(defn straight? 
    [hand]
    (def p4 (get-keys-frequencies hand))
    (if (= (count p4) 5)
        (do
            (def p5 (sort p4))
            (def f (Character/digit (first p5) 10))
            (def l (Character/digit (last p5) 10))
            
            (if (= f (- l 4))
                true
                false
                )
            )
        false
        )
    )

(defn straight-flush? 
    [hand]
    (and (flush? hand) (straight? hand))
    )

(defn value 
    [hand]
    (cond
        (straight-flush? hand) 8
        (straight? hand) 7
        (flush? hand) 6
        (full-house? hand) 5
        (four-of-a-kind? hand) 4
        (three-of-a-kind? hand) 3
        (two-pair? hand) 2
        (pair? hand) 1
        :else 0
        )
    )

(println (value ["4H" "2H" "3H" "1H" "5H"]))