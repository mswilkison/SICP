(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 (/ 1 2)))

(defn first-denomination [items]
  (first items))

(defn except-first-denomination [items]
  (rest items))

(defn no-more? [items]
  (empty? items))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values))))

(cc 100 us-coins)
(cc 100 uk-coins)

; The order of the input list coin-values has no affect
; because cc recursively evaluates every possible sublist
