(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn enumerate-interval [low high]
  (if (> low high)
    ()
    (cons low (enumerate-interval (inc low) high))))

(defn accumulate [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (accumulate op initial (rest lst)))))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn flatmap [proc lst]
  (accumulate append () (map proc lst)))

(def empty-board '())

(defn make-position [row col]
  (cons row [col]))

(defn position-row [position]
  (first position))

(defn position-col [position]
  (last position))

(defn adjoin-position [row col positions]
  (append positions (list (make-position row col))))

(defn safe? [col positions]
  (defn attacks? [q1 q2]
    (or (= (position-row q1) (position-row q2))
        (= (abs (- (position-row q1) (position-row q2)))
           (abs (- (position-col q1) (position-col q2))))))
  (defn iter [q board]
    (or (empty? board)
        (and (not (attacks? q (first board)))
             (iter q (rest board)))))
  (let [kth-queen (nth positions (dec col))
        other-queens (filter (fn [k]
                               (not (= col (position-col k))))
                             positions)]
    (iter kth-queen other-queens)))

(defn queens [board-size]
 (defn queen-cols [k]
    (if (= k 0)
      (list empty-board)
      (filter
        (fn [positions] (safe? k positions))
        (flatmap
          (fn [rest-of-queens]
            (map (fn [new-row]
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (dec k))))))
  (queen-cols board-size))

(queens 8)
