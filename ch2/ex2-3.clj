(defn make-point [x y]
  (cons x [y]))

(defn x-point [point]
  (first point))

(defn y-point [point]
  (last point))

(defn make-segment [start end]
  (cons start [end]))

(defn start-segment [seg]
  (first seg))

(defn end-segment [seg]
  (last seg))

(defn make-rect [top-right top-left
                 bottom-left bottom-right]
  (cons top-right [top-left bottom-left bottom-right]))

(def rec (make-rect (make-point 1 1)
           (make-point -1 1)
           (make-point -1 -1)
           (make-point 1 -1)))

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn length [rect]
  (abs (- (x-point (first rect))
          (x-point (first (rest rect))))))

(defn width [rect]
  (abs (- (y-point (first rect))
          (y-point (last rect)))))

(defn perim [rect]
  (* 2 (+ (length rect) (width rect))))

(defn area [rect]
  (* (length rect) (width rect)))
