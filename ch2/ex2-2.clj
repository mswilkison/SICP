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

(defn average [x y]
  (/ (+ x y) 2))

(defn midpoint-segment [seg]
  (make-segment (average (x-point (start-segment seg))
                         (x-point (end-segment seg)))
                (average (y-point (start-segment seg))
                         (y-point (end-segment seg)))))
