; Exercise 2.46
(defn make-vect [x-cor y-cor]
  (cons x-cor [y-cor]))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (first (rest v)))

(defn bi-op-vect [op v1 v2]
  (make-vect (op (xcor-vect v1)
                 (xcor-vect v2))
             (op (ycor-vect v1)
                 (ycor-vect v2))))

(defn add-vect [v1 v2]
  (bi-op-vect + v1 v2))

(defn sub-vect [v1 v2]
  (bi-op-vect - v1 v2))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Exercise 2.47
(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(defn make-frame [origin edge1 edge2]
  (cons origin (cons edge1 [edge2])))

(defn origin-frame [frame]
  (first frame))

(defn edge1-frame [frame]
  (first (rest frame)))

(defn edge2-frame [frame]
  (first (rest (rest frame))))

; Exercise 2.48
(defn make-segment [v1 v2]
  (list v1 v2))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (first (rest segment)))

; Exercise 2.49
(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(def edge1 (make-vect -1 5))
(def edge2 (make-vect 5 5))
(def f (make-frame (make-vect 0 0) edge1 edge2))
(origin-frame f)
(edge1-frame f)
(edge2-frame f)

((frame-coord-map f) (make-vect 1 0))
(def v (make-vect 1 1))
(xcor-vect v)
(ycor-vect v)
(edge1-frame f)
(scale-vect (xcor-vect v) (edge1-frame f))

(defn segments->painter [segment-list]
  (fn [frame]
    (doall
      (fn [segment]
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

; (a)
(def outline-segments
  (list (make-segment (make-vect 0 0)
                      (make-vect 1 0))
        (make-segment (make-vect 0 0)
                      (make-vect 0 1))
        (make-segment (make-vect 1 0)
                      (make-vect 1 1))
        (make-segment (make-vect 0 1)
                      (make-vect 1 1))))

(def outline (segments->painter outline-segments))

; (b)
(def X-segments
  (list (make-segment (make-vect 0 0)
                      (make-vect 1 1))
        (make-segment (make-vect 0 1)
                      (make-vect 1 0))))

(def X (segments->painter X-segments))

; (c)
(def diamond-segments
  (list (make-segment (make-vect 0 0.5)
                      (make-vect 0.5 0))
        (make-segment (make-vect 0 0.5)
                      (make-vect 0.5 1))
        (make-segment (make-vect 0.5 0)
                      (make-vect 1 0.5))
        (make-segment (make-vect 0.5 1)
                      (make-vect 1 0.5))))

(def diamond (segments->painter diamond-segments))

; (d)
(def wave-segments
  (list (make-segment (make-vect 0 (/ 11 16)) ; bottom-left
                      (make-vect (/ 3 16) (/ 7 16)))
        (make-segment (make-vect (/ 3 16) (/ 7 16))
                      (make-vect (/ 5 16) (/ 10 16)))
        (make-segment (make-vect (/ 5 16) (/ 10 16))
                      (make-vect (/ 6 16) (/ 8 16)))
        (make-segment (make-vect (/ 6 16) (/ 8 16))
                      (make-vect (/ 5 16) 0))
        (make-segment (make-vect 0 (/ 14 16)) ; upper-left
                      (make-vect (/ 3 16) (/ 10 16)))
        (make-segment (make-vect (/ 3 16) (/ 10 16))
                      (make-vect (/ 5 16) (/ 11 16)))
        (make-segment (make-vect (/ 5 16) (/ 11 16))
                      (make-vect (/ 7 16) (/ 11 16)))
        (make-segment (make-vect (/ 7 16) (/ 11 16))
                      (make-vect (/ 6 16) (/ 14 16)))
        (make-segment (make-vect (/ 6 16) (/ 14 16))
                      (make-vect (/ 7 16) 1))
        (make-segment (make-vect (/ 7 16) 0) ; middle legs
                      (make-vect (/ 8.5 16) (/ 5 16)))
        (make-segment (make-vect (/ 8.5 16) (/ 5 16))
                      (make-vect (/ 10 16) 0))
        (make-segment (make-vect (/ 10 16) 1) ; upper-right
                      (make-vect (/ 11 16) (/ 14 16)))
        (make-segment (make-vect (/ 11 16) (/ 14 16))
                      (make-vect (/ 10 16) (/ 11 16)))
        (make-segment (make-vect (/ 10 16) (/ 11 16))
                      (make-vect (/ 10 16) (/ 13 16)))
        (make-segment (make-vect (/ 10 16) (/ 13 16))
                      (make-vect 1 (/ 6 16)))
        (make-segment (make-vect (/ 12 16) 0) ; bottom-right
                      (make-vect (/ 11 16) (/ 8 16)))
        (make-segment (make-vect (/ 11 16) (/ 8 16))
                      (make-vect 1 (/ 3 16)))))

(def wave (segments->painter wave-segments))
