; (a)
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
                      (make-vect 1 (/ 3 16)))
        (make-segment (make-vect (/ 7.5 16) (/ 12 16)) ; add a mouth
                      (make-vect (/ 9.5 16) (/ 12 16)))
        (make-segment (make-vect (/ 7 16) (/ 15 16)) ; add left eye
                      (make-vect (/ 8 16) (/ 15 16)))
        (make-segment (make-vect (/ 10 16) (/ 15 16)) ; add right eye
                      (make-vect (/ 11 16) (/ 15 16)))
        (make-segment (make-vect (/ 8.5 10) (/ 14 16)) ; add nose
                      (make-vect (/ 8.5 10) (/ 13 16)))))

(def wave (segments->painter wave-segments))

; (b)
(defn corner-split [painter n]
  (if (= n 0)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          corner (corner-split painter (dec n))]
        (beside (below painter up)
                (below right corner)))))

; (c)
(defn square-of-four [tl tr bl br]
  (fn [painter]
    (let [top (beside (tl painter) (tr painter))
          bottom (beside (bl painter) (br painter))]
      (below bottom top))))

(def square-limit
  (square-of-four identity flip-horiz
                  flip-vert rotate-180)) ; Roger stares outwards
