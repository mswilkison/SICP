(defn f [g]
  (g 2))

(f square)

(f (fn [z] (* z (+ z 1))))

(f f)
;--> (f 2)
;--> (2 2)
; This causes and error in which 2 (which is not a procedure) is applied to 2
