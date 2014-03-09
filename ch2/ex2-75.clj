(defn make-from-mag-ang [r a]
  (defn dispatch [op]
    (cond (= op 'magnitude) r
          (= op 'angle) a
          (= op 'real-part) (* r (Math/cos a))
          (= op 'imag-part (* r (Math/sin a)))
          :else (throw (Exception. "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
