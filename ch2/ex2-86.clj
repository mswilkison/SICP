; generic operations
(defn sine [x] (apply-generic 'sine x))
(defn cosine [x] (apply-generic 'cosine x))

;add to scheme-number package
(put 'sine 'scheme-number (fn [x]
                            (tag (Math/sin x))))
(put 'cosine 'scheme-number (fn [x]
                              (tag (Math/cos x))))

;add to rational package
(put 'sine 'rational (fn [x]
                       (tag (Math/sin x))))
(put 'cosine 'rational (fn [x]
                         (tag (Math/cos x))))

; To handle handle complex numbers with generic types, we need to replace
; the primitive operators with generic operators
(defn add-complex [z1 z2]
  (make-from-real-imag (add (real-part z1) (real-part z2))
                        (add (imag-part z1) (imag-part z2))))
(defn sub-complex [z1 z2]
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                        (sub (imag-part z1) (imag-part z2))))
(defn mul-complex [z1 z2]
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                      (add (angle z1) (angle z2))))
(defn div-complex [z1 z2]
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                      (sub (angle z1) (angle z2))))
