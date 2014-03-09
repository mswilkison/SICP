(defn attach-tag [type-tag contents]
  (if (number? contents)
    contents
    (cons type-tag [contents])))

(defn type-tag [datum]
  (cond (seq? datum) (first datum)
        (number? datum) 'scheme-number
        :else (throw (Exception. "Bad tagged datum -- TYPE-TAG" datum))))

(defn contents [datum]
  (cond (seq? datum) (rest datum)
        (number? datum) datum
        :else (throw (Exception. "Bad tagged datum -- CONTENTS" datum))))
