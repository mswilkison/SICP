(defn split [transform1 transform2]
  (fn [painter n]
    (if (= n 0)
      painter
      (let [smaller ((split transform1 transform2) painter (dec n))]
        (transform1 painter (transform2 smaller smaller))))))
