; Each division needs to provide its name and unique
; personnel file in order to construct a generic-file
(defn make-generic-file [division file]
  (cons division [file]))

(defn division [file]
  (first file))

(defn personnel-file [file]
  (second file))

(defn get-record [generic-file employee]
  (get 'get-record (division generic-file)) employee
                                            (personnel-file generic-file))


; We make a generic record by consing the division name
; to an original record
(defn make-generic-record [div record]
  (cons div [record]))

(defn division [generic-record]
  (first generic-record))

(defn original-record [generic-record]
  (second generic-record))

(defn get-salary [generic-record]
  ((get 'get-salary (division generic-record))
    (original-record generic-record)))

; Assumes each divison implements an exists? predicate
(defn exists? [employee division]
  ((get 'exists? division) employee))

(defn find-employee-record [employee files]
  (cond (empty? files) (throw (Exception. "this employee does not exist" employee))
        (exists? employee (division (first files))) (get-record (first files) employee)
        :else (find-employee-record employee (rest files))))
