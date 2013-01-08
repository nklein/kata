
(defclass game ()
  ((rolls :initform (make-array '(21) :element-type 'integer
                                      :initial-element 0
                                      :fill-pointer 0)
          :reader rolls)))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (with-accessors ((index fill-pointer)) (rolls game)
        (setf (aref (rolls game) index) pins)
        (incf index))))

(defgeneric regular-score (game index)
  (:method ((game game) (index integer))
     (let ((rolls (rolls game)))
       (+ (aref rolls index)
          (aref rolls (1+ index))))))

(defgeneric strike-p (game index)
  (:method ((game game) (index integer))
     (= 10 (aref (rolls game) index))))

(defgeneric strike-score (game index)
  (:method ((game game) (index integer))
     (+ 10 (regular-score game (1+ index)))))

(defgeneric spare-p (game index)
  (:method ((game game) (index integer))
     (= 10 (regular-score game index))))

(defgeneric spare-score (game index)
  (:method ((game game) (index integer))
     (+ 10 (aref (rolls game) (+ index 2)))))

(defgeneric frame-score (game index)
  (:method ((game game) (index integer))
     (cond
      ((strike-p game index) (values (strike-score game index) 1))
      ((spare-p game index) (values (spare-score game index) 2))
      (t (values (regular-score game index) 2)))))

(defgeneric score (game)
  (:method ((game game))
     (loop :for frame :from 1 :to 10
           :with index = 0
           :summing (multiple-value-bind (score inc) (frame-score game index)
                      (incf index inc)
                      score))))

(defgeneric roll-many (game &key initial rolls pins)
  (:method ((game game) &key initial rolls pins)
     (mapc (lambda (pins)
             (roll game pins))
           initial)
     (dotimes (ii rolls (score game))
       (roll game pins))))

(ql:quickload :nst)

(nst:def-fixtures game-instance ()
   (game))

(nst:def-test-group bowling-game-kata-tests (game-instance)
    (:each-setup (setf game (make-instance 'game)))
  
  (nst:def-test all-gutter-balls-test (:equal 0)
    (roll-many game :rolls 20 :pins 0))
  
  (nst:def-test all-one-balls-test (:equal 20)
    (roll-many game :rolls 20 :pins 1))
  
  (nst:def-test spare-test (:equal 14)
    (roll-many game :initial '(5 5 1 2) :rolls 16 :pins 0))
  
  (nst:def-test strike-test (:equal 19)
    (roll-many game :initial '(10 1 2 3 0) :rolls 14 :pins 0))
  
  (nst:def-test perfect-game-test (:equal 300)
    (roll-many game :rolls 12 :pins 10)))