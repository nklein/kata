(defclass game ()
  ((rolls :initform (make-array '(21) :element-type 'integer
                                      :initial-element 0
                                      :fill-pointer 0)
          :accessor rolls)))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
    (let ((rolls (rolls game)))
      (with-accessors ((index fill-pointer)) rolls
        (setf (aref rolls index) pins)
        (incf index)))))

(defun two-ball-sum (game index)
  (let ((rolls (rolls game)))
    (+ (aref rolls index) (aref rolls (1+ index)))))

(defun strike-p (game index)
  (= 10 (aref (rolls game) index)))

(defun strike-score (game index)
  (+ 10 (two-ball-sum game (1+ index))))

(defun spare-p (game index)
  (= 10 (two-ball-sum game index)))

(defun spare-score (game index)
  (+ 10 (aref (rolls game) (+ index 2))))

(defun regular-score (game index)
  (+ (two-ball-sum game index)))

(defgeneric score (game)
  (:method ((game game))
    (loop :for ii :from 0 :to (fill-pointer (rolls game))
          :for frame :from 1 :to 10
          :summing (cond
                    ((strike-p game ii) (strike-score game ii))
                    ((spare-p game ii) (prog1
                                           (spare-score game ii)
                                         (incf ii)))
                    (t (prog1
                           (regular-score game ii)
                         (incf ii)))))))

(defgeneric roll-many (game &key initial rolls pins)
  (:method ((game game) &key initial rolls pins)
    (dolist (pins initial)
      (roll game pins))
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