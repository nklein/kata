
(defclass game ()
  ((rolls :initarg :rolls :initform nil :reader rolls)))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
    (make-instance 'game :rolls (list* pins (rolls game)))))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun strike-p (rolls)
  (= 10 (first rolls)))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (rest rolls))))

(defun spare-p (rolls)
  (= 10 (two-ball-sum rolls)))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun regular-score (rolls)
  (two-ball-sum rolls))

(defun calculate-score (rolls frame)
  (flet ((add-to-score (score next)
           (+ score (calculate-score next (1+ frame)))))
    (cond
     ((null rolls) 0)
     ((< 10 frame) 0)
     ((strike-p rolls) (add-to-score (strike-score rolls) (cdr rolls)))
     ((spare-p rolls)  (add-to-score (spare-score rolls) (cddr rolls)))
     (t                (add-to-score (regular-score rolls) (cddr rolls))))))

(defgeneric score (game)
  (:method ((game game))
    (calculate-score (reverse (rolls game)) 1)))

(defun roll-many (game &key initial rolls pins)
  (cond
   (initial (roll-many (roll game (first initial))
                       :initial (rest initial)
                       :rolls rolls
                       :pins pins))
   ((zerop rolls) game)
   (t (roll-many (roll game pins) :rolls (1- rolls) :pins pins))))

(ql:quickload :nst)

(nst:def-test-group bowling-game-kata-tests ()
  (nst:def-test all-gutter-balls (:equal 0)
    (score (roll-many (make-instance 'game) :rolls 20 :pins 0)))
  
  (nst:def-test all-one-balls (:equal 20)
    (score (roll-many (make-instance 'game) :rolls 20 :pins 1)))
  
  (nst:def-test spare-test (:equal 14)
    (score (roll-many (make-instance 'game)
                      :initial '(5 5 1 2) :rolls 16 :pins 0)))
  
  (nst:def-test strike-test (:equal 19)
    (score (roll-many (make-instance 'game)
                      :initial '(10 1 2 3 0) :rolls 14 :pins 0)))
  
  (nst:def-test perfect-game-test (:equal 300)
    (score (roll-many (make-instance 'game) :rolls 12 :pins 10))))