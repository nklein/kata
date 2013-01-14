
(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

(defun sum-of-two-balls (rolls)
  (+ (first rolls) (second rolls)))

(defun spare-p (rolls)
  (= 10 (sum-of-two-balls rolls)))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun strike-p (rolls)
  (= 10 (first rolls)))

(defun strike-score (rolls)
  (+ 10 (sum-of-two-balls (rest rolls))))

(defun regular-score (rolls)
  (sum-of-two-balls rolls))

(defun calc-score (rolls frame)
  (cond
   ((null rolls) 0)
   ((< 10 frame) 0)
   ((strike-p rolls) (+ (strike-score rolls)
                        (calc-score (cdr rolls) (1+ frame))))
   ((spare-p rolls)  (+ (spare-score rolls)
                        (calc-score (cddr rolls) (1+ frame))))
   (t                (+ (regular-score rolls)
                        (calc-score (cddr rolls) (1+ frame))))))

(defgeneric score (game)
  (:method ((game game))
     (calc-score (reverse (rolls game)) 1)))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (make-instance 'game :rolls (list* pins (rolls game)))))

(defgeneric roll-many (game &key initial rolls pins)
  (:method ((game (eql nil)) &key initial rolls pins)
     (roll-many (make-instance 'game)
                :initial initial :rolls rolls :pins pins))
  
  (:method ((game game) &key initial rolls pins)
     (cond
      (initial (roll-many (roll game (first initial))
                          :initial (rest initial) :rolls rolls :pins pins))
      ((plusp rolls)
               (roll-many (roll game pins) :rolls (1- rolls) :pins pins))
      (t game))))

(ql:quickload :nst)

(nst:def-test-group functional-bowling-game-kata-tests ()
   (nst:def-test all-gutter-balls (:equal 0)
      (score (roll-many nil :rolls 20 :pins 0)))
   
   (nst:def-test all-one-balls (:equal 20)
      (score (roll-many nil :rolls 20 :pins 1)))
   
   (nst:def-test spare-test (:equal 14)
      (score (roll-many nil :initial '(5 5 1 2) :rolls 16 :pins 0)))
   
   (nst:def-test strike-test (:equal 19)
      (score (roll-many nil :initial '(10 1 2 3 0) :rolls 14 :pins 0)))
   
   (nst:def-test perfect-game-test (:equal 300)
      (score (roll-many nil :rolls 12 :pins 10))))
