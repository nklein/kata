;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

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

(defun score-rolls (rolls frame)
  (flet ((add (amount rest)
           (+ amount (score-rolls rest (1+ frame)))))
    (cond
     ((null rolls) 0)
     ((< 10 frame) 0)
     ((strike-p rolls) (add (strike-score rolls) (cdr rolls)))
     ((spare-p rolls) (add (spare-score rolls) (cddr rolls)))
     (t (add (regular-score rolls) (cddr rolls))))))

(defgeneric score (game)
  (:method ((game game))
     (score-rolls (reverse (rolls game)) 1)))

(defgeneric roll-many (game &key initial rolls pins)
  (:method ((game game) &key initial rolls pins)
    (cond
     (initial (roll-many (roll game (first initial))
                         :initial (rest initial)
                         :rolls rolls
                         :pins pins))
     ((zerop rolls) game)
     (t (roll-many (roll game pins)
                   :rolls (1- rolls)
                   :pins pins)))))

(ql:quickload :nst)

(nst:def-criterion (:score (target) (game))
  (let ((actual (score game)))
    (if (= actual target)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score ~A but got ~A"
                               :args (list target actual)))))

(nst:def-test-group bowling-game-kata-tests ()
  (nst:def-test all-gutter-balls-test (:score 0)
    (roll-many (make-instance 'game) :rolls 20 :pins 0))
  
  (nst:def-test all-one-balls-test (:score 20)
    (roll-many (make-instance 'game) :rolls 20 :pins 1))

  (nst:def-test spare-test (:score 14)
    (roll-many (make-instance 'game) :initial '(5 5 1 2) :rolls 16 :pins 0))
  
  (nst:def-test strike-test (:score 19)
    (roll-many (make-instance 'game) :initial '(10 1 2 3 0) :rolls 14 :pins 0))
  
  (nst:def-test perfect-game-test (:score 300)
    (roll-many (make-instance 'game) :rolls 12 :pins 10)))