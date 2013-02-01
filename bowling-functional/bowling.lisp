;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (make-instance 'game :rolls (list* pins (rolls game)))))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun spare-p (rolls)
  (= 10 (two-ball-sum rolls)))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun regular-score (rolls)
  (two-ball-sum rolls))

(defun strike-p (rolls)
  (= 10 (first rolls)))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (rest rolls))))

(defun score-rolls (rolls frame)
  (flet ((add-to-score (amount &optional (next (cddr rolls)))
           (+ amount (score-rolls next (1+ frame)))))
    (cond
     ((null rolls) 0)
     ((< 10 frame) 0)
     ((strike-p rolls) (add-to-score (strike-score rolls) (cdr rolls)))
     ((spare-p rolls) (add-to-score (spare-score rolls)))
     (t (add-to-score (regular-score rolls))))))

(defgeneric score (game)
  (:method ((game game))
     (score-rolls (reverse (rolls game)) 1)))

(defun roll-initial (game initial)
  (cond
   ((null initial) game)
   (t (destructuring-bind (first . rest) initial
        (roll-initial (roll game first) rest)))))

(defun roll-repeats (game rolls pins)
  (cond
   ((plusp rolls) (roll-repeats (roll game pins) (1- rolls) pins))
   (t game)))

(defgeneric roll-many (game &key initial pins rolls)
  (:method ((game game) &key initial pins rolls)
    (roll-repeats (roll-initial game initial) rolls pins)))

(ql:quickload :nst)

(nst:def-criterion (:score (target) (game))
  (let ((score (score game)))
    (if (= target score)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score ~A but got ~A"
                               :args (list target score)))))

(nst:def-test-group bowling-game-kata-tests ()
  (nst:def-test all-gutter-balls-test (:score 0)
    (roll-many (make-instance 'game) :rolls 20 :pins 0))
  
  (nst:def-test all-one-balls-test (:score 20)
    (roll-many (make-instance 'game) :rolls 20 :pins 1))
  
  (nst:def-test spare-test (:score 14)
    (roll-many (make-instance 'game) :initial '(5 5 1 2) :rolls 16 :pins 0))
  
  (nst:def-test strike-test (:score 19)
    (roll-many (make-instance 'game)
               :initial '(10 1 2 3 0) :rolls 14 :pins 0))
  
  (nst:def-test perfect-game-test (:score 300)
    (roll-many (make-instance 'game) :rolls 12 :pins 10)))