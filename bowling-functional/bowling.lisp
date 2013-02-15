;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun strike-p (rolls)
  (= (first rolls) 10))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (rest rolls))))

(defun spare-p (rolls)
  (= (two-ball-sum rolls) 10))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun regular-score (rolls)
  (two-ball-sum rolls))

(defun score-rolls (rolls score frame)
  (flet ((add (n &optional (next (cddr rolls)))
           (score-rolls next (+ n score) (1+ frame))))
    (cond
      ((null rolls) score)
      ((< 10 frame) score)
      ((strike-p rolls) (add (strike-score rolls) (rest rolls)))
      ((spare-p rolls) (add (spare-score rolls)))
      (t (add (regular-score rolls))))))

(defgeneric score (game)
  (:method ((game game))
    (score-rolls (rolls game) 0 1)))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
           (make-instance 'game :rolls (append (rolls game) (list pins)))))

(defun roll-start (game start)
  (if start
      (roll-start (roll game (first start)) (rest start))
    game))

(defun roll-constantly (game n pins)
  (if (plusp n)
      (roll-constantly (roll game pins) (1- n) pins)
    game))

(defgeneric roll-many (game &key start how-many pins)
  (:method ((game game) &key start how-many pins)
    (roll-constantly (roll-start game start) how-many pins)))

(ql:quickload :nst)

(nst:def-criterion (:score (target) (game))
  (let ((actual (score game)))
    (if (= actual target)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score of ~A but got ~A"
                               :args (list target actual)))))

(nst:def-test-group bowling-game-kata-tests ()
  (nst:def-test all-gutter-balls-test (:score 0)
    (roll-many (make-instance 'game) :how-many 20 :pins 0))
  
  (nst:def-test all-one-balls-test (:score 20)
    (roll-many (make-instance 'game) :how-many 20 :pins 1))
  
  (nst:def-test spare-test (:score 14)
    (roll-many (make-instance 'game)
               :start '(5 5 1 2) :how-many 16 :pins 0))
  
  (nst:def-test strike-test (:score 19)
    (roll-many (make-instance 'game)
               :start '(10 1 2 3 0) :how-many 14 :pins 0))

  (nst:def-test perfect-game (:score 300)
    (roll-many (make-instance 'game) :how-many 12 :pins 10)))