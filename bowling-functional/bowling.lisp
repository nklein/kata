;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;; Start: Fri Feb 15 17:09:15 CST 2013
;; End:   Fri Feb 15 17:38:13 CST 2013
;; SLOC:  61 bowling.lisp

(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (make-instance 'game :rolls (append (rolls game)
                                         (list pins)))))

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

(defun score-rolls (rolls frame)
  (flet ((accumulate (score-fn &optional (next-fn #'cddr))
           (+ (funcall score-fn rolls)
              (score-rolls (funcall next-fn rolls)
                           (1+ frame)))))
    (cond
     ((null rolls)     0)
     ((< 10 frame)     0)
     ((strike-p rolls) (accumulate #'strike-score #'cdr))
     ((spare-p rolls)  (accumulate #'spare-score))
     (t                (accumulate #'regular-score)))))

(defgeneric score (game)
  (:method ((game game))
     (score-rolls (rolls game) 1)))

(defun roll-constantly (game balls pins)
  (if (plusp balls)
      (roll-constantly (roll game pins) (1- balls) pins)
    game))

(defun roll-list (game list)
  (if list
      (roll-list (roll game (first list)) (rest list))
    game))

(ql:quickload :nst)

(nst:def-criterion (:bowling-score (target) (game))
  (let ((actual (score game)))
    (if (= actual target)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score of ~A but got ~A"
                               :args (list target actual)))))

(nst:def-test-group bowling-game-kata-tests ()
  (nst:def-test all-gutter-balls-test (:bowling-score 0)
    (roll-constantly (make-instance 'game) 20 0))
  
  (nst:def-test all-one-balls-test (:bowling-score 20)
    (roll-constantly (make-instance 'game) 20 1))
  
  (nst:def-test spare-test (:bowling-score 14)
    (roll-constantly (roll-list (make-instance 'game) '(5 5 1 2))
                     16 0))
  
  (nst:def-test strike-test (:bowling-score 19)
    (roll-constantly (roll-list (make-instance 'game) '(10 1 2 3 0))
                     14 0))
  
  (nst:def-test perfect-game-test (:bowling-score 300)
    (roll-constantly (make-instance 'game) 12 10)))
