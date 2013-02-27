;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;;; Start: Tue Feb 26 23:05:46 CST 2013
;;; End: Tue Feb 26 23:33:29 CST 2013
;;; SLOC: 59

(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (make-instance 'game :rolls (append (rolls game) (list pins)))))

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
  (flet ((accumulate (value &optional (next-fn #'cddr))
           (+ value (score-rolls (funcall next-fn rolls) (1+ frame)))))
    (cond
     ((null rolls) 0)
     ((< 10 frame) 0)
     ((strike-p rolls) (accumulate (strike-score rolls) #'cdr))
     ((spare-p rolls) (accumulate (spare-score rolls)))
     (t (accumulate (regular-score rolls))))))

(defgeneric score (game)
  (:method ((game game))
     (score-rolls (rolls game) 1)))

(defun roll-multiple-times (game &key pins count)
  (if (plusp count)
      (roll-multiple-times (roll game pins) :pins pins :count (1- count))
    game))

(defun roll-list (game list)
  (if (null list)
      game
    (roll-list (roll game (first list)) (rest list))))

(defun roll-many (game &key initial pins count)
  (roll-multiple-times (roll-list game initial) :pins pins :count count))

(ql:quickload :nst)

(nst:def-criterion (:bowling-score (target) (game))
  (let ((score (score game)))
    (if (= score target)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score of ~A but got ~A"
                               :args (list target score)))))

(nst:def-test-group bowling-game-kata-tests ()
  (nst:def-test all-gutter-balls-test (:bowling-score 0)
    (roll-many (make-instance 'game) :pins 0 :count 20))
  
  (nst:def-test all-one-balls-test (:bowling-score 20)
    (roll-many (make-instance 'game) :pins 1 :count 20))
  
  (nst:def-test spare-test (:bowling-score 14)
    (roll-many (make-instance 'game) :initial '(5 5 1 2) :pins 0 :count 16))
  
  (nst:def-test strike-test (:bowling-score 19)
    (roll-many (make-instance 'game) :initial '(10 1 2 3 0)
                                     :pins 0 :count 14))
  
  (nst:def-test perfect-game-test (:bowling-score 300)
    (roll-many (make-instance 'game) :pins 10 :count 12)))
