;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;;; Start: Wed Aug 14 11:24:35 CDT 2013
;;; End: Wed Aug 14 11:56:07 CDT 2013
;;; SLOC: 50

(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

(defmethod roll ((game game) pins)
  (make-instance 'game :rolls (append (rolls game) (list pins))))

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

(defun score-rolls (rolls score frame)
  (flet ((add (points &optional (next-fn #'cddr))
           (score-rolls (funcall next-fn rolls) (+ score points) (1+ frame))))
    (cond
      ((or (null rolls) (< 10 frame)) score)
      ((strike-p rolls) (add (strike-score rolls) #'cdr))
      ((spare-p rolls) (add (spare-score rolls)))
      (t (add (two-ball-sum rolls))))))

(defmethod score ((game game))
  (score-rolls (rolls game) 0 1))

(ql:quickload :nst)

(defun roll-many (game &key initial pins rolls)
  (cond
    (initial (roll-many (roll game (first initial))
                        :initial (rest initial) :pins pins :rolls rolls))
    ((zerop rolls) game)
    (t (roll-many (roll game pins) :pins pins :rolls (1- rolls)))))

(nst:def-criterion (:bowling-score (target) (&rest roll-options))
  (let* ((game (apply #'roll-many (make-instance 'game) roll-options))
         (got (score game)))
    (if (= got target)
        (nst:make-success-report)
        (nst:make-failure-report :format "Expected ~D but got ~A~%"
                                 :args (list target got)))))

(nst:def-test-group bowling-game-kata-tests ()
  (nst:def-test gutter-game-test (:bowling-score 0)
    :rolls 20 :pins 0)

  (nst:def-test ones-game-test (:bowling-score 20)
    :rolls 20 :pins 1)

  (nst:def-test spare-test (:bowling-score 14)
    :initial '(5 5 1 2) :rolls 16 :pins 0)

  (nst:def-test strike-test (:bowling-score 19)
    :initial '(10 1 2 3 0) :rolls 16 :pins 0)

  (nst:def-test perfect-game-test (:bowling-score 300)
    :rolls 12 :pins 10))
