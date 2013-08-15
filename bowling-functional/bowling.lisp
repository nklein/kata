;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata
;;; http://nklein.com/2013/01/bowling-game-kata-in-common-lisp

;;; Start: Wed Aug 14 18:58:57 CDT 2013
;;; End: Wed Aug 14 19:22:38 CDT 2013
;;; SLOC: 51

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
     ((< 10 frame) score)
     ((strike-p rolls) (add (strike-score rolls) #'cdr))
     ((spare-p rolls) (add (spare-score rolls)))
     (t (add (two-ball-sum rolls))))))

(defmethod score ((game game))
  (score-rolls (rolls game) 0 1))

(ql:quickload :nst)

(defun roll-many (game &key pins times)
  (cond
   ((zerop times) game)
   (t (roll-many (roll game pins) :pins pins :times (1- times)))))

(nst:def-criterion (:bowling-score (target) (&key initial pins times))
  (let* ((game (make-instance 'game :rolls initial))
         (final (roll-many game :pins pins :times times))
         (got (score final)))
    (if (= got target)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected ~D but got ~A"
                               :args (list got target)))))

(nst:def-test-group bowling-game-tests ()
  (nst:def-test can-roll-test (:true)
    (roll (make-instance 'game) 1))

  (nst:def-test zero-game-test (:bowling-score 0)
    :pins 0 :times 20)

  (nst:def-test ones-game-test (:bowling-score 20)
    :pins 1 :times 20)

  (nst:def-test spare-test (:bowling-score 14)
    :initial '(5 5 1 2) :pins 0 :times 16)

  (nst:def-test strike-test (:bowling-score 19)
    :initial '(10 1 2 3 0) :pins 0 :times 14)

  (nst:def-test perfect-game-test (:bowling-score 300)
    :pins 10 :times 12))
