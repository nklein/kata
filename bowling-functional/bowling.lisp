;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;;; Start: Sun Jun 23 15:30:40 CDT 2013
;;; Hard to think... lots of noisy kids around
;;; End: Sun Jun 23 16:04:18 CDT 2013
;;; SLOC: 50

(defclass game ()
  ((rolls :reader rolls :initarg :rolls))
  (:default-initargs :rolls nil))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun spare-p (rolls)
  (= (two-ball-sum rolls) 10))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun strike-p (rolls)
  (= 10 (first rolls)))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (rest rolls))))

(defun score-rolls (rolls score frame)
  (flet ((add (amount &optional (rest (cddr rolls)))
           (score-rolls rest (+ score amount) (1+ frame))))
    (cond
     ((null rolls) score)
     ((< 10 frame) score)
     ((spare-p rolls) (add (spare-score rolls)))
     ((strike-p rolls) (add (strike-score rolls) (rest rolls)))
     (t (add (two-ball-sum rolls))))))

(defun score (game)
  (score-rolls (rolls game) 0 1))

(defun roll (game pins)
  (make-instance 'game :rolls (append (rolls game) (list pins))))

(defun roll-many (game &key pins times)
  (cond
   ((zerop times) game)
   (t (roll-many (roll game pins) :pins pins :times (1- times)))))

(ql:quickload :nst)

(nst:def-criterion (:bowling-score (target) (&key initial pins times))
  (let* ((final (roll-many (make-instance 'game :rolls initial)
                           :pins pins :times times))
         (got (score final)))
    (if (= target got)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected ~D but got ~S~%"
                               :args (list target got)))))

(nst:def-test-group bowling-tests ()
  (nst:def-test gutter-test (:bowling-score 0)
    :pins 0 :times 20)

  (nst:def-test ones-test (:bowling-score 20)
    :pins 1 :times 20)

  (nst:def-test spare-test (:bowling-score 14)
    :initial '(5 5 1 2) :pins 0 :times 16)

  (nst:def-test strike-test (:bowling-score 19)
    :initial '(10 1 2 3 0) :pins 0 :times 14)

  (nst:def-test perfect-game (:bowling-score 300)
    :pins 10 :times 12))
