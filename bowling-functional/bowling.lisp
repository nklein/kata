;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;;; Start: Tue Jul 30 14:52:49 CDT 2013
;;; End:   Tue Jul 30 15:28:28 CDT 2013
;;; SLOC:  53

(defclass game ()
  ((rolls :initarg :rolls :reader rolls :type list))
  (:default-initargs :rolls nil))

(defgeneric roll (game pins)
  (:method ((game game) pins)
     (make-instance 'game :rolls (append (rolls game) (list pins)))))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun frame-score (rolls)
  (two-ball-sum rolls))

(defun strike? (rolls)
  (= (first rolls) 10))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (rest rolls))))

(defun spare? (rolls)
  (= (two-ball-sum rolls) 10))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun add-score (rolls score frame)
  (flet ((add (amount &optional (next #'cddr))
              (add-score (funcall next rolls) (+ amount score) (1+ frame))))
    (cond
     ((< 10 frame)    score)
     ((strike? rolls) (add (strike-score rolls) #'cdr))
     ((spare? rolls)  (add (spare-score rolls)))
     (t               (add (frame-score rolls))))))

(defgeneric score (game)
  (:method ((game game))
     (add-score (rolls game) 0 1)))

(defun roll-many (game &key initial pins rolls)
  (cond
   ((zerop rolls) game)
   (initial (roll-many (roll game (first initial))
                       :initial (rest initial) :pins pins :rolls rolls))
   (t (roll-many (roll game pins) :pins pins :rolls (1- rolls)))))

(ql:quickload :nst)

(nst:def-criterion (:bowling-score (target) (&rest roll-options))
  (let ((got (score (apply #'roll-many (make-instance 'game) roll-options))))
    (if (= target got)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score of ~D but got ~A"
                               :args (list target got)))))

(nst:def-test-group bowling-game-tests ()
  (nst:def-test zero-game (:bowling-score 0)
    :pins 0 :rolls 20)

  (nst:def-test ones-game (:bowling-score 20)
    :pins 1 :rolls 20)

  (nst:def-test spare-game (:bowling-score 15)
    :initial '(5 5 2 1) :pins 0 :rolls 16)

  (nst:def-test strike-game (:bowling-score 21)
    :initial '(10 3 2 1 0) :pins 0 :rolls 14)

  (nst:def-test perfect-game (:bowling-score 300)
    :pins 10 :rolls 12))
