;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;;; Start: Mon May 13 22:24:17 CDT 2013
;;; End:   Mon May 13 22:59:14 CDT 2013
;;; SLOC:  56

(defclass game ()
  ((rolls :reader rolls
          :initarg :rolls))
  (:default-initargs :rolls nil))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (make-instance 'game :rolls (append (rolls game) (list pins)))))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun spare-p (rolls)
  (= (two-ball-sum rolls) 10))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun strike-p (rolls)
  (= (first rolls) 10))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (rest rolls))))

(defun score-rolls (rolls score frame)
  (flet ((add (amount &optional (next-fn #'cddr))
           (score-rolls (funcall next-fn rolls)
                        (+ amount score)
                        (1+ frame))))
    (cond
     ((< 10 frame) score)
     ((strike-p rolls) (add (strike-score rolls) #'cdr))
     ((spare-p rolls) (add (spare-score rolls)))
     (t (add (two-ball-sum rolls))))))

(defgeneric score (game)
  (:method ((game game))
     (score-rolls (rolls game) 0 1)))

(defun roll-many (game &key initial rolls pins)
  (cond
   (initial (roll-many (roll game (first initial))
                       :initial (rest initial)
                       :rolls rolls
                       :pins pins))
   ((zerop rolls) game)
   (t (roll-many (roll game pins)
                 :rolls (1- rolls)
                 :pins pins))))

(ql:quickload :nst)

(nst:def-criterion (:bowl (target &key initial rolls pins) ())
  (let ((score (score (roll-many (make-instance 'game)
                                 :initial initial
                                 :rolls rolls
                                 :pins pins))))
    (if (= score target)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score of ~D, but got ~A~%"
                               :args (list target score)))))

(nst:def-test-group bowling-tests ()
  (nst:def-test gutter-ball-game (:bowl 0 :rolls 20 :pins 0))

  (nst:def-test all-ones-game (:bowl 20 :rolls 20 :pins 1))

  (nst:def-test spare-test (:bowl 15 :initial (5 5 2 1) :rolls 16 :pins 0))

  (nst:def-test strike-test (:bowl 21 :initial (10 3 2 1 0) :rolls 16 :pins 0))

  (nst:def-test perfect-game (:bowl 300 :rolls 12 :pins 10)))
