;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;;; Start: Thu May 16 07:01:38 CDT 2013
;;; End:   Thu May 16 07:36:22 CDT 2013
;;; SLOC:  50

(defstruct game
  (rolls nil :type list))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun strike-p (rolls)
  (= (first rolls) 10))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (cdr rolls))))

(defun spare-p (rolls)
  (= (two-ball-sum rolls) 10))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun score-rolls (rolls frame score)
  (flet ((add (amount &optional (next-fn #'cddr))
           (score-rolls (funcall next-fn rolls) (1+ frame) (+ score amount))))
    (cond
     ((< 10 frame) score)
     ((strike-p rolls) (add (strike-score rolls) #'cdr))
     ((spare-p rolls) (add (spare-score rolls)))
     (t (add (two-ball-sum rolls))))))

(defun score (game)
  (check-type game game)
  (score-rolls (game-rolls game) 1 0))

(defun roll (game pins)
  (check-type game game)
  (check-type pins (integer 0 10))
  (make-game :rolls (append (game-rolls game) (list pins))))

(defun roll-many (game &key rolls pins)
  (check-type game game)
  (check-type rolls (integer 0 20))
  (check-type pins (integer 0 10))
  (cond
   ((zerop rolls) game)
   (t (roll-many (roll game pins) :rolls (1- rolls) :pins pins))))

(ql:quickload :nst)

(nst:def-criterion (:bowled (target &key initial rolls pins) ())
  (let ((score (score (roll-many (make-game :rolls initial)
                                 :rolls rolls
                                 :pins pins))))
    (if (= score target)
        (nst:make-success-report)
      (nst:make-failure-report :format "Expected score of ~D but got ~S"
                               :args (list target score)))))

(nst:def-test-group bowling-tests ()
  (nst:def-test gutter-ball-game (:bowled 0 :rolls 20 :pins 0))
  (nst:def-test all-ones-game (:bowled 20 :rolls 20 :pins 1))
  (nst:def-test spare-test (:bowled 15 :initial (5 5 2 1) :rolls 16 :pins 0))
  (nst:def-test strike-test (:bowled 21 :initial (10 3 2 1 0)
                                        :rolls 14 :pins 0))
  (nst:def-test perfect-game (:bowled 300 :rolls 12 :pins 10)))
