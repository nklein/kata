;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

;;; Start: Sun May 12 22:23:08 CDT 2013
;;; End:   Sun May 12 22:58:00 CDT 2013
;;; SLOC:  59

(defclass game ()
  ((rolls :reader rolls
          :initarg :rolls))
  (:default-initargs :rolls nil))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (make-instance 'game :rolls (append (rolls game)
                                         (list pins)))))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun spare-p (rolls)
  (= (two-ball-sum rolls) 10))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun strike-p (rolls)
  (= (first rolls) 10))

(defun strike-score (rolls)
  (+ 10 (second rolls) (third rolls)))

(defun rolls-score (rolls frame accum)
  (flet ((add (amount &optional (next-frame-fn #'cddr))
           (rolls-score (funcall next-frame-fn rolls)
                        (1+ frame)
                        (+ accum amount))))
    (cond
     ((< 10 frame) accum)
     ((strike-p rolls) (add (strike-score rolls) #'cdr))
     ((spare-p rolls) (add (spare-score rolls)))
     (t (add (two-ball-sum rolls))))))

(defgeneric score (game)
  (:method ((game game))
    (rolls-score (rolls game) 1 0)))

(defun roll-many (game &key initial rolls pins)
  (cond
   (initial (roll-many (roll game (first initial))
                       :initial (rest initial)
                       :rolls rolls
                       :pins pins))
   ((plusp rolls) (roll-many (roll game pins) :rolls (1- rolls) :pins pins))
   (t game)))

(ql:quickload :nst)

(nst:def-criterion (:bowling-score (target) (game))
  (let ((score (score game)))
    (cond
     ((= score target) (nst:make-success-report))
     (t (nst:make-failure-report :format "Expected score ~D but got ~S"
                                 :args (list target score))))))

(nst:def-fixtures game-ins ()
  (game (make-instance 'game)))

(nst:def-test-group bowling-tests (game-ins)
  (nst:def-test gutter-ball-test (:bowling-score 0)
    (roll-many game :rolls 20 :pins 0))

  (nst:def-test all-ones-test (:bowling-score 20)
    (roll-many game :rolls 20 :pins 1))

  (nst:def-test spare-test (:bowling-score 15)
    (roll-many game :initial '(5 5 2 1) :rolls 16 :pins 0))

  (nst:def-test strike-test (:bowling-score 21)
    (roll-many game :initial '(10 3 2 1 0) :rolls 14 :pins 0))

  (nst:def-test perfect-game (:bowling-score 300)
    (roll-many game :rolls 12 :pins 10)))
