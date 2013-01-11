;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

(defclass game ()
  ((rolls :initform (make-array '(21) :element-type 'integer
                                      :initial-element 0
                                      :fill-pointer 0)
          :reader rolls)))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (let ((rolls (rolls game)))
       (with-accessors ((index fill-pointer)) rolls
         (setf (aref rolls index) pins)
         (incf index)))))

(defgeneric sum-of-next-two-balls (game index)
  (:method ((game game) (index integer))
    (let ((rolls (rolls game)))
      (+ (aref rolls index) (aref rolls (1+ index))))))

(defgeneric strike-p (game index)
  (:method ((game game) (index integer))
    (= 10 (aref (rolls game) index))))

(defgeneric strike-score (game index)
  (:method ((game game) (index integer))
     (values (+ 10 (sum-of-next-two-balls game (1+ index)))
             1)))

(defgeneric spare-p (game index)
  (:method ((game game) (index integer))
    (= 10 (sum-of-next-two-balls game index))))

(defgeneric spare-score (game index)
  (:method ((game game) (index integer))
     (values (+ 10 (aref (rolls game) (+ index 2)))
             2)))

(defgeneric regular-score (game index)
  (:method ((game game) (index integer))
    (values (sum-of-next-two-balls game index)
            2)))

(defgeneric frame-score (game index)
  (:method ((game game) (index integer))
     (cond
      ((strike-p game index) (strike-score game index))
      ((spare-p game index) (spare-score game index))
      (t (regular-score game index)))))

(defgeneric score (game)
  (:method ((game game))
     (loop :with index = 0
           :for frame :from 1 :to 10
           :summing (multiple-value-bind (score inc) (frame-score game index)
                      (incf index inc)
                      score))))

(defgeneric roll-many (game &key initial rolls pins)
  (:method ((game game) &key initial rolls pins)
     (mapc (lambda (pins) (roll game pins)) initial)
     (dotimes (ii rolls (score game))
       (roll game pins))))

(ql:quickload :nst)

(nst:def-fixtures game-instance ()
   (game))

(nst:def-test-group bowling-game-kata-game-test (game-instance)
         (:each-setup (setf game (make-instance 'game)))
         
  (nst:def-test all-gutter-balls-test (:equal 0)
     (roll-many game :rolls 20 :pins 0))
  
  (nst:def-test all-one-balls-test (:equal 20)
     (roll-many game :rolls 20 :pins 1))
  
  (nst:def-test spare-test (:equal 14)
     (roll-many game :initial '(5 5 1 2) :rolls 16 :pins 0))
  
  (nst:def-test strike-test (:equal 19)
     (roll-many game :initial '(10 1 2 3 0) :rolls 14 :pins 0))
  
  (nst:def-test perfect-game-test (:equal 300)
     (roll-many game :rolls 12 :pins 10)))