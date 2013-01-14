
(defclass game ()
  ((rolls :initform (make-array '(21) :element-type 'integer
                                      :initial-element 0
                                      :fill-pointer 0)
          :reader rolls)))

(defgeneric sum-of-two-balls (game index)
  (:method ((game game) (index integer))
     (let ((rolls (rolls game)))
       (+ (aref rolls index) (aref rolls (1+ index))))))

(defgeneric strike-p (game index)
  (:method ((game game) (index integer))
     (let ((rolls (rolls game)))
       (= 10 (aref rolls index)))))

(defgeneric strike-score (game index)
  (:method ((game game) (index integer))
     (+ 10 (sum-of-two-balls game (1+ index)))))

(defgeneric spare-p (game index)
  (:method ((game game) (index integer))
     (= 10 (sum-of-two-balls game index))))

(defgeneric spare-score (game index)
  (:method ((game game) (index integer))
     (+ 10 (aref (rolls game) (+ index 2)))))

(defgeneric regular-score (game index)
  (:method ((game game) (index integer))
     (sum-of-two-balls game index)))

(defgeneric score (game)
  (:method ((game game))
     (loop :for frame :from 1 :to 10
           :for ii :from 0 :by 2
           :summing (cond
                     ((strike-p game ii) (prog1
                                             (strike-score game ii)
                                           (decf ii)))
                     ((spare-p game ii) (spare-score game ii))
                     (t (regular-score game ii))))))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (let ((rolls (rolls game)))
       (with-accessors ((index fill-pointer)) rolls
          (setf (aref rolls index) pins)
          (incf index)))))

(defgeneric roll-many (game &key initial rolls pins)
  (:method ((game game) &key initial rolls pins)
     (dolist (pins initial)
       (roll game pins))
     (dotimes (ii rolls (score game))
       (roll game pins))))

(ql:quickload :nst)

(nst:def-fixtures game-instance ()
   (game))

(nst:def-test-group bowling-game-kata-tests (game-instance)
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