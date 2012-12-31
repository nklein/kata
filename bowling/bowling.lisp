;;; http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata

(defclass game ()
  ((rolls :initform (make-array '(21) :element-type 'integer
                                      :initial-element 0
                                      :fill-pointer 0)
          :accessor rolls)))

(defgeneric roll (game pins)
  (:method ((game game) (pins integer))
     (with-accessors ((rolls rolls)) game
       (with-accessors ((index fill-pointer)) rolls
         (prog1
             (setf (aref rolls index) pins)
           (incf index))))))

(defgeneric score (game)
  (:method ((game game))
     (with-accessors ((rolls rolls)) game
       (labels ((at (index)
                  (aref rolls index))
                
                (sum-of (start how-many)
                  (loop :for index :from start :below (+ start how-many)
                        :summing (at index)))
                
                (sum-of-frame (index)
                  (sum-of index 2))
              
                (strike-p (index)
                  (= 10 (at index)))
                
                (strike-bonus (index)
                  (sum-of (1+ index) 2))
                
                (spare-p (index)
                  (= 10 (sum-of-frame index)))
              
                (spare-bonus (index)
                  (+ (at (+ index 2)))))
         
         (loop :for frame :from 1 :to 10
               :for index :from 0
               :summing (cond
                         ((strike-p index) (+ 10 (strike-bonus index)))
                         
                         ((spare-p index) (prog1
                                              (+ 10 (spare-bonus index))
                                            (incf index)))
                         (t (prog1
                                (sum-of-frame index)
                              (incf index)))))))))

(ql:quickload :nst)

(nst:def-fixtures game-instance ()
  (game))

(defun roll-many (game &key rolls pins)
  (dotimes (ii rolls (score game))
    (roll game pins)))

(defun roll-spare (game)
  (roll game 5)
  (roll game 5))

(defun roll-progression (game &key rolls)
  (loop :for pins :from 1 :to rolls
        :do (roll game pins)))

(nst:def-test-group bowling-kata-tests (game-instance)
       (:each-setup (setf game (make-instance 'game)))
       
  (nst:def-test all-gutter-balls-test (:equal 0)
    (roll-many game :rolls 20 :pins 0))
  
  (nst:def-test all-one-balls-test (:equal 20)
    (roll-many game :rolls 20 :pins 1))
  
  (nst:def-test spare-test (:equal 14)
    (progn
      (roll-spare game)
      (roll-progression game :rolls 2)
      (roll-many game :rolls 16 :pins 0)))
  
  (nst:def-test strike-test (:equal 19)
     (progn
       (roll game 10)
       (roll-progression game :rolls 3)
       (roll-many game :rolls 15 :pins 0)))
  
  (nst:def-test perfect-game (:equal 300)
     (roll-many game :rolls 12 :pins 10)))
