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
         (prog1
             (setf (aref rolls index) pins)
           (incf index))))))

(defgeneric sum-of (game index count)
  (:method ((game game) (index integer) (count integer))
     (let ((rolls (rolls game)))
       (loop :for ii :from 0 :below count
             :summing (aref rolls (+ ii index))))))

(defgeneric normal-score (game index)
  (:method ((game game) (index integer))
   (sum-of game index 2)))

(defgeneric spare-p (game index)
  (:method ((game game) (index integer))
     (= 10 (sum-of game index 2))))

(defgeneric spare-score (game index)
  (:method ((game game) (index integer))
     (sum-of game index 3)))

(defgeneric strike-p (game index)
  (:method ((game game) (index integer))
     (= 10 (aref (rolls game) index))))

(defgeneric strike-score (game index)
  (:method ((game game) (index integer))
     (sum-of game index 3)))

(defgeneric score (game)
  (:method ((game game))
      (loop :with index = 0
            :for frame :from 1 :to 10
            :summing (cond
                      ((strike-p game index)
                          (prog1
                              (strike-score game index)
                            (incf index 1)))
                      
                      ((spare-p game index)
                          (prog1
                              (spare-score game index)
                            (incf index 2)))
                      
                      (t  (prog1
                              (normal-score game index)
                            (incf index 2)))))))

(ql:quickload :nst)

(nst:def-fixtures game-instance ()
   (game))

(defgeneric roll-many (game &key initial rolls pins)
  (:method ((game game) &key initial rolls pins)
     (mapc (lambda (pins)
             (roll game pins))
           initial)
     (dotimes (ii rolls (score game))
       (roll game pins))))

(nst:def-test-group bowling-game-kata-tests (game-instance)
        (:each-setup (setf game (make-instance 'game)))
        
  (nst:def-test all-gutter-balls-test (:equal 0)
     (roll-many game :rolls 20 :pins 0))
  
  (nst:def-test all-one-balls-test (:equal 20)
     (roll-many game :rolls 20 :pins 1))
  
  (nst:def-test spare-test (:equal 14)
     (roll-many game :initial '(5 5 1 2) :rolls 16 :pins 0))
  
  (nst:def-test strike-test (:equal 19)
     (roll-many game :initial '(10 1 2 3) :rolls 15 :pins 0))
  
  (nst:def-test perfect-game-test (:equal 300)
     (roll-many game :rolls 12 :pins 10)))
