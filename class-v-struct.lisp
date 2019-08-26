(defstruct simple-struct
  (slot-1)
  (slot-2)
  (slot-3))

(defclass simple-class ()
  ((slot-1 :initarg :slot-1)
   (slot-2 :initarg :slot-2)
   (slot-3 :initarg :slot-3)))

(defun set-random-class (my-class)
  (ecase (random 3)
    (0 (setf (slot-value my-class 'slot-1)
	     (random 100)))
    (1 (setf (slot-value my-class 'slot-2)
	     (random 100)))
    (2 (setf (slot-value my-class 'slot-3)
	     (random 100)))))

(defun set-random-struct (my-struct)
  (ecase (random 3)
    (0 (setf (simple-struct-slot-1 my-struct)
	     (random 100)))
    (1 (setf (simple-struct-slot-2 my-struct)
	     (random 100)))
    (2 (setf (simple-struct-slot-3 my-struct)
	     (random 100)))))

(defun run-set-test (run-times)
  (let ((my-class-instance (make-instance 'simple-class))
	(my-struct-instance (make-simple-struct)))
    (time
     (loop for i from 1 to run-times do
       (set-random-class my-class-instance)))
    (time
     (loop for i from 1 to run-times do
       (set-random-struct my-struct-instance)))))

(defun run-instantiate-test (run-times)                                                                                                                                                                            
  (time                                                                                                                                                                                                            
   (loop for i from 1 to run-times do                                                                                                                                                                              
     (make-instance 'simple-class :slot-1 (random 10)                                                                                                                                                              
                                  :slot-2 (random 10)                                                                                                                                                              
                                  :slot-3 (random 10))))                                                                                                                                                           
  (time                                                                                                                                                                                                            
   (loop for i from 1 to run-times do                                                                                                                                                                              
     (make-simple-struct :slot-1 (random 10)                                                                                                                                                                       
                         :slot-2 (random 10)                                                                                                                                                                       
                         :slot-3 (random 10)))))                                                                                                                                                                   

(defun run-instantiate-and-set-test (make-times set-times)
  ;; (with-open-stream (*standard-output* (make-broadcast-stream))
  (time
   (loop for i from 1 to make-times do
     (let ((my-class (make-instance 'simple-class :slot-1 (random 10)
						  :slot-2 (random 10)
						  :slot-3 (random 10))))
       (loop for i from 1 to set-times do
	 (set-random-class my-class)))))
  (time
   (loop for i from 1 to make-times do
     (let ((my-struct (make-simple-struct :slot-1 (random 10)
					  :slot-2 (random 10)
					  :slot-3 (random 10))))
       (loop for i from 1 to set-times do
	 (set-random-struct my-struct))))))

(defun run-test-suite ()
  (let ((num-set-runs 1000000000)
	(num-instantiate-runs 10000000))
    (run-set-test num-set-runs)
    (run-instantiate-test num-instantiate-runs)
    (run-instantiate-and-set-test 1000 100000)))

(run-test-suite)
