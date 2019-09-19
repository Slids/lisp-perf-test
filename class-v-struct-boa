(defstruct (simple-struct                                                                                                            
             (:constructor make-simple-struct                                                                                        
                           (slot-1 slot-2 slot-3)))                                                                                  
  (slot-1)                                                                                                                           
  (slot-2)                                                                                                                           
  (slot-3))                                                                                                                          
                                                                                                                                     
(defclass simple-class ()                                                                                                            
  ((slot-1 :initarg :slot-1)                                                                                                         
   (slot-2 :initarg :slot-2)                                                                                                         
   (slot-3 :initarg :slot-3)))                                                                                                       
                                                                                                                                     
(defun set-random-class (my-class slot-number value)                                                                                 
  (ecase slot-number                                                                                                                 
    (0 (setf (slot-value my-class 'slot-1)                                                                                           
             value))                                                                                                                 
    (1 (setf (slot-value my-class 'slot-2)                                                                                           
             value))                                                                                                                 
    (2 (setf (slot-value my-class 'slot-3)                                                                                           
             value))))                                                                                                               
                                                                                                                                     
(defun set-random-struct (my-struct slot-number value)                                                                               
  (ecase slot-number                                                                                                                 
    (0 (setf (simple-struct-slot-1 my-struct)                                                                                        
             value))                                                                                                                 
    (1 (setf (simple-struct-slot-2 my-struct)                                                                                        
             value))                                                                                                                 
    (2 (setf (simple-struct-slot-3 my-struct)                                                                                        
             value))))                                                                                                               
                                                                                                                                     
(defun run-set-test (run-times)                                                                                                      
  (let ((my-class-instance (make-instance 'simple-class))                                                                            
        (my-struct-instance (make-simple-struct nil nil nil)))                                                                       
    (time                                                                                                                            
     (loop for i from 1 to run-times do                                                                                              
          (set-random-class my-class-instance (mod i 3) i)))                                                                         
    (time                                                                                                                            
     (loop for i from 1 to run-times do                                                                                              
          (set-random-struct my-struct-instance (mod i 3) i)))))                                                                     
                                                                                                                                     
(defun run-instantiate-test (run-times)                                                                                              
  (time                                                                                                                              
   (loop for i from 1 to run-times do                                                                                                
        (make-instance 'simple-class :slot-1 i                                                                                       
                       :slot-2 i                                                                                                     
                       :slot-3 i)))                                                                                                  
  (time                                                                                                                              
   (loop for i from 1 to run-times do                                                                                                
        (make-simple-struct i i i ))))                                                                                               
                                                                                                                                     
(defun run-instantiate-and-set-test (make-times set-times)                                                                           
  (time                                                                                                                              
   (loop for i from 1 to make-times do                                                                                               
        (let ((my-class                                                                                                              
               (make-instance 'simple-class :slot-1 i                                                                                
                              :slot-2 i                                                                                              
                              :slot-3 i)))                                                                                           
          (loop for i from 1 to set-times do                                                                                         
               (set-random-class my-class (mod i 3) i)))))                                                                           
  (time                                                                                                                              
   (loop for i from 1 to make-times do                                                                                               
        (let ((my-struct (make-simple-struct i i i)))                                                                                
          (loop for i from 1 to set-times do                                                                                         
               (set-random-struct my-struct (mod i 3) i))))))                                                                        
                                                                                                                                     
(defun run-test-suite ()                                                                                                             
  (let ((num-set-runs 1000000000)                                                                                                    
        (num-instantiate-runs 10000000))                                                                                             
    (run-set-test num-set-runs)                                                                                                      
    (run-instantiate-test num-instantiate-runs)                                                                                      
    (run-instantiate-and-set-test 1000 100000)))                                                                                     
                                                                                                                                     
(run-test-suite)                                                                                                                     
