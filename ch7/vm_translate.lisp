;; 
;; Author: Moment
;; Date: 2018/5/10
;; version: 1.0.0
;;
;; vm translator

(defun statck-op (op segment-or-var &optional index)
    "The Stack push/pop implementation.
    op: push/pop
    segment-or-var: push :static,local,argument,this,that,constant or pointer
                    pop  : variable
    index : the index of the segment."
    (let ((top 0))
        (if (string= op "push")
            (progn
                (translate-push segment-or-var index top)
                (setf top (+ 1 top)))
            (progn
                (setf top (- top 1))
                (translate-pop segment-or-var top)))))
                
        
    
    