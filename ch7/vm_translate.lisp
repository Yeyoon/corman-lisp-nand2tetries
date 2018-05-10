;; 
;; Author: Moment
;; Date: 2018/5/10
;; version: 1.0.0
;;
;; vm translator
(defvar *stack-top* 256)

(defun gen-push-asm (segment index)
    (with-output-to-string (out)
        (write-line (concatenate 'string "@" (write-to-string (+ segment index))))
        (write-line "D=M")
        (write-line (concatenate 'string "@" (write-to-string *stack-top*)))
        (setf *stack-top* (+ 1 *stack-top*))
        (write-string "M=D")))

(defun gen-pop-asm (segment index)
    (with-output-to-string (out)
        (setf *statck-top* (- *stack-top* 1))
        (write-line (concatenate 'string "@" (write-to-string *stack-top*)))
        (write-line "D=M")
        (write-line (concatenate 'string "@" (write-to-string (+ segment index))))
        (write-string "M=D")))

(defun gen-normal-asm (op)
    (format T "noraml operation op : ~a~%" op))

(defun genasm (op &rest args)
    "The basic function to use gen asm codes."
    (cond ((push? op) (gen-push-asm (first args) (second args)))
          ((pop? op) (gen-pop-asm (first args) (second args)))
          (T (gen-normal-asm op))))
          

(defun statck-op (op segment-or-var &optional index)
    "The Stack push/pop implementation.
    op: push/pop
    segment-or-var: push :static,local,argument,this,that,constant or pointer
                    pop  : variable
    index : the index of the segment."
    (let ((top 0))
        (if (string= op "push")
            (progn
                (genasm 'push segment-or-var index top)
                (setf top (+ 1 top)))
            (progn
                (setf top (- top 1))
                (genasm 'pop segment-or-var top)))))
                
        
    
    