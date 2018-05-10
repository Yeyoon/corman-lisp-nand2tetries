;; 
;; Author: Moment
;; Date: 2018/5/10
;; version: 1.0.0
;;
;; vm translator
(defvar *stack-top* 256)

(defvar *segment-table* 
    '(("local" 1)
      ("argument" 2)
      ("this" 3)
      ("that" 4)
      ("pointer" 3)
      ("temp" 5)
      ("static" 16)
      ("constant" "constant")))

(defun demul-command (command)
    "Demul the command to (TYPE ARG1 ARG2).
    If do not have the field, just set it to NIL."
    (let* ((a (split command))
           (type (first a)))
        (cond ((C_PUSH? type) (valuse 'C_PUSH (get-segment (second a)) (parse-integer (third a))))
              ((C_POP? type) (values 'C_POP (get-segment (second a)) (parse-integer (third a))))
              ((C_LABEL? type) (values 'C_LABEL (second a) NIL))
              ((C_GOTO? type) (values 'C_GOTO (second a) NIL))
              ((C_IF? type) (values 'C_IF (second a) NIL))
              ((C_FUNCTION? type) (values 'C_FUNCTION (second a) NIL))
              ((C_RETURN? type) (values 'C_RETURN NIL NIL))
              ((C_CALL? type) (values 'C_CALL (second a) NIL))
              (T (values 'C_ARITHMETIC NIL NIL)))))





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
                
        
    
    