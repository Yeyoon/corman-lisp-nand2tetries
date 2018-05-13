;; 
;; Author: Moment
;; Date: 2018/5/10
;; version: 1.0.0
;;
;; vm translator
;;
;; using R6 for ARG1 
;; using R7 for ARG2

(defun remove-comment (line)
    "Return NIL if the line is a comment.
    Remove the comments from line."
    (subseq line 0 (search "//" line)))

(defun remove-useless (line)
    "Remove the useless parts from line.
    the comments and the white spaces."
    (let ((r (string-trim " " (remove-comment line))))
        (when (and (string/= r "") (string/= r " "))
            r)))

(defun set-stack-top-asm ()
    (with-output-to-string (out)
        (write-line "@SP" out)
        (write-line (concatenate 'string "M=" (write-to-string *stack-top*)) out)))

(defvar *stack-top* 256)

(defun init-stack (stream)
    (write-line (set-stack-top-asm) stream))

(defvar *segment-table* 
    '(("local" "LCL")
      ("argument" "ARG")
      ("this" "THIS")
      ("that" "THAT")
      ("pointer" "R3")
      ("temp" "R5")
      ("static" "16")
      ("constant" NIL)))

(defun get-seg (segment)
    (dolist (var *segment-table*)
        (when (string= segment (first var))
            (return (second var)))))
    

(defun split (str &optional delimiter)
    "Split string by delimiter."
    (let ((string (string-trim " " str)))
        (if (string= string "")
            NIL
            (let ((index (search  (or delimiter " ") string)))
            (if index
                (append (list (subseq string 0 index)) (split (subseq string (+ 1 index)) delimiter))
                (list string))))))

(defun C_PUSH? (type)
    (string= "push" type))

(defun C_POP? (type)
    (string= "pop" type))

(defun C_LABEL? (type)
    (string= "label" type))

(defun C_GOTO? (type)
    (string= "goto" type))

(defun C_IF? (type)
    (string= "if-goto" type))

(defun C_FUNCTION? (type)
    (string= "function" type))

(defun C_RETURN? (type)
    (string= "return" type))

(defun C_CALL? (type)
    (string= "call" type))

(defun demul-command (command)
    "Demul the command to (TYPE ARG1 ARG2).
    If do not have the field, just set it to NIL."
    (let ((a (split command)))
        (destructuring-bind (type arg1 arg2) a
            (values type arg1 arg2))))
            

;; gen the A address asm
;; segment : string
;; index   : string
(defun gen-a-asm (segment index)
    (let ((seg (get-seg segment)))
        (with-output-to-string (out)
            (cond ((string= segment "temp") (write-line (concatenate 'string "@" (write-to-string (+ (parse-integer index) 5))) out))
                 ((string= segment "pointer") (write-line (concatenate 'string "@" (write-to-string (+ (parse-integer index) 3))) out))
                 (T (progn
                        (write-line (concatenate 'string "@" seg) out)
                        (write-line "D=M" out)
                        (write-line (concatenate 'string "@" index) out)
                        (write-line "AD=D+A" out)))))))

;; gen the constant asm
;; val : string
(defun gen-constant-asm (val)
    (with-output-to-string (out)
        (write-line (concatenate 'string "@" val) out)
        (write-line "D=A" out)))
        
(defun constant? (segment)
    (string= "constant" segment))

(defun gen-push-asm (segment index)
    (with-output-to-string (out)
        (write-line (concatenate 'string "// push " segment " " index ) out)
        (if (constant? segment)
            (write-line (gen-constant-asm index) out)
            (progn
                (write-line (gen-a-asm segment index) out)
                (write-line "D=M" out)))
        (write-string (gen-push-from-register-asm "D") out)))

(defun gen-base-push-asm (reg-or-val)
    (with-output-to-string (out)
        (write-line "@SP" out)
        (write-line "A=M" out)
        (write-line (concatenate 'string "M=" reg-or-val) out)
        (write-line "@SP" out)
        (write-line "M=M+1" out)))

(defun gen-push-from-register-asm (reg)
    (with-output-to-string (out)
        (write-string (gen-base-push-asm reg) out)))

;; using R6 for temp using
(defun gen-pop-asm (segment index)
    (with-output-to-string (out)
        (write-line (concatenate 'string "// pop " segment " " index) out)
        (write-string (gen-a-asm segment index) out)
        (write-line "D=A" out)
        (write-line "@R6" out)
        (write-line "M=D" out)        
        (write-string (gen-pop-to-register-asm "D") out)
        (write-line "@R6" out)
        (write-line "A=M" out)
        (write-line "M=D" out)))

(defun gen-normal-asm (op)
    (cond ((add? op) (gen-add-asm))
          ((sub? op) (gen-sub-asm))
          ((neg? op) (gen-neg-asm))
          ((eq? op) (gen-eq-asm))
          ((gt? op) (gen-gt-asm))
          ((lt? op) (gen-lt-asm))
          ((and? op) (gen-and-asm))
          ((or? op) (gen-or-asm))
          ((not? op) (gen-not-asm))
          (T (format T "unknonwn noraml operation op : ~a~%" op))))

(defun add? (op)(string= "add" op))
(defun sub? (op)(string= "sub" op))
(defun neg? (op)(string= "neg" op))
(defun eq? (op) (string= "eq" op))
(defun gt? (op)(string= "gt" op))
(defun lt? (op) (string= "lt" op))
(defun and? (op) (string= "and" op))
(defun or? (op) (string= "or" op))
(defun not? (op) (string= "not" op))

(defvar *true* -1)
(defvar *false* 0)
(defvar *inner-label-cnt* 0)

(defun gen-pop-to-addr-asm (addr)
    (with-output-to-string (out)
        (write-string (gen-pop-to-register-asm "D") out)
        (write-line (concatenate 'string "@" addr) out)
        (write-line "M=D" out)))


(defun gen-pop-to-register-asm (reg)
    (with-output-to-string (out)
        (write-line "@SP" out)
        (write-line "M=M-1" out)
        (write-line "@SP" out)
        (write-line "A=M" out)
        (write-line (concatenate 'string reg "=M") out)))

(defun gen-set-addr-to-a-asm (addr)
    "This function is used to gen asm such as:
    A <------ addr."
    (with-output-to-string (out)
        (write-line (concatenate 'string "@" addr) out)))
    
;;
;; Gen the codes which using 2 args
;; the first is D, the second is M
;; such as: add/sub/lt/gt/eq
;;
(defun gen-2-args-asm ()
    (with-output-to-string (out)
        (write-string (gen-pop-to-register-asm "D") out)
        (write-line "@SP" out)
         (write-line "M=M-1" out)
         (write-line "@SP" out)
         (write-line "A=M" out)))

;; using R6 for a
;; using D for b  
;; for a + b  
(defun gen-add-asm ()
    (with-output-to-string (out)
        (write-string (gen-2-args-asm) out)
        (write-line "D=D+M" out)
        (write-string (gen-push-from-register-asm "D") out)))

(defun gen-sub-asm ()
    (with-output-to-string (out)
        (write-string (gen-2-args-asm) out)
        (write-line "D=M-D" out)
        (write-string (gen-push-from-register-asm "D") out)))

(defun get-label (&optional new)
    (progn
        (when new
            (setf *inner-label-cnt* (+ 1 *inner-label-cnt*)))
        (concatenate 'string ".inner_label_" (write-to-string (- *inner-label-cnt* 1)))))

        

(defun gen-boolean-asm (jmp-asm-code)
    (let ((new-label (get-label 1)))
        (with-output-to-string (out)
            (write-string (gen-2-args-asm) out)
            (write-line "D=M-D" out)
            (write-string (gen-push-from-register-asm "-1") out)
            (write-line (concatenate 'string "@" new-label) out)
            (write-line (concatenate 'string "D;" jmp-asm-code) out)
            (write-string (gen-pop-to-register-asm "D") out)
            (write-string (gen-base-push-asm "0") out)
            (write-line (concatenate 'string "(" new-label ")") out))))
     
(defun gen-eq-asm () (gen-boolean-asm "JEQ"))
(defun gen-gt-asm () (gen-boolean-asm "JGT"))
(defun gen-lt-asm () (gen-boolean-asm "JLT"))


        
(defun gen-neg-asm ()
    (with-output-to-string (out)
        (write-string (gen-pop-to-register-asm "D") out)
        (write-string (gen-push-from-register-asm "-D") out)))

(defun gen-logic-asm (logic-op)
    (with-output-to-string (out)
        (write-string (gen-2-args-asm) out)
        (write-line (concatenate 'string "D=M" logic-op "D") out)
        (write-string (gen-push-from-register-asm "D") out)))

(defun gen-and-asm () (gen-logic-asm "&"))
(defun gen-or-asm () (gen-logic-asm "|"))
(defun gen-not-asm ()
    (with-output-to-string (out)
        (write-string (gen-pop-to-register-asm "D") out)
        (write-string (gen-push-from-register-asm "!D") out)))
      

(defun genasm (op arg1 arg2)
    "The basic function to use gen asm codes."
    (cond ((c_push? op) (gen-push-asm arg1 arg2))
          ((c_pop? op) (gen-pop-asm arg1 arg2))
          (T (gen-normal-asm op))))
          

(defun read-all-commands (filename)
    (let ((r NIL))
        (progn
            (with-open-file (stream filename :direction :input :if-does-not-exist nil)
                (when stream
                    (loop for line = (read-line stream nil)
                        while line do
                            (let ((l (remove-useless line)))
                                (when l
                                    (setf r (append r (list l))))))))
            r)))
                
 
(defun build-output-filename (filename)
    (let ((index (position-if #'(lambda (ch) (char= ch #\.)) filename)))
        (concatenate 'string (subseq filename 0 index) ".asm")))       

;;
;;=====================================================================
;;The interface for processing
;;
(defun vm-translator (&optional filename)
    (let* ((f (or filename (read)))
          (commands (read-all-commands f))
          (of (build-output-filename f)))
        (with-open-file (stream of :direction :output :if-exists :supersede)
            (dolist (var commands)
                ;;(fresh-line stream)
                ;;(write-line "// gen for vm command : " stream)
                ;;(write-string (concatenate 'string "// " (string-trim "\n" var)) stream)
                (multiple-value-bind (type arg1 arg2) (demul-command var)
                    (write-string (genasm type arg1 arg2) stream))))))
                
        
    
    