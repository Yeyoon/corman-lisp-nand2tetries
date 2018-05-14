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

(defun demul-command (command)
    "Demul the command to (TYPE ARG1 ARG2).
    If do not have the field, just set it to NIL."
    (let ((a (split command)))
        (destructuring-bind (type arg1 arg2) a
            (values type arg1 arg2))))
     
(defun push? (op) (string= "push" op))
(defun pop? (op) (string= "pop" op))       
(defun add? (op)(string= "add" op))
(defun sub? (op)(string= "sub" op))
(defun neg? (op)(string= "neg" op))
(defun eq? (op) (string= "eq" op))
(defun gt? (op)(string= "gt" op))
(defun lt? (op) (string= "lt" op))
(defun and? (op) (string= "and" op))
(defun or? (op) (string= "or" op))
(defun not? (op) (string= "not" op))
(defun label? (op) (string= "label" op))
(defun if-goto? (op) (string= "if-goto" op))
(defun goto? (op) (string= "goto" op))

(defvar *true* -1)
(defvar *false* 0)
(defvar *inner-label-cnt* 0)
(defvar *current-function-name* "")

(defun assember-2-args-op-asm (op)
    (list "@SP" "A=M-1" "D=M" "A=A-1" (concatenate 'string "M=M" op "D") "@SP" "M=M-1"))  

(defun assember-1-arg-op-asm (op)
    (list "@SP" "A=M-1" (concatenate 'string "M=" op "M")))  

(defun assember-push-segment-asm (&optional reg)
    "User who call this function must be sure that
    the value which you will pushed stored in register reg.
    if reg is not given the register D is default choice."
    (list "@SP" "A=M" (or (and reg (concatenate 'string "M=" reg)) "M=D") "@SP" "M=M+1"))

(defun assember-pop-segment-asm ()
    "User who call this function must be sure that
    the dest addr is stored in register R6."
    (list "@SP" "A=M-1" "D=M" "@R6" "A=M" "M=D" "@SP" "M=M-1"))


(defun assember-a-addr (segment index)
    (cond ((string= segment "temp") (list (concatenate 'string "@" (write-to-string (+ (parse-integer index) 5))) "D=A"))
          ((string= segment "pointer") (list (concatenate 'string "@" (write-to-string (+ (parse-integer index) 3))) "D=A"))
          ((string= segment "constant") (list (concatenate 'string "@" index) "D=A"))
          (T (list (concatenate 'string "@" (get-seg segment)) "D=M" (concatenate 'string "@" index) "AD=D+A"))))

(defun assember-a-addr-R6 (segment index)
    (append (assember-a-addr segment index)
        (list "@R6" "M=D")))

(defun assember-a-addrvalue-D (segment index)
    (if (string= "constant" segment)
        (assember-a-addr segment index)
        (append (assember-a-addr segment index)
         (list "D=M"))))

(defun write-codes-to-string (codes)
    (with-output-to-string (out)
        (dolist (var codes)
            (write-line var out)))) 
;;
;; Gen the codes which using 2 args
;; the first is D, the second is M
;; such as: add/sub/lt/gt/eq
;;


(defun gen-add-asm () (assember-2-args-op-asm "+"))
(defun gen-sub-asm () (assember-2-args-op-asm "-"))
(defun gen-and-asm () (assember-2-args-op-asm "&"))
(defun gen-or-asm  () (assember-2-args-op-asm "|"))


(defun get-label (&optional new)
    (progn
        (when new
            (setf *inner-label-cnt* (+ 1 *inner-label-cnt*)))
        (concatenate 'string ".inner_label_" (write-to-string (- *inner-label-cnt* 1)))))

        

(defun gen-boolean-asm (jmp-asm-code)
    (let ((new-label (get-label 1)))
        (list 
            "@SP" 
            "A=M-1" 
            "D=M" 
            "A=A-1" 
            "D=M-D" 
            "M=-1" 
            "@SP"
            "M=M-1"
            (concatenate 'string "@" new-label) 
            (concatenate 'string "D;" jmp-asm-code) 
            "@SP"
            "A=M-1"
            "M=0" 
            (concatenate 'string "(" new-label ")"))))
     
(defun gen-eq-asm () (gen-boolean-asm "JEQ"))
(defun gen-gt-asm () (gen-boolean-asm "JGT"))
(defun gen-lt-asm () (gen-boolean-asm "JLT"))
      
(defun gen-neg-asm () (assember-1-arg-op-asm "-"))
(defun gen-not-asm () (assember-1-arg-op-asm "!"))

(defun build-label (string)
    (concatenate 'string *current-function-name* "$" string))
      
(defun gen-label-asm (string) (list (concatenate 'string "(" (build-label string) ")")))
(defun gen-if-goto-asm (label) 
    (list "@SP" "A=M-1" "D=M" "@SP" "M=M-1" (concatenate 'string "@" (build-label label)) "D;JNE"))
(defun gen-goto-asm (label)
    (list (concatenate 'string "@" (build-label label)) "0;JMP"))
 
;; gen the A address asm
;; segment : string
;; index   : string

(defun gen-push-asm (segment index)
    (append (list (concatenate 'string "// " "push " segment " " index))
            (assember-a-addrvalue-D segment index)
            (assember-push-segment-asm)))

;; using R6 for temp using
(defun gen-pop-asm (segment index)
    (append (list (concatenate 'string "// pop " segment " " index))
        (assember-a-addr-R6 segment index)
        (assember-pop-segment-asm)))

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

(defun genasm (op arg1 arg2)
    "The basic function to use gen asm codes."
    (cond ((push? op) (gen-push-asm arg1 arg2))
          ((pop? op) (gen-pop-asm arg1 arg2))
          ((label? op) (gen-label-asm arg1))
          ((if-goto? op) (gen-if-goto-asm arg1))
          ((goto? op) (gen-goto-asm arg1))
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
                    (write-string (write-codes-to-string (genasm type arg1 arg2)) stream))))))
                
        



    


    