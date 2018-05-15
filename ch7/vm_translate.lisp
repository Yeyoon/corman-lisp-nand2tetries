;; 
;; Author: Moment
;; Date: 2018/5/10
;; version: 1.0.0
;;
;; vm translator
;;
;; using R6 for ARG1 
;; using R7 for ARG2
(use-package 'pathnames)

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
(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

(defvar *segment-table* 
    '(("local" "LCL")
      ("argument" "ARG")
      ("this" "THIS")
      ("that" "THAT")
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
(defun function? (op) (string= "function" op))
(defun call? (op) (string= "call" op))
(defun return? (op) (string= "return" op))

(defvar *true* -1)
(defvar *false* 0)
(defvar *inner-label-cnt* 0)
(defvar *current-function-name* "")
(defvar *current-filename* "")

(defun set-current-filename (filename)
    (setf *current-filename* filename))

(defvar *static-map-table* NIL)
(defun get-static-segment-1 (filename)
    (dolist (var *static-map-table*)
        (when (string= (first var) filename)
            (return (second var)))))

(defun add-new-static-segment (filename)
    (let ((len (length *static-map-table*)))
        (progn
            (setf *static-map-table* (append *static-map-table* (list (list filename (+ (* 2 len) 16)))))
            (+ (* 2 len) 16))))
 
(defun get-static-segment ()
    (let ((v  (get-static-segment-1 *current-filename*)))
        (or v
            (add-new-static-segment *current-filename*))))

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
          ((string= segment "static") (list (concatenate 'string "@" (write-to-string (+ (parse-integer index) (get-static-segment) ))) "D=A"))
          (T (list (concatenate 'string "@" (get-seg segment)) "D=M" (concatenate 'string "@" index) "AD=D+A"))))

(defun assember-a-addr-R6 (segment index)
    (append (assember-a-addr segment index)
        (list "@R6" "M=D")))

(defun assember-a-addrvalue-D (segment index)
    (let ((f1 (assember-a-addr segment index)))
        (cond ((member segment (list "constant") :test #'equal) f1)
              (T (append f1 (list "D=M"))))))


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
(defun gen-goto-asm (label &key (need-build T))
    (list (concatenate 'string "@" (if need-build (build-label label) label)) "0;JMP"))

(defun gen-function-asm (function-name args-num)
    (when function-name
        (setf *current-function-name* function-name)
        (let ((local-instructions  (loop for x from 0 to (parse-integer args-num)
                for y = (gen-push-asm "constant" "0")
                while (< x (parse-integer args-num))
                collect y)))
            (flatten (append (list (concatenate 'string "(" function-name ")")) local-instructions)))))

(defun gen-call-asm (function-name args-num)
    (when function-name
        (flatten
            (append 
                (list (concatenate 'string "@" (get-label 1)) "D=A")
                (assember-push-segment-asm)
                (list "@LCL" "D=M")
                (assember-push-segment-asm)
                (list "@ARG" "D=M")
                (assember-push-segment-asm)
                (list "@THIS" "D=M")
                (assember-push-segment-asm)
                (list "@THAT" "D=M")
                (assember-push-segment-asm)
                (list (concatenate 'string "@" (write-to-string (+ 5 (parse-integer args-num))))
                      "D=A"
                      "@SP" "D=M-D" "@ARG" "M=D")
                (list "@SP" "D=M" "@LCL" "M=D")
                (gen-goto-asm function-name :need-build NIL)
                (list (concatenate 'string "(" (get-label) ")"))))))

(defun gen-return-asm ()
    (flatten
        (append
            (list "@LCL" "D=M" "@R13" "M=D") ;; R13 is for frame
            (list "@5" "D=A" "@R13" "A=M-D" "D=M" "@R14" "M=D") ;; R14 is for retAddr
            (gen-pop-asm "argument" "0") ;; *ARG = pop
            (list "@ARG" "D=M+1" "@SP" "M=D")
            (list "@R13" "M=M-1" "A=M" "D=M" "@THAT" "M=D")
            (list "@R13" "M=M-1" "A=M" "D=M" "@THIS" "M=D")
            (list "@R13" "M=M-1" "A=M" "D=M" "@ARG" "M=D")
            (list "@R13" "M=M-1" "A=M" "D=M" "@LCL" "M=D")
            (list "@R14" "A=M" "0;JMP"))))
 
;; gen the A address asm
;; segment : string
;; index   : string

(defun gen-push-asm (segment index)
    (append 
            (assember-a-addrvalue-D segment index)
            (assember-push-segment-asm)))

;; using R6 for temp using
(defun gen-pop-asm (segment index)
    (append
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
          ((function? op) (gen-function-asm arg1 arg2))
          ((call? op) (gen-call-asm arg1 arg2))
          ((return? op) (gen-return-asm))
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
        (if index
            (concatenate 'string (subseq filename 0 index) ".asm")
            (concatenate 'string filename "\\" (pathname-name filename) ".asm"))))       


(defun assember-file (filename)
    (let ((commands (read-all-commands filename)))
        (progn
            (set-current-filename filename)
            (loop for var in commands
                collect (multiple-value-bind (type arg1 arg2) (demul-command var)
                            (append
                                (list (concatenate 'string "// " var)) 
                                (genasm type arg1 arg2)))))))

(defun assember-directory (dir)
    (let ((files (directory (concatenate 'string dir "\\*.vm"))))
        (flatten 
            (loop for filename in files
                collect (assember-file (namestring filename))))))

(defun vm-translator (filename-or-dir)
    (if (probe-file filename-or-dir)
        (assember-file filename-or-dir)
        (assember-directory filename-or-dir)))

(defun init-stack (&optional stream)
    (progn
        (write-line "// SP = 256 " stream)
        (write-line "@256" stream)
        (write-line "D=A" stream)
        (write-line "@SP" stream)
        (write-line "M=D" stream)
        (write-line "// call Sys.init " stream)
        (let ((r (gen-call-asm "Sys.init" "0")))
            (dolist (var r)
                (write-line var stream)))))

;;
;;=====================================================================
;;The interface for processing
;;
(defun main (&optional filename)
    (let* ((f (or filename (read)))
           (of (build-output-filename f))
           (res (vm-translator f)))
        (with-open-file (stream of :direction :output :if-exists :supersede)
            (print res)
            (init-stack stream)
            (dolist (var res)
                (write-line var stream)))))
                
        



    


    