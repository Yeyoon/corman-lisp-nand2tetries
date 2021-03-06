;;
;; Tools utilies START
;; ===================================================================
;;
(defun get-value-by-key (key table)
    (dolist (var table)
        (when (equal (first var) key)
            (return (cdr var)))))

;; if the command is string of digitals return the number
;; otherwise return NIL
(defun digital? (command)
    "Return if the command is a string contains all numbers."
    (handler-case (parse-integer command)
        (error NIL)))

(defun command-string (command)
    "Trans the interger to binary string."
   (format NIL "~B" (parse-integer command)))

(defun remove-comment (line)
    "Return NIL if the line is a comment.
    Remove the comments from line."
    (subseq line 0 (search "//" line)))

;; This funciton is copyed from http://cl-cookbook.readthedocs.io/zh_CN/latest/strings.html
(defun replace-all (string part replacement &key (test #'char=))
    "Returns a new string in which all the occurences of the part
    is replaced with replacement."
    (with-output-to-string (out)
        (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string :start2 old-pos :test test)
            do (write-string string out :start old-pos :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun remove-white-space (line)
    "Remove the wihte space in line."
    (replace-all line " " ""))

(defun remove-useless (line)
    "Remove the useless parts from line.
    the comments and the white spaces."
    (let ((r (remove-white-space (remove-comment line))))
        (when (and (string/= r "") (string/= r " "))
            r)))

(defun read-all-lines (file-name)
    "Read and remove all comments from file-name."
    (let ((r NIL) (rom-index 0))
        (progn
            (with-open-file (stream file-name :direction :input :if-does-not-exist nil)
                (when stream
                    (loop for line = (read-line stream nil)
                        while line do 
                            (let ((l (remove-useless line)))
                             (when l
                                   ;; do first pass here
                                   (setf rom-index (first-process-label l rom-index))
                                   (setf r (append r (list l))))))))
             r)))

;; Tools Utilities End
;;=======================================================================


;;========================================================================
;; DEFINE  symbol table module START
;;
(defun init-symbol-table ()
    "Add the SPECIAL FORM to symbol table."
    (progn
        (add-entry "SP" 0)
        (add-entry "LCL" 1)
        (add-entry "ARG" 2)
        (add-entry "THIS" 3)
        (add-entry "THAT" 4)
        (add-entry "R0" 0)
        (add-entry "R1" 1)
        (add-entry "R2" 2)
        (add-entry "R3" 3)
        (add-entry "R4" 4)
        (add-entry "R5" 5)
        (add-entry "R6" 6)
        (add-entry "R7" 7)
        (add-entry "R8" 8)
        (add-entry "R9" 9)
        (add-entry "R10" 10)
        (add-entry "R11" 11)
        (add-entry "R12" 12)
        (add-entry "R13" 13)
        (add-entry "R14" 14)
        (add-entry "R15" 15)
        (add-entry "SCREEN" 16384)
        (add-entry "KBD" 24576)))

(defvar *symbol-table* (make-hash-table :test 'equal))

(defun contain? (symbol)
    "Check if *symbol-table* contains the symbol."
    (gethash symbol *symbol-table*))

(defun add-entry (symbol val)
    "Add the pair (symbol val) to the *symbol-table*."
    (setf (gethash symbol *symbol-table*) val))

(defun get-address (symbol)
    "Get the address of the symbol from *symbol-table*."
    (contain? symbol))

;;
;; DEFINE symbol module table END
;;===========================================================================


;;
;; DEBUG tools START
;;===========================================================================
(defvar *debug* NIL)

(defun debug-write-codes (codes stream)
    (progn
        (when *debug*
            (format T "wirting codes:[~a]~%" codes)
            (setf *debug* NIL))
        (write-line codes stream)))


;;
;; DEBUG tools END
;;===========================================================================

;; ==========================================================================
;; CORE parser parts
;;===========================================================================
(defun L_COMMAND? (type)
    (eq type 'L_COMMAND))

(defun A_COMMAND? (type)
    (eq type 'A_COMMAND))

(defun C_COMMAND? (type)
    (eq type 'C_COMMAND))

(defun demul-type-content (command)
    "Return (Type content) of this command.
    Only in first function call."
    (let ((first-char (char command 0)))
        (cond ((eq first-char #\@) (values 'A_COMMAND (subseq command 1)))
              ((eq first-char (code-char 40)) 
                (values 'L_COMMAND 
                        (subseq command 1 
                            (let ((end (- (length command) 1)))
                                (when (>= end 0) end)))))
              (T (values 'C_COMMAND command)))))
                                                                                                
            
(defun dest-comp-jump (command)
    "Return the dest,comp and jump field of the command.
    Only if the command is C_COMMAND. dest=com;jump"
    (let ((index= (position-if (lambda (ch) (eq ch #\=)) command))
          (index2 (position-if (lambda (ch) (eq ch #\;)) command)))
        (values (and index= (subseq command 0 index=))
                (subseq command (or (and index= (+ 1 index=)) 0) index2)
                (and index2 (subseq command (+ 1 index2))))))


(defun first-process-label (label-command rom-index)
    (multiple-value-bind (type command) (demul-type-content label-command)
        (if (L_COMMAND? type)
            (progn
                (add-entry command rom-index)
                rom-index)
            (+ 1 rom-index))))

(defvar *dest-table*
    '((NIL . "000")
      ("M" . "001")
      ("D" . "010")
      ("MD" . "011")
      ("A" . "100")
      ("AM" . "101")
      ("AD" . "110")
      ("AMD" . "111")))

(defvar *jump-table*
    '((NIL . "000")
      ("JGT" . "001")
      ("JEQ" . "010")
      ("JGE" . "011")
      ("JLT" . "100")
      ("JNE" . "101")
      ("JLE" . "110")
      ("JMP" . "111")))

(defvar *comp-table*
    '(("0" . "0101010")
      ("1" . "0111111")
      ("-1" . "0111010")
      ("D" . "0001100")
      ("A" . "0110000")
      ("M" . "1110000")
      ("!D" . "0001101")
      ("!A" . "0110001")
      ("!M" . "1110001")
      ("-D" . "0001111")
      ("-A" . "0110011")
      ("-M" . "1110011")
      ("D+1" . "0011111")
      ("A+1" . "0110111")
      ("M+1" . "1110111")
      ("D-1" . "0001110")
      ("A-1" . "0110010")
      ("M-1" . "1110010")
      ("D+A" . "0000010")
      ("D+M" . "1000010")
      ("D-A" . "0010011")
      ("D-M" . "1010011")
      ("A-D" . "0000111")
      ("M-D" . "1000111")
      ("D&A" . "0000000")
      ("D&M" . "1000000")
      ("D|A" . "0010101")
      ("D|M" . "1010101")))

(defun assember-c-command (command)
    "Trans the c-command to binary bits."
    (multiple-value-bind (dest comp jump) (dest-comp-jump command)
         (concatenate 'string "111"
            (get-value-by-key comp *comp-table*)
            (get-value-by-key dest *dest-table*)
            (get-value-by-key jump *jump-table*))))



(defun assember-a-command (command)
    "Trans the a-command to binnary bits string."
    (let* ((str (command-string command))
           (len (- 15 (length str))))
        (with-output-to-string (out)
            (progn
                (write-string "0" out)
                (dotimes (i len)
                    (write-string "0" out))
                (write-string str out))))) 
    
(defun second-pass (commands file-name)
  "Produce the binary strings."
  (let ((ram-index 16))
    (with-open-file (stream file-name :direction :output :if-exists
                     :supersede)
      (dolist (c commands)
        ;(format T "CURRENT PROCESS COMMAND : ~a~%" c)
        (multiple-value-bind (type command) (demul-type-content c)
          (cond ((l_command? type) (format t "L_COMMAND : ~a~%" c))
                ((a_command? type)
                 (if (digital? command)
                     (debug-write-codes (assember-a-command command) stream)
                     (let ((addr (get-address command)))
                       (if addr
                           (write-line (assember-a-command (write-to-string addr)) stream)
                           (progn 
                                  (format T "command ~a ==> add new symbol ~a to ram index ~a~%" c command ram-index)
                                  (setf *debug* 1) 
                                  (add-entry command ram-index)
                                  (debug-write-codes (assember-a-command (write-to-string ram-index)) stream)
                                  (setf ram-index (+ 1 ram-index))
                                            )))))
                ((c_command? type)
                 (debug-write-codes (assember-c-command command) stream))
                (t (format t "Error unknown command : ~a~%" c))))))))
;;
;; CORE parser parts END
;;

;;==========================================================================================
;; this is the main routing for the program
(defun assember (&optional filename)
    "This is the toplevel funciton of this program."
    (progn
        (init-symbol-table)
        (let ((source-file (or filename (read))))
            (when source-file
                (format T "source-file is ~a~%" source-file)
                (let* ((p (position-if (lambda (ch) (char= ch #\.)) source-file))
                       (dest-file-name (concatenate 'string (subseq source-file 0 p) ".hack"))
                       (commands (read-all-lines source-file)))
                    (progn
                       (format T "start first pass commands : ~a~%" commands)
                       (format T "after first-pass~%")
                       (second-pass commands dest-file-name))
                       (format T "Process End~%"))))))
                
                            
                
