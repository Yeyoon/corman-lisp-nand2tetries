;;
;; Author : Moment
;; Date : 2018/5/15
;;


;; TOOLS FOR PROCESS READERS 

;; Split string
(defun split (str &optional delimiter)
    "Split string by delimiter."
    (let ((string (string-trim " " str)))
        (if (string= string "")
            NIL
            (let ((index (search  (or delimiter " ") string)))
            (if index
                (append (list (subseq string 0 index)) (split (subseq string (+ 1 index)) delimiter))
                (list string))))))

;; for /* */ comments
(defvar *in-comments* NIL)

;; Lexical elements

;; 1. keyword
(defvar *keywords* 
    '("class" "constructor" "function" "method"
      "field" "static" "var" "int" "char" "boolena"
      "void" "true" "false" "null" "this" "let" 
      "do" "if" "else" "while" "return"))

(defun keyword? (key)
    (member key *keywords* :test #'equal))

;; 2. symbol
(defvar *symbols*
    '("{" "}" "(" ")" "[" "]" "." ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"))

(defun symbol? (sym) (member sym *symbols* :test #'equal))

;; integerConstant
(defun integerConstant? (intstr)
    (handler-case
        (let ((v (parse-integer intstr)))
            (and (>= v 0) (<= v 32767) intstr))
        (error NIL)))

;; stringConstant
(defun stringConstant? (strstr)
    (let* ((s (string-trim " " strstr))
           (ll (length s)))
        (and (char= (char s 0) #\")
             (char= (char s (- ll 1)) #\"))))

;; identifier
(defun identifier? (str)
    (let ((ch (char str 0)))
        (not (member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))))

;;; 
;; codes for generate tokens
;;
       
(defstruct token type value)


;; 
;; build token
;;
(defun build-token (token)
  (cond ((symbol? token) (make-token :type 'symbol :value token))
	((keyword? token) (make-token :type 'keyword :value token))
	((integerConstant? token) (make-token :type 'integerConstant :value token))
	((stringConstant? token) (make-token :type 'stringConstant :value token))
	((identifier? token) (make-token :type 'identifier :value token))
	(T (format T "Unknown token ~a~%" token))))


(defun start-comment? (token) 
  (search "/*" token))

(defun end-comment? (token)
  (let ((index (search "*/" token)))
    (when index
      (+ index 2))))

(defun is-line-comment? (line)
  (and (>= (length line) 2)
       (char= #\/ (char line 0))
       (char= #\/ (char line 1))))

(defun process-token (token)
  (if *in-comments*
      (let ((index (end-comment? token)))
	(when index
	  (setf *in-comments* NIL)
	  (when (< index (length token))
	    (build-token (subseq token index)))))
      (let ((index (start-comment? token)))
	(if index
	  (progn
	    (setf *in-comments* T)
	    (when (> index 0)
	      (build-token (subseq token 0 index))))
	  (build-token token)))))

(defun process-token-list (str-list)
  (loop for token in str-list
        for new-token = (process-token token)
     when new-token
     collect new-token))

(defun remove-line-comment (line)
  (subseq line 0 (search "//" line)))

;;
;; process a new line
;;

(defun process-line (line)
  (if *in-comments*
      (process-token-list (split line))
      (when (not (is-line-comment? line))
	(process-token-list (split (remove-line-comment line))))))
	  
	 
    

;;;
;;; TESTING CASES
;;;
(progn
(setf *in-comments* NIL)
(process-line " sgsa /*sgjsa */sljas //sjgalsk "))
	
    
