;;
;; Using MACRO to define structures
;;

;;
;; Author : Moment
;; Over write the parse.lisp
;; less cons for memory using
;;

;;
;; token struct
;;===============================
(defun symbol? (type) (equal type 'symbol))
(defun keyword? (type) (equal type 'keyword))
(defun integerConstant? (type) (equal type 'integerConstant))
(defun stringConstant? (type) (equal type 'stringConstant))
(defun identifier? (type) (equal type 'identifier))

(defun print-pair (stream pair value)
  (progn
    (fresh-line stream)
    (format stream "<~a>~a</~a>" pair value pair)))

(defun print-token (token stream depth)
  (when (token-p token)
    (let ((type (token-type token))
	  (value (token-value token)))
      (cond ((symbol? type) (print-pair stream "symbol" value))
	    ((keyword? type) (print-pair stream "keyword" value))
	    ((integerConstant? type) 
	     (print-pair stream "integerConstant" value))
	    ((stringConstant? type)
	     (print-pair stream "stringConstant" value))
	    ((identifier? type)
	     (print-pair stream "identifier" value))
	    (T (format T "Unknonw token ~a~%" value))))))

(defstruct (token (:print-function print-token))
  type
  value)

(defconstant +const-symbol-list+ 
  '("{" "}" "(" ")" "[" "]" "." ";" "+" "-" 
    "*" "/" "&" "|" "<" ">" "=" "~" ","))

(defconstant +const-keyword-list+
  '("class" "constructor" "function" "method"
    "field" "static" "var" "int" "char" "boolean"
    "void" "true" "false" "null" "this" "let" 
    "do" "if" "else" "while" "return"))
;;
;; for less cons predefined some constant token
;;
(defconstant +const-token-list+ 
  (append
   (loop for s in +const-symbol-list+
      collect (make-token :type 'symbol :value s)) 
   (loop for k in +const-keyword-list+
      collect (make-token :type 'keyword :value k))))


;;;
;;; interface for build token
;;;
(defun integerConstant-string? (str-token)
  "True if the str-token is an integerConstant.
  Otherwise return NIL."
  (handler-case 
      (let ((v (parse-integer str-token)))
	(and (>= v 0) (<= v 32767)))
    (error NIL)))

(defun stringConstant-string? (str-token)
  (and (>= (length str-token) 2)
       (char= #\" (char str-token 0))
       (char= #\" (char str-token (- (length str-token) 1) ))))

(defun identifier-string? (str-token)
  (let ((ch (char str-token 0)))
    (not (member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))))

(defun build-token-1 (str-token)
  "str-token must be an identifier or stringConstant or integerConstant.
   build a token from str-token."
  (cond ((integerConstant-string? str-token) 
	 (make-token :type 'integerConstant :value str-token))
	((stringConstant-string? str-token)
	 (make-token :type 'stringConstant :value (subseq str-token 1 (- (length str-token) 1))))
	((identifier-string? str-token)
	 (make-token :type 'identifier :value str-token))
	(T 
	 (format T 
		 "Come here something is WRONG. unknown string: ~a~%" 
		 str-token))))

(defun build-token (str-token)
  "build token from a string."
  (let ((token (find-if #'(lambda (v) (string= str-token (token-value v)))
			+const-token-list+)))
    (if token
	token
	(build-token-1 str-token))))

(defun =? (token str)
  (and (token-p token)
       (string= (token-value token) str)))


(defmacro define-struct (struct predecate &rest fields)
  (let* ((prints (concatenate 'string "print-" (string struct)))
	 (pf (intern (string-upcase prints)))
	 (m (concatenate 'string "make-" (string struct)))
	 (ms (intern (string-upcase m)))
	 (b (concatenate 'string "build-" (string struct)))
	 (bf (intern (string-upcase b))))
    `(progn
       (defstruct (,struct (:print-function ,pf))
	 ,@fields)

       (defun ,pf (s stream depth)
	 (progn
	   (fresh-line stream)
	   (dolist (x ',fields)
	     (let* ((xx (concatenate 'string (string ,struct) "-" (string x)))
		    (xxx (intern (string-upcase xx))))
	       (let ((v (xxx s)))
		 (cond ((token-p v)
			(format stream "~a" v))
		       ((listp v)
			(dolist (y v)
			  (format stream "~a" y)))
		       (T (format T "~%ERROR-PRINT~%"))))))))
       (defun ,bf (input-stream)
	 (when (,predecate input-stream)
	   (,ms
	    ,(mapcar fields
		     #'(lambda (field)
			 (let* ((x (concatenate 'string ":" (string field)))
				(sx (intern (string-upcase x)))
				(y (concatenate 'string "build-" (string field)))
				(bx (intern (string-upcase y))))
			   (list sx bx))))))))))




;;;
;;; testing 
;;;

;;
;; class
;; 'class' classVarName '{' classVarDec* subroutineDec* '}'
;;
(define-struct classs class?
  token-class
  classVarname
  token-{
  classVarDec*
  subroutineDec*
  token-})

(macroexpand-1 
 '(define-struct classs class?
  token-class
  classVarname
  token-1
  classVarDec*
  subroutineDec*
  token-2))
		     
			
