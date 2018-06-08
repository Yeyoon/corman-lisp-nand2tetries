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

(defun t-v (value)
  (cond ((string= value "<") "&lt;")
	((string= value ">") "&gt;")
	((string= value "&") "&amp;")
	(T value)))

(defun print-pair (stream pair value)
  (progn
    (fresh-line stream)
    (format stream "<~a>~a</~a>" pair (t-v value) pair)))

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

;;
;; Program structure
;;==========================================================

;; TESTING MACRO using in class
(defmacro define-struct (struct &optional head &rest fields)
  (let* ((pstr (concatenate 'string "print-" struct))
	 (pst (intern (string-upcase struct)))
	 (pfunc (intern (string-upcase pstr))))
    `(progn
       (defstruct (,pst (:print-function ,pfunc))
	 ,@fields)
       
       (defun ,pfunc (st stream depth)
	 (progn 
	   (when (stringp ,head)
	     (format stream (concatenate 'string "<" ,head ">")))
	   (dolist (var ',fields)
	     (let* ((str (concatenate 'string ,struct "-" (string var)))
		   (syr (intern (string-upcase str))))
	       (progn
		 (format T "str is ~a, sys is ~a~%" str syr)
	       (if (token-p (funcall syr st))
		   (format stream "~a~%" (funcall syr st))
		   (if (listp (funcall syr st))
		       (dolist (x (funcall syr st))
			 (format stream "~a" x))
		       (format stream "~a" (funcall syr st)))))))
	   (when (stringp ,head)
	     (format stream (concatenate 'string "</" ,head ">"))))))))

;;
;; class
;; 'class' classVarName '{' classVarDec* subroutineDec* '}'
;;
(defstruct (class-s (:print-function print-class))
  className
  (classVarDec* NIL)
  (subroutineDec* NIL))

(defun print-class (class stream depth)
  (progn
    (fresh-line stream)
    (format stream "<class>~%")
    (format stream "~a~%" (build-token "class"))
    (format stream "~a" (class-s-className class))
    (format stream "~a~%" (build-token "{"))
    (dolist (var (class-s-classVarDec* class))
      (format stream "~a" var))
    (dolist (var (class-s-subroutineDec* class))
      (format stream "~a" var))
    (format stream "~a~%" (build-token "}"))
    (format stream "</class>")))

(defun build-class (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= (token-value token) "class"))
      (consume-one-token input-stream)
      (let ((c (make-class-s
		:className (build-className input-stream)
		:classVarDec* (and
			       (consume-one-token input-stream :value "{")
			       (build-classVarDec* input-stream))
		:subroutineDec* (build-subroutineDec* input-stream))))
	(and (consume-one-token input-stream :value "}")
	     c)))))


;;
;; classVarDec
;; ('static'|'field') type varName (','varName)* ';'
;;
(defstruct (classVarDec (:print-function print-classVarDec))
  static-field
  type 
  varName)

(defun print-classVarDec (classVarDec stream depth)
  (progn
    (fresh-line stream)
    (format stream "<classVarDec>~%")
    (format stream "~a~%" (classVarDec-static-field classVarDec))
    (format stream "~a" (classVarDec-type classVarDec))
    (dolist (var (classVarDec-varName classVarDec))
      (format stream "~a" var)
      (when (not (equal var (first (last (classVarDec-varName classVarDec)))))
	(format stream "~a~%" (build-token ","))))
    (format stream "~a~%" (build-token ";"))
    (format stream "</classVarDec>")))

(defun build-classVarDec (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) 
	       (or (string= (token-value token) "static")
		   (string= (token-value token) "field")))
      (consume-one-token input-stream)
      (let ((r (make-classVarDec
		:static-field token
		:type (build-type input-stream)
		:varName (build-varName* input-stream))))
	(and (consume-one-token input-stream :value ";")
	     r)))))

(defun build-classVarDec* (input-stream)
  (let ((r NIL))
    (do ((x (build-classVarDec input-stream) (build-classVarDec input-stream)))
	((null x) r)
      (setf r (append r (list x))))))
      


;;
;; type
;; 'int' | 'char' | 'boolean'  | className
;;
(defstruct (types (:print-function print-type))
  type)

(defun print-type (type stream depth)
  (format stream "~a~%" (types-type type)))

(defun build-type (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (or (member (token-value token) '("int" "char" "boolean" "class") :test #'equal)
		   (equal (token-type token) 'identifier)))
      (consume-one-token input-stream)
      (make-types :type token))))



;;
;; subroutineDec
;; ('constructor' | 'function' | 'method') ('void' | type) 
;; subroutineName '(' parameterList ')' subroutineBody
;;
(defstruct (subroutineDec (:print-function print-subroutineDec))
  con-fun-method
  void-type
  subroutineName
  parameterList
  subroutineBody)


(defun print-subroutineDec (subroutineDec stream depth)
  (progn
    (fresh-line stream)
    (format stream "<subroutineDec>~%")
    (format stream "~a~%" (subroutineDec-con-fun-method subroutineDec))
    (format stream "~a" (subroutineDec-void-type subroutineDec))
    (format stream "~a" (subroutineDec-subroutineName subroutineDec))
    (format stream "~a~%" (build-token "("))
    (format stream "~a" (subroutineDec-parameterList subroutineDec))
    (format stream "~a~%" (build-token ")"))
    (format stream "~a" (subroutineDec-subroutineBody subroutineDec))
    (format stream "</subroutineDec>")))

(defun build-subroutineDec (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (member (token-value token) '("constructor" "function" "method") :test #'equal))
      (consume-one-token input-stream)
      (make-subroutineDec
       :con-fun-method token
       :void-type (build-void-type input-stream)
       :subroutineName (build-subroutineName input-stream)
       :parameterList (and (consume-one-token input-stream :value "(")
			   (build-parameterList input-stream))
       :subroutineBody (and (consume-one-token input-stream :value ")")
			    (build-subroutineBody input-stream))))))

(defun build-void-type (input-stream)
  (let ((token (next input-stream)))
    (when (token-p token)
      (if (string= (token-value token) "void")
	  (progn
	    (consume-one-token input-stream)
	    token)
	  (build-type input-stream)))))


(defun build-subroutineDec* (input-stream)
  (let ((r NIL))
    (do ((x (build-subroutineDec input-stream)
	    (build-subroutineDec input-stream)))
	((null x) r)
      (setf r (append r (list x))))))

;;
;; parameterList
;; ((type varName) (',' type varName)*)?
;;
(defstruct (parameterList (:print-function print-parameterList))
  type-varName)

(defun print-parameterList (parameterList stream depth)
  (progn
    (format stream "<parameterList>~%")
    (dolist (x (parameterList-type-varName parameterList))
      (format stream "~a" (first x))
      (format stream "~a" (second x))
      (when (not (equal x (first (last (parameterList-type-varName parameterList)))))
	(format stream "~a" (build-token ","))))
    (format stream "</parameterList>~%")))

(defun build-parameterList-1 (input-stream)
  (let ((type (build-type input-stream)))
    (when (Types-p type)
      (cons (list type (build-varName input-stream))
	    (let ((token (next input-stream)))
	      (when (and (token-p token) (string= (token-value token) ","))
		(consume-one-token input-stream)
		(build-parameterList-1 input-stream)))))))
		       
(defun build-parameterList (input-stream)
  (make-parameterList
   :type-varName (build-parameterList-1 input-stream)))


;;
;; subroutineBody
;; '{' varDec* statements '}'
;;
(defstruct (subroutineBody (:print-function print-subroutineBody))
  varDec*
  statements)

(defun print-subroutineBody (sbb stream depth)
  (progn
    (format stream "<subroutineBody>~%")
    (format stream "~a~%" (build-token "{"))
    (dolist (var (subroutineBody-varDec* sbb))
      (format stream "~a" var))
    (format stream "~a~%" (subroutineBody-statements sbb))
    (format stream "~a~%" (build-token "}"))
    (format stream "</subroutineBody>~%")))

(defun build-subroutineBody (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (string= (token-value token) "{"))
      (consume-one-token input-stream)
      (let ((r (make-subroutineBody
		:varDec* (build-varDec* input-stream)
		:statements (build-statements input-stream))))
	(progn
	  (consume-one-token input-stream :value "}")
	  r)))))


;;
;; varDec
;; 'var' type varName (',' varName)* ';'
;;
(defstruct (varDec (:print-function print-varDec))
  type
  varName)

(defun print-varDec (varDec stream depth)
  (progn
    (fresh-line stream)
    (format stream "<varDec>~%")
    (format stream "~a~%" (build-token "var"))
    (format stream "~a" (varDec-type varDec))
    (dolist (v (varDec-varName varDec))
      (format stream "~a" v)
      (when (not (equal (first (last (varDec-varName varDec))) v))
	(format stream "~a~%" (build-token ","))))
    (format stream "~a~%" (build-token ";"))
    (format stream "</varDec>~%")))

(defun build-varDec (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= (token-value token) "var"))
      (consume-one-token input-stream)
      (let ((r (make-varDec
		:type (build-type input-stream)
		:varName (build-varName* input-stream))))
	(and (consume-one-token input-stream :value ";")
	     r)))))

(defun build-varDec* (input-stream)
  (let ((r NIL))
    (do ((x (build-varDec input-stream) (build-varDec input-stream)))
	((null x) r)
      (setf r (append r (list x))))))


;;
;; className
;; identifier
(defstruct (className (:print-function print-className))
  name)

(defun print-className (cname stream depth)
  (format stream "~a~%" (className-name cname)))

(defun build-className (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (equal 'identifier (token-type token)))
      (consume-one-token input-stream)
      (make-className :name token))))


;;
;; subroutineName
;; identifier
;;
(defstruct (subroutineName (:print-function print-subroutineName))
  name)

(defun print-subroutineName (subname stream depth)
  (format stream "~a~%" (subroutineName-name subname)))

(defun build-subroutineName (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (equal 'identifier (token-type token)))
      (consume-one-token input-stream)
      (make-subroutineName :name token))))

;;
;; varName
;; identifier
;;
(defstruct (varName (:print-function print-varName))
  name)

(defun print-varName (varName stream depth)
  (format stream "~a~%" (varName-name varName)))

(defun build-varName (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (equal 'identifier (token-type token)))
      (consume-one-token input-stream)
      (make-varName :name token))))


(defun build-varName* (input-stream)
  (let ((v (build-varName input-stream)))
    (when v
      (cons v
	    (let ((token (next input-stream)))
	      (when (and (token-p token)
			 (string= (token-value token) ","))
		(consume-one-token input-stream)
		(build-varName* input-stream)))))))



;;;
;;; statements
;;; ================================================================
;;;


;;
;; statemnets
;; statement*
;;
(defstruct (statements (:print-function print-statements))
  statement*)

(defun print-statements (statements stream depth)
  (progn
    (format stream "<statements>~%")
    (dolist (var (statements-statement* statements))
      (format stream "~a~%" var))
    (format stream "</statements>")))

(defun build-statements (input-stream)
  (make-statements 
   :statement*
   (let ((s NIL))
     (do ((x (build-statement input-stream) (build-statement input-stream)))
	 ((null x) s)
       (setf s (append s (list x)))))))


;;
;; statement
;; letStatement | ifStatement | whileStatement | doStatement | returnStatement

;; staemetn is a fack struct 
;; never to build

(defun build-statement (input-stream)
  (or (build-ifStatement input-stream)
      (build-letStatement input-stream)
      (build-whileStatement input-stream)
      (build-doStatement input-stream)
      (build-returnStatement input-stream)))


;;
;; letStatement
;; 'let' varName ('[' expression ']')? '=' expression ';'
;;
(defstruct (letStatement (:print-function print-letStatement))
  varName
  array-expression
  expression)

(defun print-letStatement (lst stream depth)
  (progn
    (format stream "<letStatement>~%")
    (format stream "~a~%" (build-token "let"))
    (format stream "~a" (letStatement-varName lst))
    (when (letStatement-array-expression lst)
      (format stream "~a~%" (build-token "["))
      (format stream "~a~%" (letStatement-array-expression lst))
      (format stream "~a~%" (build-token "]")))
    (format stream "~a~%" (build-token "="))
    (format stream "~a~%" (letStatement-expression lst))
    (format stream "~a~%" (build-token ";"))
    (format stream "</letStatement>")))

(defun build-letStatement (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= "let" (token-value token)))
      (consume-one-token input-stream)
      (let ((r
	     (make-letStatement 
	      :varName (build-varName input-stream)
	      :array-expression (build-array-expression input-stream)
	      :expression (and (consume-one-token input-stream :value "=")
			       (build-expression input-stream))
	      )))
	(progn
	  (consume-one-token input-stream :value ";")
	  r)))))



;;
;; ifStatement
;; 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
;;
(defstruct (ifStatement (:print-function print-ifStatement))
  expression
  if-statements
  else-statements)

(defun print-ifStatement (ist stream depth)
  (progn
    (format stream "<ifStatement>")
    (format stream "~a~%" (build-token "if"))
    (format stream "~a~%" (build-token "("))
    (format stream "~a~%" (ifStatement-expression ist))
    (format stream "~a~%" (build-token ")"))
    (format stream "~a~%" (build-token "{"))
    (format stream "~a" (ifStatement-if-statements ist))
    (format stream "~a~%" (build-token "}"))
    (when (ifStatement-else-statements ist)
      (format stream "~a~%" (build-token "else"))
      (format stream "~a~%" (build-token "{"))
      (format stream "~a~%" (ifStatement-else-statements ist))
      (format stream "~a~%" (build-token "}")))
    (format stream "</ifStatement>")))

(defun build-ifStatement (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= "if" (token-value token)))
      (consume-one-token input-stream)
      (next input-stream)
      (consume-one-token input-stream :value "(")
      (make-ifStatement
       :expression (build-expression input-stream)
       :if-statements (and (consume-one-token input-stream :value ")")
			   (consume-one-token input-stream :value "{")
			   (build-statements input-stream))
       :else-statements (and (consume-one-token input-stream :value "}")
			     (build-else-statements input-stream))))))

(defun build-else-statements (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= (token-value token) "else"))
      (consume-one-token input-stream)
      (next input-stream)
      (consume-one-token input-stream :value "{")
      (let ((r (build-statements input-stream)))
	(progn
	  (consume-one-token input-stream :value "}")
	  r)))))



;;
;; whileStatement
;; 'while' '(' expression ')' '{' statements '}'
;;
(defstruct (whileStatement (:print-function print-whileStatement))
  expression
  statements)

(defun print-whileStatement (wst stream depth)
  (progn
    (format stream "<whileStatement>")
    (format stream "~a~%" (build-token "while"))
    (format stream "~a~%" (build-token "("))
    (format stream "~a~%" (whileStatement-expression wst))
    (format stream "~a~%" (build-token ")"))
    (format stream "~a~%" (build-token "{"))
    (format stream "~a~%" (whileStatement-statements wst))
    (format stream "~a~%" (build-token "}"))
    (format stream "</whileStatement>")))

(defun build-whileStatement (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= (token-value token) "while"))
      (consume-one-token input-stream)
      (next input-stream)
      (consume-one-token input-stream :value "(")
      (make-whileStatement
       :expression (let ((r (build-expression input-stream)))
		     (progn (consume-one-token input-stream :value ")")
			    r))
       :statements (and (consume-one-token input-stream :value "{")
			(let ((r (build-statements input-stream)))
			  (progn (consume-one-token input-stream :value "}")
				 r)))))))

;;
;; doStatement
;; 'do' subroutineCall ';'
;;
(defstruct (doStatement (:print-function print-doStatement))
  subroutineCall)

(defun print-doStatement (dst stream depth)
  (progn
    (format stream "<doStatement>~%")
    (format stream "~a~%" (build-token "do"))
    (format stream "~a" (doStatement-subroutineCall dst))
    (format stream "~a~%" (build-token ";"))
    (format stream "</doStatement>")))

(defun build-doStatement (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= (token-value token) "do"))
      (consume-one-token input-stream)
      (let ((r (build-subroutineCall input-stream)))
	  (and (consume-one-token input-stream :value ";")
	       (make-doStatement
		:subroutineCall r))))))


;;
;; returnStatement
;; 'return' expression? ';'
;;
(defstruct (returnStatement (:print-function print-returnStatement))
  expression)

(defun print-returnStatement (rst stream depth)
  (progn
    (format stream "<returnStatement>~%")
    (format stream "~a~%" (build-token "return"))
    (when (returnStatement-expression rst)
      (format stream "~a~%" (returnStatement-expression rst)))
    (format stream "~a~%" (build-token ";"))
    (format stream "</returnStatement>")))

(defun build-returnStatement (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token) (string= (token-value token) "return"))
      (consume-one-token input-stream)
      (let ((r (build-expression input-stream)))
	(and (consume-one-token input-stream :value ";")
	     (make-returnStatement :expression r))))))



;;;
;;; expressions
;;;===============================================================
;;;

;;
;; expression
;; term (op term)*
;;
(defstruct (expression (:print-function print-expression))
  term*)

(defun print-expression (exp stream depth)
  (progn
    (format stream "<expression>~%")
    (dolist (var (expression-term* exp))
      (format stream "~a" var))
    (fresh-line stream)
    (format stream "</expression>")))

(defun build-expression-1 (input-stream)
  (let ((term (build-term input-stream)))
    (when term 
      (cons term 
	    (let ((op (build-op input-stream)))
	      (when op
		(cons op (build-expression-1 input-stream))))))))

(defun build-expression (input-stream)
  (let ((e1 (build-expression-1 input-stream)))
    (when e1
      (make-expression
       :term* e1))))

(defun build-array-expression (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (string= "[" (token-value token)))
      (consume-one-token input-stream)
      (let ((r (build-expression input-stream)))
	(progn
	  (consume-one-token input-stream :value "]")
	  r)))))

;;
;; term
;; integerConstant | stringConstant | keywordConstant | varName
;; | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOP term
;;
(defstruct (term (:print-function print-term))
  arg1
  arg2)

(defun print-term (term stream depth)
  (progn
    (format stream "<term>~%")
    ;; check if it is one arg
    (if (null (term-arg2 term))
	;; check if it is '(' expression ')'
	(if (expression-p (term-arg1 term))
	    (progn
	      (format stream "~a~%" (build-token "("))
	      (format stream "~a" (term-arg1 term))
	      (format stream "~a~%" (build-token ")")))
	    (format stream "~a" (term-arg1 term)))
	;; now it is 2 args
	;; check if it is varName '[' expression ']'
	(if (varName-p (term-arg1 term))
	    (progn
	      (format stream "~a" (term-arg1 term))
	      (format stream "~a~%" (build-token "["))
	      (format stream "~a~%" (term-arg2 term))
	      (format stream "~a" (build-token "]")))
	    ;; it is unaryOp term
	    (progn
	      (format stream "~a" (term-arg1 term))
	      (format stream "~a" (term-arg2 term)))))
    (fresh-line stream)
    (format stream "</term>")))

(defun build-term (input-stream)
  (let ((sbc (build-subroutineCall input-stream)))
    (if sbc (progn
	(make-term :arg1 sbc :arg2 NIL))
	(let ((token (next input-stream)))
	  (when (and (token-p token)
		     (or (member (token-type token) '(integerConstant stringConstant))
			 (keywordConstant? token)
			 (unaryOp? token)
			 (string= "(" (token-value token))
			 (equal 'identifier (token-type token))))
	    (cond ((member (token-type token) '(integerConstant stringConstant))
		   (progn (consume-one-token input-stream) (make-term :arg1 token :arg2 NIL)))
		  ((keywordConstant? token)
		   (make-term :arg1 (build-keywordConstant input-stream) :arg2 NIL))
		  ((unaryOp? token)
		   (make-term :arg1 (build-unaryOp input-stream)
			      :arg2 (build-term input-stream)))
		  ((string= "(" (token-value token))
		   (progn
		     (consume-one-token input-stream)
		     (make-term :arg1 (let ((r (build-expression input-stream)))
					(progn (consume-one-token input-stream :value ")") r))
				:arg2 NIL)))
		  ((equal 'identifier (token-type token))
		   (make-term :arg1 (build-varName input-stream)
			      :arg2 (let ((token (next input-stream)))
				      (when (and (token-p token)
						 (string= "[" (token-value token)))
					(consume-one-token input-stream)
					
					(let ((r (build-expression input-stream)))
					  (progn
					    (consume-one-token input-stream :value "]")
					    r))))))
		  (T (format T "NOT TERM ~a~%" token))))))))




	       
;;
;;  subroutineCall
;;  subroutineName '(' expressionList ')' | (className | varName) '.' subroutineName '(' expressionList ')'
;;
(defstruct (subroutineCall (:print-function print-subroutineCall))
  classVarName
  subroutineName
  expressionList)

(defun print-subroutineCall (sbc stream depth)
  (progn
    (when (subroutineCall-classVarName sbc)
      (format stream "~a~%" (subroutineCall-classVarname sbc))
      (format stream "~a~%" (build-token ".")))
    (when (subroutineCall-subroutineName sbc)
      (format stream "~a" (subroutineCall-subroutineName sbc)))
    (format stream "~a~%" (build-token "("))
    (let ((expl (subroutineCall-expressionList sbc)))
      (if expl
          (format stream "~a~%" expl)
        (progn
          (format stream "<expressionList>~%")
          (format stream "</expressionList>~%"))))

    (format stream "~a~%" (build-token ")"))))


(defun build-subroutineCall (input-stream)
  (let ((token (next input-stream))
	(ntoken (peek-token input-stream)))
    (when (and (token-p token) 
	       (equal (token-type token) 'identifier)
	       (or (=? ntoken "(")
		   (=? ntoken ".")))
      (consume-one-token input-stream)
      (let ((ntoken (next input-stream)))
	(if (string= (token-value ntoken) "(")
	    (make-subroutineCall
	     :classVarName NIL
	     :subroutineName token
	     :expressionList (and (consume-one-token input-stream) 
				  (let ((r (build-expressionList input-stream)))
				    (progn (consume-one-token input-stream :value ")")
					   r))))
	    (make-subroutineCall
	     :classVarName token
	     :subroutineName (and (consume-one-token input-stream :value ".")
				  (build-subroutineName input-stream))
	     :expressionList (and (consume-one-token input-stream :value "(")
				  (let ((r (build-expressionList input-stream)))
				    (progn (consume-one-token input-stream :value ")")
					   r)))))))))


;;
;; expressionList
;; (expression (',' expression)*)?
;;
(defstruct (expressionList (:print-function print-expressionList))
  expression*)

(defun print-expressionList (expl stream depth)
  (progn
    (format stream "<expressionList>~%")
    (let ((r (last (expressionList-expression* expl))))
      (dolist (v (expressionList-expression* expl))
	(format stream "~a~%" v)
	(when (not (equal v (first r)))
	  (format stream "~a~%" (build-token ",")))))
    (format stream "</expressionList>")))

(defun build-expressionList-1 (input-stream)
  (let ((v (build-expression input-stream)))
    (when v
      (cons v 
	    (let ((token (next input-stream)))
	      (when (and (token-p token) (string= (token-value token) ","))
		(consume-one-token input-stream)
		(build-expressionList-1 input-stream)))))))

(defun build-expressionList (input-stream)
  (make-expressionList :expression* (build-expressionList-1 input-stream)))
	     



;; 
;; op
;; '+' | '-' | '*' '/' | '&' | '|' | '<' | '>' | '='
;;
(defstruct (op (:print-function print-op))
  op)

(defun print-op (op stream depth)
  (format stream "~a~%" (op-op op)))

(defun build-op (input-stream)
  (let ((token (next input-stream)))
    (when (and (token-p token)
	       (member (token-value token) '("+" "-" "*" "/" "&" "|" "<" ">" "=") :test #'equal))
      (consume-one-token input-stream)
      (make-op :op token))))

;;
;; unaryOp
;; '-' | '~'
;;

(defstruct (unaryOp (:print-function print-unaryOp))
  uop)

(defun print-unaryOp (uop stream depth)
  (format stream "~a~%" (unaryOp-uop uop)))
  

(defun build-unaryOp (input-stream)
  (let ((token (next input-stream)))
    (when (unaryOp? token)
      (consume-one-token input-stream)
      (make-unaryOp :uop token))))


		 
(defun unaryOp? (token)
  (and (token-p token)
       (or (string= (token-value token) "-")
	   (string= (token-value token) "~"))))



;;
;; keywordConstant
;; 'true' | 'false' | 'null' | 'this'
;;
(defstruct (keywordConstant (:print-function print-keywordConstant))
  kvalue)

(defun print-keywordConstant (kc stream depth)
  (format stream "~a~%" (keywordConstant-kvalue kc)))

(defun build-keywordConstant (input-stream)
  (let ((token (next input-stream)))
    (when (keywordConstant? token)
      (consume-one-token input-stream)
      (make-keywordConstant :kvalue token))))

(defun keywordConstant? (token)
  (and (token-p token)
       (member (token-value token) '("true" "false" "null" "this") :test #'equal)))


;;;
;;; BELOW IS FOR PARSING FROM STREAM
;;; ==========================================================
;;;

;; this is for tracing the token
(defvar *current-token* NIL)
(defvar *peek-token* NIL)

(defun next (stream)
  (if *current-token*
      *current-token*
      (if *peek-token*
	  (progn
	    (setf *current-token* *peek-token*)
	    (setf *peek-token* NIL)
	    *current-token*)
	  (let ((token (get-next-token stream)))
	    (when token
	      (setf *current-token* token)
	      *current-token*)))))

(defun peek-token (stream)
  (let ((r (get-next-token stream)))
    (when r
      (setf *peek-token* r)
      r)))

(defun consume-one-token (stream &key value)
  (let ((token (next stream)))
    (and (token-p token)
         (or (setf *current-token* NIL)
             (if value (=? token value) T)))))
	  


;;
;; DETAILS FOR PARSING TO TOKEN FROM A STREAM
;;

;;
;; parse-engine
;;
(defun process-line-comment (stream)
  (let ((ch (read-char stream nil)))
    (when ch
      (when (char/= ch #\newline)
	(process-line-comment stream)))))

(defun process-seg-comment (stream)
  (let ((ch (read-char stream nil)))
    (when ch 
      (if (char/= ch #\*)
	  (process-seg-comment stream)
	  (let ((nch (peek-char nil stream nil)))
	    (when nch
	      (if (char/= nch #\/)
		  (process-seg-comment stream)
		  (read-char stream nil))))))))

(defun symbol-char? (ch) 
  (member ch 
	  +const-symbol-list+ 
	  :test #'char=
	  :key #'(lambda (ch) (char ch 0))))

(defun get-next-char (stream)
  (let ((ch (read-char stream nil)))
    (when ch
      (cond ((char= ch #\/)
	     (let ((nch (peek-char nil stream nil)))
	       (when nch
		 (cond ((char= nch #\/)
			(progn (read-char stream nil)
			       (process-line-comment stream)
			       #\newline))
		       ((char= nch #\*)
			(progn (read-char stream nil)
			       (process-seg-comment stream)
			       (get-next-char stream)))
		       (T ch)))))
	    ((char= ch #\SPACE)
	     (do ((nch 
		   (peek-char nil stream nil) 
		   (peek-char nil stream nil)))
		 ((char/= nch #\SPACE) #\space)
	       (read-char stream nil)))
	    (T ch)))))
  
(defun stop-char? (ch)
  (or (char= #\newline ch)
      (char= #\space ch)
      (char= #\tab ch)
      (symbol-char? ch)
      (char= #\" ch)))

(defun collect-to-vector (stream vector &key test)
  (do ((ch (get-next-char stream)
	   (get-next-char stream)))
      ((funcall test ch) ch)
    (vector-push-extend ch vector)))
  
(defun get-next-token-1 (stream)
  (let ((token-v (make-array 0 
			     :fill-pointer 0 
			     :adjustable T  
			     :element-type 'character)))
	(let ((ch (collect-to-vector stream token-v :test #'stop-char?)))
	  (cond ((symbol-char? ch)
		 (if (= 0 (length token-v))
		     (progn
		       (vector-push-extend ch token-v)
		       token-v)
		     (progn
		       (unread-char ch stream)
		       token-v)))
		((or (char= #\space ch) 
		     (char= #\newline ch)
		     (char= #\tab ch))
		 (if (= 0 (length token-v))
		     (get-next-token-1 stream)
		     token-v))
		((char= #\" ch)
		 (if (= 0 (length token-v))
		     (progn
		       (vector-push-extend ch token-v)
		       (collect-to-vector stream token-v :test #'(lambda (ch) (char= #\" ch)))
		       (vector-push-extend #\" token-v)
		       token-v)
		     (progn
		       (unread-char ch stream)
		       token-v)))
		(T (format T "unknown condition ~%"))))))

(defun get-next-token (stream)
  (let ((token-str (get-next-token-1 stream)))
    (when token-str
      (build-token token-str))))




;;;
;;; interfaces for processing files
;;;

(defun build-class-from-file (filename)
  (let* ((i (search "." filename))
	 (temp (subseq filename 0 i))
	 (ofile (concatenate 'string temp "-v.xml")))
    (with-open-file (out ofile :direction :output :if-exists :supersede)
      (format out "~a"
	      (with-open-file (in filename :direction :input :if-does-not-exist nil)
		(build-class in))))))


(defun run (dir)
  (let ((files (directory (concatenate 'string dir "/*.jack"))))
    (dolist (file files)
      (let ((name (namestring file)))
	(when (and (> (length name) 5)
		   (string= ".jack" (subseq name (- (length name) 5))))
	  (setf *current-token* NIL *peek-token* NIL)
	  (build-class-from-file name))))))
	 


;;;
;;; chapter 11 contents
;;;

;; basic
(defun append-string-1 (lst)
  (let ((r ""))
    (progn
      (dolist (x lst)
	(setf r (concatenate 'string r x)))
      r)))

(defun append-string (&rest lst)
  (append-string-1 lst))

;; symbol table
(defstruct entry
  name
  type
  kind
  index)

(defparameter *class-symbol-table* NIL)
(defparameter *method-symbol-table* NIL)

(defparameter *current-method-name* NIL)
(defparameter *current-class-name* NIL)

(defun set-current-method-name (m)
  (setf *current-method-name* m))

(defun get-current-method-name ()
  *current-method-name*)

(defun set-current-class-name (m)
  (setf *current-class-name* m))

(defun get-current-class-name ()
  *current-class-name*)

(defun add-entry-to-stable (entry table)
  (if table
      (setf *method-symbol-table* (append *method-symbol-table* (list entry)))
      (setf *class-symbol-table* (append *class-symbol-table* (list entry)))))

(defun get-entry-from-stable (name)
  (let ((e (find-if #'(lambda (e) (string= name (entry-name e))) *method-symbol-table*)))
    (if e e
	(find-if #'(lambda (e) (string= name (entry-name e))) *class-symbol-table*))))


(defun get-new-index-from-stable (kind table)
  (let ((index 0))
    (progn
      (dolist (x table)
	(when (string= (entry-kind x) kind)
	  (setf index (+ 1 (entry-index x)))))
      index)))

(defun get-seg-from-symbol-table(name)
  (progn
    (pprint *method-symbol-table*)
    (pprint *class-symbol-table*)
  (let ((e (get-entry-from-stable name)))
    (when e
      (let ((kind (entry-kind e)))
	(cond ((string= kind "field") "this")
	      ((string= kind "var") "local")
	      (T (entry-kind e))))))))

(defun get-index-from-symbol-table (name)
  (let ((e (get-entry-from-stable name)))
    (when e
      (entry-index e))))

(defun add-symbol-entry (name type kind)
  (let* ((table (if *current-method-name* 
		    *method-symbol-table*
		    *class-symbol-table*))
	 (index (get-new-index-from-stable kind table)))
    (add-entry-to-stable
     (make-entry :name name
		 :type type
		 :kind kind
		 :index index)
     *current-method-name*)))
    

(defun codeWrites-expression (obj)
  (let* ((terms (expression-term* obj))
	 (len (length terms)))
    (when terms
      (if (= 1 len)
	  (codeWrites-term (first terms))
	  (append-string
	    (codeWrites-term (first terms))
	    (codeWrites-term (first (last terms)))
	    (codeWrites-op (second terms)))))))

(defun codeWrites-uop (uop)
  (let ((op (unaryOP-uop uop)))
    (cond ((=? op "-") (format NIL "neg~%"))
	  ((=? op "~") (format NIL "not~%"))
	  (T (format T "Moment : wrong uop ~a" uop)))))

(defun codeWrites-term (obj)
  (let ((arg1 (term-arg1 obj))
	(arg2 (term-arg2 obj)))
    (if arg2
	(if (varName-p arg1)
	    ;; varName [xxx]
	    (codeWrite-array arg1 arg2)
	    (append-string
	      (codeWrites-term arg2)
	      (codeWrites-uop arg1)))
	(cond ((integerConstant-p arg1)
	       (codeWrites-integerConstant arg1))
	      ((stringConstant-p arg1)
	       (codeWrites-stringConstant arg1))
	      ((keywordConstant-p arg1)
	       (codeWrites-keywordConstant arg1))
	      ((varName-p arg1)
	       (codeWrites-varName arg1))
	      ((subroutineCall-p arg1)
	       (codeWrites-subroutineCall arg1))
	      ((expression-p arg1)
	       (codeWrites-expression arg1))
	      (T (format T "something is wrong for obj ~a~%" arg1))))))


(defun codeWrites-op (op)
  (let ((v (op-op op)))
    (when (token-p v)
      (cond ((=? v "+") (format NIL "add~%"))
	    ((=? v "-") (format NIL "sub~%"))
	    ((=? v "*") (format NIL "call Math.multiply 2~%"))
	    ((=? v "/") (format NIL "call Math.divide 2 ~%"))
	    ((=? v "|") (format NIL "or~%"))
	    ((=? v "&") (format NIL "and~%"))
	    ((=? v "<") (format NIL "lg~%"))
	    ((=? v ">") (format NIL "gt~%"))
	    ((=? v "=") (format NIL "eq~%"))
	    (T (format T "wrong op ~a~%" v))))))

(defun integerConstant-p (token)
  (and (token-p token)
       (equal (token-type token) 'integerConstant)))

(defun codeWrites-integerConstant (intc)
  (when (integerConstant-p intc)
    (format NIL "push constant ~a~%" (token-value intc))))

(defun stringConstant-p (token)
  (and (token-p token)
       (equal (token-type token) 'stringConstant)))

(defun codeWrites-stringConstant (token)
  (when (stringConstant-p token)
    (format NIL "push string Constant ~a~%" (token-value token))))

(defun keywordConstant-p (token)
  (and (token-p token)
       (member (token-value token) '("true" "false" "null" "this") :test #'equal)))

(defun codeWrites-keywordConstant (kwd)
  (let ((token (keywordConstant-kvalue kwd)))
    (when (keywordConstant-p token)
      (cond ((=? token "true") (format NIL "push constant 0~%"))
	    ((=? token "false") (format NIL "push constant -1~%"))
	    ((=? token "null") (format NIL "push constant 0~%"))
	    (T (format NIL "push this 0~%"))))))

(defun codeWrites-varName (varName)
  (let* ((name-token (varName-name varName))
	 (name (token-value name-token))
	 (seg (get-seg-from-symbol-table name))
	 (index (get-index-from-symbol-table name)))
    (if (and seg index)
	(format NIL "push ~a ~a~%" seg index)
	"")))

(defun class-name? (name-token)
  (let ((name (token-value name-token)))
    (string= name (get-current-class-name))))

(defun codeWrites-subroutineCall (sbr)
  (let* ((cvname (subroutineCall-classVarName sbr))
	 (sbname-token (subroutineCall-subroutineName sbr))
	 (sbname (subroutineName-name sbname-token))
	 (expl (subroutineCall-expressionList sbr))
	 (exp (expressionList-expression* expl)))
    (append-string
      (format NIL "// codeWrites subroutineCall IN~%")
      (if cvname (codeWrites-varName (make-varName :name cvname)) "")
      (format NIL "// start processing expressions ~%")
      (append-string-1 (mapcar #'(lambda (x) (codeWrites-expression x)) exp))
      (let ((len (length exp)))
	(if cvname
	    (format NIL "call ~a.~a ~a~%" (token-value cvname) (token-value sbname) len)
	    (format NIL "call ~a.~a ~a~%" (get-current-class-name) (token-value sbname) len))))))
;;;
;;; statements
;;;

(defun codeWrites-statements (sts)
  (append-string-1
   (mapcar #'(lambda (st) (codeWrites-statement st)) (statements-statement* sts))))

(defun codeWrites-statement (st)
  (cond ((letStatement-p st) (codeWrites-letStatement st))
	((ifStatement-p st) (codeWrites-ifStatement st))
	((whileStatement-p st) (codeWrites-whileStatement st))
	((doStatement-p st) (codeWrites-doStatement st))
	((returnStatement-p st) (codeWrites-returnStatement st))
	(T (format NIL "unknown statement ~a" st))))

;;
;; let statement
;;
(defun codeWrites-letStatement (st)
  (let ((vname (letStatement-varName st))
	(aexp (letStatement-array-expression st))
	(exp (letStatement-expression st)))
    (progn
    (append-string
     (codeWrites-expression exp)
     (if aexp
       (append-string
	(codeWrites-expression aexp)
	(let ((seg (get-seg-from-symbol-table (token-value (varName-name vname))))
	      (index (get-index-from-symbol-table (token-value (varName-name vname)))))
	  (format NIL "push ~a ~a~%" seg index))
	(format NIL "add~%")
	(format NIL "pop pointer 1 ~%")
	(format NIL "pop that 0 ~%"))
       (append-string
	(let ((seg (get-seg-from-symbol-table (token-value (varName-name vname))))
	      (index (get-index-from-symbol-table (token-value (varName-name vname)))))
	  (format NIL "pop ~a ~a~%" seg index)))))))
	)
     


;;
;; if statement
;;
;; 
(defparameter *true-index* -1)
(defparameter *false-index* -1)

(defun gen-if-label (&optional true)
  (if true
      (progn
	(setf *true-index* (+ 1 *true-index*))
	(format NIL "IF-TRUE~a" *true-index*))
      (progn
	(setf *false-index* (+ 1 *false-index*))
	(format NIL "IF-FALSE~a" *false-index*))))

(defun codeWrites-ifStatement (ist)
  (let ((exp (ifStatement-expression ist))
	(ifst (ifStatement-if-statements ist))
	(ests (ifStatement-else-statements ist))
	(true-label (gen-if-label T))
	(false-label (gen-if-label)))
    (append-string
     (codeWrites-expression exp)
     (format NIL "not~%")
     (format NIL "if-goto ~a~%" false-label)
     (codeWrites-statements ifst)
     (when ests
       (format NIL "if-goto ~a~%" true-label))
     (format NIL "label ~a~%" false-label)
     (when ests
       (codeWrites-statements ests))
     (when ests
       (format NIL "label ~a~%" true-label)))))


;;
;; while statements
;;
(defun codeWrites-whileStatement (wst)
  (let ((exp (whileStatement-expression wst))
	(sts (whileStatement-statements wst))
	(true-label (gen-if-label T))
	(false-label (gen-if-label)))
    (append-string
     (format NIL "label ~a~%" true-label)
     (codeWrites-expression exp)
     (format NIL "not~%")
     (format NIL "if-goto ~a~%" false-label)
     (codeWrites-statements sts)
     (format NIL "goto ~a~%" true-label)
     (format NIL "label ~a~%" false-label))))


;;
;; do Statements
;;
(defun codeWrites-doStatement (dst)
  (let ((sbcall (doStatement-subroutineCall dst)))
    (append-string
     (codeWrites-subroutineCall sbcall)
     (format NIL "pop temp 0 ~%"))))

;;
;; return Statement
;;
(defun codeWrites-returnStatement (rst)
  (let ((exp (returnStatement-expression rst)))
    (if exp
	(append-string
	 (codeWrites-expression exp)
	 (format NIL "return~%"))
	(append-string
	 (format NIL "push constant 0~%")
	 (format NIL "return~%")))))


;;
;; varDec
;;
(defun codeWrites-varDec (vdc)
  (let ((type (varDec-type vdc))
	(vnames (varDec-varName vdc)))
    (dolist (v vnames)
      (add-symbol-entry (token-value (varName-name v))
			(token-value (types-type type))
			"var" ))))


;;
;; subroutineDec
;;
(defun codeWrites-subroutineDec (sdc)
  (let* ((cfm (subroutineDec-con-fun-method sdc))
	 (vt (subroutineDec-void-type sdc))
	 (sname (subroutineDec-subroutineName sdc))
	 (sbody (subroutineDec-subroutineBody sdc))
	 (plst (subroutineDec-parameterList sdc))
	 (funcname (token-value (subroutineName-name sname))))
    (progn
      ;; need build method table
      (setf *current-method-name* funcname)
      (setf *method-symbol-table* NIL)
      (when (string= (token-value cfm) "method")
	;; need add this to the table
	(add-symbol-entry "this" *current-class-name* "argument"))

      ;; add parameter to symbol table
      (when plst
	(dolist (p (parameterList-type-varName plst))
	  (let ((ty (first p))
		(na (second p)))
	    (add-symbol-entry (token-value (varName-name na))
			      (token-value (types-type ty))
			      "argument"))))

      ;; processing local variables
      (let ((vdcs (subroutineBody-varDec* sbody))
	    (sts (subroutineBody-statements sbody)))
	(progn
	  (dolist (vdc vdcs)
	    (codeWrites-varDec vdc))
	  (append-string
	   (let ((n 0))
	     (dolist (vdc vdcs)
	       (dolist (vn (varDec-varName vdc))
		 (setf n (+ 1 n))))
	     (format NIL "function ~a.~a ~a~%" *current-class-name* *current-method-name* n))
	   (if (string= (token-value cfm) "method")
	       (format NIL "push argument 0~%pop pointer 0~%")
	       "")
	   (codeWrites-statements sts)))))))

;;
;; classVarDec
;;
(defun codeWrites-classVarDec (cvd)
  (let ((sf (classVarDec-static-field cvd))
	(ty (classVarDec-type cvd))
	(vns (classVarDec-varName cvd)))
    (dolist (v vns)
      (add-symbol-entry (token-value (varName-name v))
			(token-value (types-type ty))
			(token-value sf)))))


;;
;; class
;;
(defun codeWrites-class (cl)
  (let ((cname (class-s-className cl))
	(cvds (class-s-classVarDec* cl))
	(sbds (class-s-subroutineDec* cl)))
    (progn
      ;; do init
      (setf *current-class-name* (token-value (className-name cname)))
      (setf *current-method-name* NIL)
      (setf *class-symbol-table* NIL *method-symbol-table* NIL)
      (dolist (cvd cvds)
	(codeWrites-classVarDec cvd))
      (append-string-1
       (mapcar #'(lambda (sbd) (codeWrites-subroutineDec sbd))
	       sbds)))))

	   
;;; TESTING CASES
;;; 1. CLASS SEVEN
(defvar test-class "class Main {

   function void main() {
      do Output.printInt(1 + (2 * 3));
      return;
   }

}")

(defun test-codeWrites-class ()
  (progn
    (setf *current-token* NIL *peek-token* NIL)
    (with-input-from-string (stream test-class)
      (let ((c (build-class stream)))
	       (codeWrites-class c)))))

(defun test-c ()
  (with-input-from-string (s test-class)
    (build-class s)))


(defvar test-convert-to-bin "class Main {
    
    /**
     * Initializes RAM[8001]..RAM[8016] to -1,
     * and converts the value in RAM[8000] to binary.
     */
    function void main() {
	    var int value;
        do Main.fillMemory(8001, 16, -1); // sets RAM[8001]..RAM[8016] to -1
        let value = Memory.peek(8000);    // reads a value from RAM[8000]
        do Main.convert(value);           // performs the conversion
        return;
    }
    
    /** Converts the given decimal value to binary, and puts 
     *  the resulting bits in RAM[8001]..RAM[8016]. */
    function void convert(int value) {
    	var int mask, position;
    	var boolean loop;
    	
    	let loop = true;
    	while (loop) {
    	    let position = position + 1;
    	    let mask = Main.nextMask(mask);
    	
    	    if (~(position > 16)) {
    	
    	        if (~((value & mask) = 0)) {
    	            do Memory.poke(8000 + position, 1);
       	        }
    	        else {
    	            do Memory.poke(8000 + position, 0);
      	        }    
    	    }
    	    else {
    	        let loop = false;
    	    }
    	}
    	return;
    }
 
    /** Returns the next mask (the mask that should follow the given mask). */
    function int nextMask(int mask) {
    	if (mask = 0) {
    	    return 1;
    	}
    	else {
	    return mask * 2;
    	}
    }
    
    /** Fills 'length' consecutive memory locations with 'value',
      * starting at 'startAddress'. */
    function void fillMemory(int startAddress, int length, int value) {
        while (length > 0) {
            do Memory.poke(startAddress, value);
            let length = length - 1;
            let startAddress = startAddress + 1;
        }
        return;
    }
}")

(defun test-test (strings build-func write-func)
  (with-input-from-string (stream strings)
    (let ((b (funcall build-func stream)))
      (funcall write-func b))))

(defun test-converttobin ()
  (test-test test-convert-to-bin #'build-class #'codeWrites-class))
