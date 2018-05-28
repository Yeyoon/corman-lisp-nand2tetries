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
  (format stream "<~a>~a</~a>" pair value pair))

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
	 (make-token :type 'stringConstant :value str-token))
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
    (format stream "<class>")
    (format stream "~a~%" (build-token "class"))
    (format stream "~a~%" (class-s-className class))
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
;; 'int' | 'char' | 'boolean' | 'class' | className
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
    (format stream "<subroutineDec>~%")
    (format stream "~a~%" (subroutineDec-con-fun-method subroutineDec))
    (format stream "~a~%" (subroutineDec-void-type subroutineDec))
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
    (format stream "<parameterList>")
    (dolist (x (parameterList-type-varName parameterList))
      (format stream "~a~%" (first x))
      (format stream "~a~%" (second x))
      (when (not (equal x (last (parameterList-typevarName parameterList))))
	(format stream "~a~%" (build-token ","))))
    (format stream "</parameterList>~%")))

(defun build-parameterList-1 (input-stream)
  (let ((type (build-type input-stream)))
    (when (Types-p type)
      (consume-ont-token)
      (cons (list type (build-varName input-stream))
	    (let ((token (next input-stream)))
	      (when (and (token-p token) (string= (token-value token) ","))
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
      (consume-one-token)
      (next input-stream)
      (consume-one-token :value "(")
      (make-whileStatement
       :expression (let ((r (build-expression input-stream)))
		     (progn (consume-one-token :value ")")
			    r))
       :statements (and (consume-one-token "{")
			(let ((r (build-statements input-stream)))
			  (progn (consume-one-token "}")
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
      (format stream "~a~%" var))
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
	      (format stream "~a~%" (term-arg1 term))
	      (format stream "~a" (term-arg2 term)))))
    (format stream "</term>")))

(defun build-term (input-stream)
  (let ((sbc (build-subroutineCall input-stream)))
    (if sbc (progn
        (format T "sbc is ~a" sbc)
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
			      :arg2 (and (consume-one-token input-stream)
					 (build-term input-stream))))
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
    (format stream "<expressionList>")
    (let ((r (last (expressionList-expression* expl))))
      (dolist (v (expressionList-expression* expl))
	(format stream "~a~%" v)
	(when (not (equal v r))
	  (format stream "~a~%" (build-token ",")))))
    (format stream "</expressionList>")))

(defun build-expressionList-1 (input-stream)
  (let ((v (build-expression input-stream)))
    (when v
      (cons v 
	    (let ((token (next input-stream)))
	      (when (and (token-p token) (string= (token-value token) ","))
		(consume-one-token)
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
    (progn (format T "token is ~a, value is ~a~%" token value)
    (and (token-p token)
         (or (setf *current-token* NIL)
             (if value (=? token value) T))))))
	  


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
	      (format T "ch is ~a nch is ~a ~%" ch nch)
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
			       (pprint "after process seg comment stream")
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
		     (char= #\newline ch))
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

