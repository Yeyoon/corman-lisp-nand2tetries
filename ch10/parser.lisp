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

(defun build-token-list (string-split-list &key string-constant)
    (if string-split-list
        (if string-constant
            (append (list (concatenate 'string "\"" (first string-split-list) "\""))
                (build-token-list (rest string-split-list) :string-constant NIL))
            (append (split (first string-split-list))
                (build-token-list (rest string-split-list) :string-constant T)))
        NIL))

(defun split-line (line)
    (let ((string-split-list (split line "\"")))
        (build-token-list string-split-list :string-constant NIL)))

(defun string-list (str)
    (if (string= "" str)
        NIL
        (cons (subseq str 0 1)
            (string-list (subseq str 1)))))

;; for /* */ comments
(defvar *in-comments* NIL)

;; Lexical elements

;; 1. keyword
(defvar *keywords* 
    '("class" "constructor" "function" "method"
      "field" "static" "var" "int" "char" "boolean"
      "void" "true" "false" "null" "this" "let" 
      "do" "if" "else" "while" "return"))

(defun keyword? (key)
    (member key *keywords* :test #'equal))

;; 2. symbol
(defvar *symbols*
    '("{" "}" "(" ")" "[" "]" "." ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~" ","))

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
(defun build-token-1 (token)
  (and (string/= token "")
      (cond ((symbol? token) (make-token :type 'symbol :value token))
    	((keyword? token) (make-token :type 'keyword :value token))
    	((integerConstant? token) (make-token :type 'integerConstant :value token))
    	((stringConstant? token) (make-token :type 'stringConstant :value (subseq token 1 (- (length token) 1))))
    	((identifier? token) (make-token :type 'identifier :value token))
    	(T (format T "Unknown token ~a~%" token)))))

(defun build-token (token)
    (let ((len (length token)))
        (dotimes (i len)
            (let ((ch (subseq token i (+ 1 i))))
                (if (symbol? ch)
                    (return (list (build-token-1 (subseq token 0 i))
                                  (build-token-1 ch)
                                  (build-token (subseq token (+ i 1)))))
                    (when (= i (- len 1))
                        (return (build-token-1 token))))))))
        
              
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

(defun process-line-1 (line)
  (if *in-comments*
      (process-token-list (split line))
      (when (not (is-line-comment? line))
	        (process-token-list (split-line (remove-line-comment line))))))

(defun process-line (line)
    (when line
        (let ((nl (string-trim " " line)))
            (and (string/= nl "") (process-line-1 nl)))))


(defun parse-file-1 (filename)
    (let ((r NIL))
        (progn
            (with-open-file (stream filename :direction :input :if-does-not-exist NIL)
                (when stream
                    (do ((line (read-line stream nil) (read-line stream nil)))
                        ((null line))
                        (setf r (append r (process-line line))))))
             r)))  
	 
(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

(defun parse-file (filename)
    (flatten (parse-file-1 filename)))

;;
;; Using grammar
;;

;; inner use

(defvar *current-token-lists* NIL)
(defun move-n (n) (setf *current-token-lists* (nthcdr n *current-token-lists*)))
(defun move-one () (move-n 1))

;; 
;; ================================================================================
;; interfaces for token list
;;
(defun get-current-token-list () *current-token-lists*)
(defun set-current-token-list (tlist) (setf *current-token-lists* tlist))
(defun get-current-token () (and *current-token-lists* (first *current-token-lists*)))
(defun get-current-token-value ()
    (let ((token (get-current-token)))
        (when (token-p token)
            (token-value token))))
(defun get-current-token-type ()
    (let ((token (get-current-token)))
        (when (token-p token)
            (token-type token))))

;; gen terminals
(defun current-is-terminal? ()
    (let ((c (get-current-token)))
        (and (token-p c)
             (member (token-type c) '(keyword symbol integerConstant stringConstant identifier)))))

(defun gen-terminal (&key given type)
    (when (current-is-terminal?)
        (if (or (and given (string/= (get-current-token-value) given))
                (and type (not (equal (get-current-token-type) type))))
            NIL
            (let ((v (get-current-token-value))
                  (ty (get-current-token-type)))
                (when (and v ty)
                    (move-one)
                    (format NIL "<~a>~a</~a>" ty v ty))))))


;; flatten append
(defun flatten-append (&rest arg-lists)
    (flatten arg-lists))

;; gen keywordConstant
;; keywordConstant : 'true'|'false'|'null'|'this'
(defun gen-keywordConstant ()
    (or (gen-terminal  :given "true")
        (gen-terminal  :given "false")
        (gen-terminal  :given "null")
        (gen-terminal  :given "this")))

;; gen unaryOp
(defun gen-unaryOp ()
    (or (gen-terminal  :given "-")
        (gen-terminal  :given "~")))

;; gen op
;; op: '+'|'-'|'*'|'/'|'&'|'|'|'<'|'>'|'='
(defun gen-op ()
    (dolist (var '("+" "-" "*" "/" "&" "|" "<" ">" "="))
        (let ((v (gen-terminal :given var)))
            (when v (return v)))))

;; gen-unaryOp-term
(defun gen-unaryOp-term ()
    (let ((x (gen-unaryOp)))
        (when x
            (flatten-append
                x
                (gen-term)))))

;; gen-term-expression
(defun gen-term-expression ()
    (let ((x (gen-terminal  :given "(")))
       (when x
            (flatten-append
                    x
                    (gen-expression )
                    (gen-terminal  :given ")")))))

;; gen expressionList-1 
(defun gen-expressionList-1 ()
    (let ((x (gen-terminal  :given ",")))
        (when x
            (flatten-append
                x
                (gen-expression )
                (gen-expressionList-1)))))

;; gen expressionList
(defun gen-expressionList ()
    (let ((exp (gen-expression)))
        (when exp
            (flatten-append
                exp
                (gen-expressionList-1)))))

;; gen subroutineCall
(defun gen-subroutineCall-1 ()
    (flatten-append
        (gen-subroutineName )
        (gen-terminal  :given "(")
        (list "<expressionList>")
        (gen-expressionList )
        (list "</expressionList>")
        (gen-terminal :given ")")))

(defun gen-subroutineCall-2 ()
    (flatten-append
        (or (gen-className ) (gen-varName ))
        (gen-terminal  :given ".")
        (gen-subroutineName )
        (gen-terminal  :given "(")
        (list "<expressionList>")
        (gen-expressionList )
        (list "</expressionList>")
        (gen-terminal  :given ")")))

(defun gen-subroutineCall ()
    (let ((tlist (get-current-token-list)))
        (when tlist
            (let ((k (second tlist)))
             (when k
                (if (equal (token-value k) ".")
                    (gen-subroutineCall-2)
                    (when (equal (token-value k) "(")
                        (gen-subroutineCall-1))))))))
    
;; gen varName array
(defun gen-varName-array ()
    (let ((tlist (get-current-token-list)))
        (when (rest tlist)
            (let ((a (first tlist))
                  (b (second tlist)))
                (when (and a b (equal (token-type a) 'identifier) (string= (token-value b) "["))
                    (flatten-append
                        (gen-identifier )
                        (gen-terminal  :given "[")
                        (gen-expression )
                        (gen-terminal  :given "]")))))))

;; gen keywordConstant
(defun gen-keywordConstant ()
    (gen-terminal :type 'keywordConstant))

;; gen stringConstant
(defun gen-stringConstant ()
    (gen-terminal :type 'stringConstant))

;; gen integerConstant
(defun gen-integerConstant ()
    (gen-terminal :type 'integerConstant))

;; gen term
(defun gen-term ()
    (let ((term
            (or 
                (gen-integerConstant )
                (gen-stringConstant )
                (gen-keywordConstant )
                (gen-varName-array ) 
                (gen-subroutineCall )
                (gen-varName )
                (gen-term-expression)
                (gen-unaryOp-term ))))
        (when term
            (flatten-append
                (list "<term>")
                term
                (list "</term>")))))

;; gen op term
(defun gen-op-term ()
    (let ((gop (gen-op)))
        (when gop
            (flatten-append
                gop
                (gen-term )
                (gen-op-term )))))

;; gen expressions
(defun gen-expression ()
    (let ((term (gen-term )))
        (when term
            (flatten-append
                (list "<expression>")
                term
                (gen-op-term)
                (list "</expression>")))))
                    

;; gen return statement
(defun gen-return-statement ()
    (let ((greturn (gen-terminal  :given "return")))
        (when greturn
            (flatten-append
                (list "<returnStatement>")
                greturn
                (gen-expression )
                (gen-terminal  :given ";")
                (list "</returnStatement>")))))

;; gen do statement
(defun gen-do-statement ()
    (let ((gdo (gen-terminal  :given "do")))
        (when gdo
            (flatten-append
                (list "<doStatement>")
                gdo
                (gen-subroutineCall )
                (gen-terminal  :given ";")
                (list "</doStatement>")))))

;; gen while statement
(defun gen-while-statement ()
    (let ((gw (gen-terminal  :given "while")))
        (when gw
            (flatten-append
                (list "<whileStatement>")
                gw
                (gen-terminal  :given "(")
                (gen-expression )
                (gen-terminal  :given ")")
                (gen-terminal  :given "{")
                (gen-statements )
                (gen-terminal  :given "}")
                (list "</whileStatement>")))))

;; gen if statement
(defun gen-if-statement ()
    (let ((isif (gen-terminal  :given "if")))
        (when isif
            (flatten-append
                (list "<ifStatement>")
                isif
                (gen-terminal  :given "(" )
                (gen-expression )
                (gen-terminal  :given ")" )
                (gen-terminal  :given "{")
                (gen-statements )
                (gen-terminal  :given "}")
                (let ((el (gen-terminal  :given "else")))
                    (when el
                        (flatten-append
                            el
                            (gen-terminal  :given "{")
                            (gen-statements )
                            (gen-terminal  :given "}"))))
                (list "</ifStatement>")))))

;; gen let statement
(defun gen-let-statement ()
    (let ((islet (gen-terminal  :given "let")))
        (when islet
            (flatten-append
                (list "<letStatement>")
                islet
                (gen-varName )
                (let ((lb (gen-terminal  :given "[")))
                    (when lb
                        (flatten-append
                            lb
                            (gen-expression )
                            (gen-terminal  :given "]"))))
                (gen-terminal  :given "=")
                (gen-expression )
                (gen-terminal  :given ";")
                (list "</letStatement>")))))


;; gen statement
(defun gen-statement ()
    (or (gen-let-statement )
        (gen-if-statement )
        (gen-while-statement )
        (gen-do-statement )
        (gen-return-statement )))

;; statement?
(defun statement? (token)
    (let ((v (token-value token)))
        (member v '("let" "if" "while" "do" "return") :test #'equal)))

;;;
;;; statements
;;;
(defun gen-statements ()
    (let ((statement (get-current-token)))
        (when (and (token-p statement) (statement? statement))
            (flatten-append
                (list "<statements>")
                (let ((s NIL))
                    (do ((r (gen-statement) (gen-statement)))
                        ((null r) s)
                        (setf s (append s r))))
                (list "</statements>")))))



;; gen varName
(defun gen-varName ( &key with-stars)
    (let ((id (gen-identifier)))
        (when id
            (flatten-append
                id
                (when with-stars
                    (gen-terminal  :given ",")
                    (gen-varName  :with-stars with-stars))))))


;; gen subroutineName
(defun gen-subroutineName ()
    (gen-identifier ))

;; gen className
(defun gen-className ()
    (gen-identifier ))
  
;; gen var
;; varName (','varName)*';'
(defun gen-var (&key with-stars)
    (let ((k (gen-identifier)))
        (when k
            (flatten-append
                k
                (when with-stars
                    (let ((tmp (gen-terminal :given ",")))
                        (when tmp
                            (flatten-append
                                tmp
                                (gen-var :with-stars with-stars)))))))))      

;; gen varDec
(defun gen-varDec ( &key with-stars)
    (let ((gvar (gen-terminal  :given "var")))
        (when gvar
            (flatten-append
                (list "<varDec>")
                gvar
                (gen-type)
                (gen-var :with-stars T)
                (gen-terminal :given ";")
                (list "</varDec>")
                (let ((next (get-current-token)))
                    (when (and (token-p next) (equal (token-value next) "var"))
                        (gen-varDec :with-stars with-stars)))))))
                
 

;; gen subroutineBody
(defun gen-subroutineBody ()
    (let ((gsbb (gen-terminal :given "{")))
        (when gsbb
            (flatten-append
                (list "<subroutineBody>")
                gsbb
                (gen-varDec  :with-stars T)
                (gen-statements )
                (gen-terminal  :given "}")
                (list "</subroutineBody>")))))
            

;; gen parameterList
(defun gen-parameterList ()
    (let ((type (get-current-token)))
        (when (type? type)
            (flatten-append
                (list "<parameterList>")
                (gen-type )
                (gen-varName )
                (let ((r (gen-terminal :given ",")))
                    (when r
                        (flatten-append 
                            r
                            (gen-parameterList))))
                (list "</parameterList>")))))

;; subroutineDec?
(defun sbroutineDec? (token)
    (when (token-p token)
        (let ((con (token-value token)))
            (member con '("constructor" "function" "method") :test #'equal))))

;; gen subroutineDec
(defun gen-subroutineDec ( &key with-stars)
    (let ((sbd (get-current-token)))
        (when (sbroutineDec? sbd)
            (flatten-append
                (list "<subroutineDec>")
                (gen-terminal)
                (or (gen-terminal  :given "void")
                    (gen-type ))
                (gen-subroutineName )
                (gen-terminal  :given "(")
                (gen-parameterList )
                (gen-terminal  :given ")")
                (gen-subroutineBody )
                (when with-stars
                    (gen-subroutineDec  :with-stars with-stars))
                (list "</subroutineDec>")))))
                    
;; gen type
(defun type? (token-s)
    (and (token-p token-s)
        (or (member (token-value token-s) '("int" "char" "boolean") :test #'equal)
            (identifier? (token-value token-s)))))

(defun gen-identifier ()
    (gen-terminal :type 'identifier))

(defun gen-type ()
    (let ((type (get-current-token)))
        (when (type? type)
            (if (keyword? (token-value type))
                (gen-terminal)
                (gen-identifier)))))

;; static-field?
(defun static-field? (token)
    (and (token-p token)
        (member (token-value token) '("static" "field") :test #'equal)))

;; gen classVarDec
(defun gen-classVarDec ( &key with-stars)
    (let ((static-field (get-current-token)))
            (when (static-field? static-field)
                (flatten-append
                    (list "<classVarDec>")
                    (gen-terminal )
                    (gen-type )
                    (gen-varName  :with-stars T)
                    (gen-terminal :given ";")
                    (list "</classVarDec>")
                    (when with-stars
                        (let ((sf (get-current-token)))
                            (when sf
                                (gen-classVarDec  :with-stars with-stars))))))))

;; class?
(defun class? (token)
    (and (token-p token)
        (equal (token-value token) "class")))
                
;; 1. gen class
(defun gen-class (token-list)
    (when token-list
        (set-current-token-list token-list)
        (let ((class (get-current-token)))
            (when (class? class)
                (flatten-append
                    (list "<class>")
                    (gen-terminal :given "class")
                    (gen-identifier)
                    (gen-terminal :given "{")
                    (gen-classVarDec  :with-stars T)
                    (gen-subroutineDec :with-stars T)
                    (gen-terminal :given "}")
                    (list "</class>"))))))

(defun debug-compile-class (tlist)
    (when tlist
        (reset-current-token-index)
        (gen-class tlist)))

(defun compile-xml (filename)
    (let ((token-list (parse-file filename)))
        (when token-list
            (pprint token-list)
            (gen-class token-list))))

;;;
;;; TESTING CASES
;;;
(progn
(setf *in-comments* NIL)
(process-line " sgsa /*sgjsa */sljas //sjgalsk "))
	
    
