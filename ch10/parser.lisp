;;
;; Author : Moment
;; Date : 2018/5/15
;;

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

(defun symbol? (sym) (member sym *symbol* :test #'equal))

;; integerConstant
(defun integerConstant (intstr)
    (handler-case
        (let ((v (parse-integer intstr)))
            (and (>= v 0) (<= v 32767) intstr))
        (error NIL)))

;; stringConstant
(defun stringConstant (strstr)
    (let* ((s (string-trim " " strstr))
           (ll (length s)))
        (and (char= (char s 0) #\")
             (char= (char s (- ll 1)) #\"))))

;; identifier
(defun identifier (str)
    (let ((ch (char str 0)))
        (not (member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))))

;;; 
;; codes for generate tokens
;;
       