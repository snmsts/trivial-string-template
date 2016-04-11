;;;; trivial-string-template.lisp
(in-package :cl-user)
(defpackage #:trivial-string-template
  (:use #:cl #:proc-parse)
  (:shadow #:substitute)
  (:export #:*delimiter*
           #:*variable-pattern*
           #:*tokenizer*
           #:make-tokenizer #:tokenizer-regex #:tokenizer-separators
           #:add-separator #:add-separators #:delete-separator #:delete-separators
           #:substitute
           #:safe-substitute
           #:template
           #:source-string
           #:delimiter
           #:variable-pattern
           #:template-tokenizer
           #:safe
           #:reset-template
           #:define-template)
  (:nicknames #:string.template #:st))
(in-package #:trivial-string-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Global Constants and Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(alexandria:define-constant +empty-string+ ""
  :test 'string= :documentation "Just an empty string.")

(alexandria:define-constant +left-brace+ #\{
  :test 'char= :documentation "The left brace character.")

(alexandria:define-constant +right-brace+ #\}
  :test 'char= :documentation "The right brace character.")

(alexandria:define-constant
    +python-pattern+ "[_a-z][_a-z0-9]*"
  :test 'string=
  :documentation "Regular expression pattern for a valid python identifier.")

(alexandria:define-constant
    +lisp-common-pattern+ "([A-Za-z][A-Za-z0-9]*)(-[A-Za-z0-9]+)*"
  :test 'string=
  :documentation "Regular expression pattern for a regular Common Lisp identifier,
e.g. `variable-information', `define-symbol-macro', etc.")

(defvar *delimiter* #\$ "Default delimiter character.")

(defvar *variable-pattern* +lisp-common-pattern+ "Default variable pattern")

(defvar %segments%
  (make-array 0 :adjustable 0 :fill-pointer 0
              :element-type '(or cons simple-string))
  "A vector that stores segments information for a string template,
e.g. \"$who likes $me.\" => #((:VARIABLE \"who\") \" likes \" (:VARIABLE \"me\") \".\")")

(defvar %variables%
    (make-array 0 :adjustable 0 :fill-pointer 0
                :element-type 'simple-string)
  "A vector that stores variables names for a string template,
e.g. \"$who likes $me.\" => #(\"who\" \"me\")")

(define-symbol-macro .to-symbol.
    ;; this symbol macro will be used when the current Lisp is a Modern Case Lisp,
    ;; see http://franz.com/support/documentation/10.0/doc/case.htm#modern-mode-1 for details.
    (case (readtable-case *readtable*)
      (:preserve 'intern)
      (t (lambda (str) (intern (string-upcase str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass %tokenizer% ()
  ((regex :initarg :regex :accessor tokenizer-regex :type simple-string)
   (compiled-re)
   (separators :initarg :separators :accessor tokenizer-separators :type list))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "A class used for managing separators."))

(defun make-tokenizer-function (compiled-re separators)
  (compile nil `(lambda (char)
                  (or 
                   ,@(dolist (sep separators) `(char= char ,sep))
                   (not #+allegro (regexp:match-re ,compiled-re (string char))
                        #-allegro (cl-ppcre:scan-to-strings ,compiled-re (string char)))))))

(defun make-tokenizer
    (&key (regex "[a-zA-Z0-9-]") (separators (list #\Space #\Newline #\Tab)))
  (make-instance '%tokenizer% :regex regex :separators separators))

(defmethod initialize-instance :after ((this %tokenizer%) &key)
  (with-slots (regex compiled-re separators) this
    (setf compiled-re
          #+allegro (regexp:compile-re regex)
          #-allegro (cl-ppcre:create-scanner regex))
    (closer-mop:set-funcallable-instance-function
     this (make-tokenizer-function compiled-re separators))))

(defvar *tokenizer* (make-tokenizer)
  "A default tokenizer will be used during parsing the template strings.")

(defmethod (setf tokenizer-regex) :before ((new string) (this %tokenizer%))
  (with-slots (regex) this
    (warn "The regular expression of the TOKENIZER will be modified from ~A to ~A"
          regex new)))

(defmethod (setf tokenizer-regex) :after ((new string) (this %tokenizer%))
  (with-slots (regex compiled-re separators) this
    (setf compiled-re
          #+allegro (regexp:compile-re regex)
          #-allegro (cl-ppcre:create-scanner regex))
    (closer-mop:set-funcallable-instance-function
     this (make-tokenizer-function compiled-re separators))))

(defmethod add-separator ((tokenizer %tokenizer%) separator)
  (with-slots (compiled-re separators) tokenizer
    (pushnew separator separators :test 'char=)
    (closer-mop:set-funcallable-instance-function
     tokenizer (make-tokenizer-function compiled-re separators))
    tokenizer))

(defmethod add-separators ((tokenizer %tokenizer%) &rest separators)
  (let ((len (length separators)))
    (cond ((zerop len)
           (progn (warn "No seprators were given.") nil))
          ((= len 1) (add-separator tokenizer (first separators)))
          (t (with-slots (compiled-re) tokenizer
               (symbol-macrolet ((%separators%
                                  (slot-value tokenizer 'separators)))
                 (dolist (sep separators)
                   (pushnew sep %separators% :test 'char=))
                 (closer-mop:set-funcallable-instance-function
                  tokenizer (make-tokenizer-function compiled-re %separators%)))
               tokenizer)))))

(defmethod delete-separator ((tokenizer %tokenizer%) separator)
  (with-slots (compiled-re separators) tokenizer
    (if (find separator separators :test 'char=)
        (progn
          (setf separators (delete separator separators :test 'char=))
          (closer-mop:set-funcallable-instance-function
           tokenizer (make-tokenizer-function compiled-re separators))
          tokenizer)
        (progn
          (let ((*print-length* 5))
            (warn "~A is not in the separators list: ~A" separator separators)
            tokenizer)))))

(defmethod delete-separators ((tokenizer %tokenizer%) &rest separators)
  (let ((len (length separators))
        (*print-length* 5))
    (cond ((zerop len)
           (progn (warn "No separators were given.") nil))
          ((= len 1) (delete-separator tokenizer (first separators)))
          (t (with-slots (compiled-re) tokenizer
               (symbol-macrolet ((%separators%
                                  (slot-value tokenizer 'separators)))
                 (dolist (sep separators)
                   (unless (find sep %separators% :test 'char=)
                     (warn "~A is not in the separators list: ~A" sep %separators%)))
                 (setf %separators%
                       (delete-if (lambda (sep)
                                    (find sep separators :test 'char=))
                                  %separators%))
                 (closer-mop:set-funcallable-instance-function
                  tokenizer (make-tokenizer-function compiled-re %separators%)))
               tokenizer)))))

(defun variable-pattern-matched-p (variable-name variable-pattern)
  "Validate whether a variable's name is matched by the pattern."
  (declare (type simple-string variable-name))
  #+allegro
  (multiple-value-bind (matched-p matched)
      (regexp:match-re variable-pattern variable-name)
    (and matched-p (string= matched variable-name)))
  #-allegro
  (let ((matched (cl-ppcre:scan-to-strings variable-pattern variable-name)))
    (and (string/= +empty-string+ matched) (string= matched variable-name))))

(define-compiler-macro variable-pattern-matched-p
    (&whole form &environment env variable-name variable-pattern)
  (cond ((constantp variable-pattern env)
         `(variable-pattern-matched-p ,variable-name
                                      #+allegro (load-time-value (regexp:compile-re ,variable-pattern))
                                      #-allegro (load-time-value (cl-ppcre:create-scanner ,variable-pattern))))
        (t form)))

(defun collect-format-arguments (args variables &key (safe nil))
  "Collect arguments for `format' function."
  (declare (type list args)
           (type (array simple-string (*)) variables))
  (loop for var across variables
     collect (let ((value (getf args (intern (case (readtable-case *readtable*)
                                               (:preserve var)
                                               (t (string-upcase var)))
                                             :keyword)
                                (when safe (concatenate 'string (string *delimiter*) var)))))
               (if value value
                   (error "Missing variable(~A~A) information." *delimiter* var)))))

(declaim (inline append-supplied-p))
(defun append-supplied-p (name)
  "Given a `name' string, concatenate it with \"-SUPPLIED-P\" or \"-supplied-p\"."
  (concatenate 'string name
               (case (readtable-case *readtable*)
                 (:preserve "-supplied-p")
                 (t "-SUPPLIED-P"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Core Functions and APIs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline make-template-datum)
         (ftype (function ((array (or cons simple-string) (*))) simple-string)))
(defun make-template-datum (segments)
  (apply 'concatenate 'string
         (map 'list (lambda (s)
                      (cond ((consp s) "~A")
                            ((stringp s) s)))
              segments)))

(declaim (ftype (function (simple-string character simple-string %tokenizer%)
                          (values simple-string (array simple-string (*))))
                parse-template))
(defun parse-template (template delimiter variable-pattern &optional (tokenizer *tokenizer*))
  (declare (simple-string template variable-pattern)
           (character delimiter)
           (optimize speed (space 0) (safety 0) (debug 0) (compilation-speed 0)))
  (let ((segments %segments%)
        (variables %variables%))
    (declare (type (array (or simple-string cons) (*)) segments)
             (type (array simple-string (*)) variables))
    (setf (fill-pointer segments) 0
          (fill-pointer variables) 0)
    (when (string= template +empty-string+)
      (return-from parse-template
        (values (make-template-datum #()) #())))
    (labels ((tokenize (char)
               (declare (type character char))
               (funcall tokenizer char))
             (collect-variable (variable)
               (declare (type simple-string variable))
               (if (variable-pattern-matched-p variable variable-pattern)
                   (progn
                     (vector-push-extend variable variables)
                     (vector-push-extend (list :variable variable) segments))
                   (error "~A is not a valid identifier." variable))))
      (with-string-parsing (template)
        (tagbody
         meet-next-delimiter
           (bind (str (skip-until (lambda (c) (char= c delimiter))))
             (declare (type simple-string str))
             (cond ((eofp)
                    (vector-push-extend str segments)
                    (go result))
                   ((string= str +empty-string+)
                    (go parse-variable))
                   (t (vector-push-extend str segments)
                      (go parse-variable))))
         parse-variable
           (progn (advance*)
                  (let ((current (current)))
                    (declare (type character current))
                    (cond ((char= current delimiter) ;; escape
                           (vector-push-extend (string delimiter) segments)
                           (advance*)
                           (go meet-next-delimiter))
                          ((char= current +left-brace+) ;; brace wrapped placeholder
                           (advance*)
                           (bind (var (skip-until (lambda (c) (char= c +right-brace+))))
                             (declare (type simple-string var))
                             (unless (char= (current) +right-brace+)
                               (error "Mismatched braces."))
                             (collect-variable var)
                             (advance*)
                             (if (eofp)
                                 (go result)
                                 (go meet-next-delimiter))))
                          (t (bind (var (skip-until #'tokenize))
                               (declare (type simple-string var))                               
                               (collect-variable var)
                               (if (eofp)
                                   (go result)
                                   (progn
                                     (go meet-next-delimiter))))))))
         result
           (return-from parse-template
             (values (make-template-datum segments)
                     (subseq variables 0 (fill-pointer variables)))))))))

(defun substitute (template &rest args &key &allow-other-keys)
  "Given a template string and the variables(as keywords) it requires, return the formatted string;
if there's missing variable information, it will issue an error."
  (multiple-value-bind (datum variables)
      (parse-template template *delimiter* *variable-pattern* *tokenizer*)
    (apply 'format nil datum (collect-format-arguments args variables))))

(define-compiler-macro substitute
    (&whole form &environment env template &rest args &key &allow-other-keys)
  (cond ((constantp template env)
         (multiple-value-bind (datum variables)
             (parse-template template *delimiter* *variable-pattern* *tokenizer*)
           `(format nil ,datum
                    ,@(collect-format-arguments args variables))))
        (t form)))

(defun safe-substitute (template &rest args &key &allow-other-keys)
  "Given a template string and the variables(as keywords) it requires, return the formatted string;
compared to `substitute', this function won't issue an error when there's missing variable information but
uses the variable's name as a default value."
  (multiple-value-bind (datum variables)
      (parse-template template *delimiter* *variable-pattern* *tokenizer*)
    (apply 'format nil datum (collect-format-arguments args variables :safe t))))

(define-compiler-macro safe-substitute
    (&whole form &environment env template &rest args &key &allow-other-keys)
  (cond ((constantp template env)
         (multiple-value-bind (datum variables)
             (parse-template template *delimiter* *variable-pattern* *tokenizer*)
           `(format nil ,datum
                    ,@(collect-format-arguments args variables :safe t))))
        (t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Class Definitions and APIs ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass template ()
  (;; "This is the literal string describing a placeholder introducing delimiter.
   ;; The default value is #\$."
   ;; -- Python Docs (https://docs.python.org/2/library/string.html?highlight=string.template#string.Template)
   (delimiter :initarg :delimiter :accessor delimiter :type character)
   ;; "This is the regular expression describing the pattern for non-braced placeholders
   ;; (the braces will be added automatically as appropriate)."
   ;; The default value is the regular expression '([A-Za-z][A-Za-z0-9]*)(-[A-Za-z][A-Za-z0-9]*)*'(+lisp-common-pattern+).
   ;; -- Python Docs (https://docs.python.org/2/library/string.html?highlight=string.template#string.Template)
   (variable-pattern :initarg :variable-pattern :accessor variable-pattern :type function)
   ;; The source string.
   (source :initarg :template-source :accessor source-string :type simple-string)
   (safe :initform nil :initarg :safe :accessor safe :type symbol)
   (tokenizer :initarg :tokenizer :accessor template-tokenizer :type %tokenizer%)
   (datum :initarg :template-datum  :type simple-string)
   (variables :initarg :template-variables  :type (array simple-string (*))))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Template Class definition."))

(defun template
    (template &key (delimiter *delimiter*) (variable-pattern *variable-pattern*) (safe nil) (tokenizer *tokenizer*))
  "Constructor method for template class."
  (declare (type simple-string template))
  (when (string= template +empty-string+)
    (warn "You are using an empty string to initialize the template.")
    (return-from template
      (make-instance 'template :safe safe
                     :template-source "" :delimiter delimiter
                     :variable-pattern
                     #+allegro (regexp:compile-re variable-pattern)
                     #-allegro (cl-ppcre:create-scanner variable-pattern))))
  (multiple-value-bind (datum variables)
      (parse-template template delimiter variable-pattern tokenizer)
    (make-instance 'template :template-source template :safe safe :tokenizer tokenizer
                   :delimiter delimiter
                   :variable-pattern
                   #+allegro (regexp:compile-re variable-pattern)
                   #-allegro (cl-ppcre:create-scanner variable-pattern)
                   :template-datum datum :template-variables variables)))

(defmacro define-template
    (var (&key (delimiter *delimiter*) (variable-pattern *variable-pattern*) (safe nil) (tokenizer *tokenizer*)) template)
  (let ((obj (gensym)))
    `(let ((,obj (template ,template
                           :delimiter ,delimiter
                           :variable-pattern ,variable-pattern
                           :tokenizer ,tokenizer
                           :safe ,safe)))
       (setf (symbol-function ',var) ,obj))))
         
(defmethod initialize-instance :after ((this template) &key)
  (with-slots (delimiter datum variables safe) this
    (closer-mop:set-funcallable-instance-function
     this (make-template-function datum variables delimiter :safe safe))))

(defun reset-template
    (template source-string
     &key (delimiter (delimiter template))
       (variable-pattern (variable-pattern template))
       (tokenizer (template-tokenizer template))
       (safe (safe template)))
  "Reset several slots and re-parse the template itself."
  (declare (type template template))
  (progn
    (setf (slot-value template 'delimiter) delimiter
          (slot-value template 'variable-pattern) variable-pattern
          (slot-value template 'tokenizer) tokenizer
          (slot-value template 'safe) safe
          (slot-value template 'source) source-string)
    (reparse-template template)
    template))

(defmethod (setf source-string) :after ((new string) (this template))
    (reparse-template this))

(defmethod (setf delimiter) :after ((new character) (this template))
    (reparse-template this))

(defmethod (setf variable-pattern) :after ((new string) (this template))
    (reparse-template this))

(defmethod (setf template-tokenizer) :after ((new %tokenizer%) (this template))
  (reparse-template this))

(defmethod add-separator ((this template) separator)
  (setf (template-tokenizer this)
        (add-separator (template-tokenizer this) separator)))

(defmethod add-separators ((this template) &rest separators)
  (setf (template-tokenizer this)
        (apply 'add-separators (template-tokenizer this) separators)))

(defmethod delete-separator ((this template) separator)
  (setf (template-tokenizer this)
        (delete-separator (template-tokenizer this) separator)))

(defmethod delete-separators ((this template) &rest separators)
  (setf (template-tokenizer this)
        (apply 'delete-separators (template-tokenizer this) separators)))

(defmethod (setf safe) :after ((new symbol) (this template))
  (with-slots (delimiter datum variables safe) this
    (closer-mop:set-funcallable-instance-function
     this (make-template-function datum variables delimiter :safe safe))))

(defun reparse-template (template)
  "Re-parse a template with its own slots;
this function will be issued when one of slot is modified."
  (declare (type template template))
  (with-slots (source delimiter variable-pattern tokenizer datum variables safe) template
    (multiple-value-bind (datum variables)
        (parse-template source delimiter variable-pattern tokenizer)
      (setf (slot-value template 'datum) datum
            (slot-value template 'variables) variables)
      (closer-mop:set-funcallable-instance-function
       template (make-template-function datum variables delimiter :safe safe))
      template)))

(defun make-template-parameters-list (variables delimiter safe)
  "Helper function for `make-template-function', it helps generate the corresponding parameters list."
  (let ((vars (remove-duplicates variables :test 'string=))
        (delimiter* (string delimiter)))
    `(&key
      ,@(if safe
            (map 'list (lambda (var)
                         `(,(funcall .to-symbol. var) ,(concatenate 'string delimiter* var)))
                 vars)
            (map 'list (lambda (var)
                         `(,(funcall .to-symbol. var) nil
                            ,(funcall .to-symbol. (append-supplied-p var))))
                 vars)))))

(defun make-template-function (datum variables delimiter &key (safe nil))
  "Make a compiled function for a template, given its datum, variables, delimiter and probably the safe."
  (let ((vars (remove-duplicates variables :test 'string=)))
    (compile nil
             (if safe
                 `(lambda ,(make-template-parameters-list variables delimiter t)
                    (format nil ,datum
                            ,@(loop for var across variables
                                 collect (funcall .to-symbol. var))))
                 `(lambda ,(make-template-parameters-list variables delimiter nil)
                    (progn
                      ,@(loop for var across vars
                           collect `(unless ,(funcall .to-symbol. (append-supplied-p var))
                                      (error "The variable ~A~A is not supplied, which must be supplied in non-safe mode."
                                             ,delimiter ,var)))
                      (format nil ,datum 
                              ,@(loop for var across variables
                                   collect (intern (case (readtable-case *readtable*)
                                                     (:preserve var)
                                                     (t (string-upcase var))))))))))))
