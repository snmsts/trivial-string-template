(in-package #:cl-user)
(defpackage #:trivial-string-template-test
  (:use #:cl
        #:trivial-string-template
        #:prove)
  (:shadowing-import-from #:trivial-string-template
                          #:substitute))
(in-package #:trivial-string-template-test)

;; NOTE: To run this test file, execute `(asdf:test-system :trivial-string-template)' in your Lisp.
(define-symbol-macro .to-symbol.
    (case (readtable-case *readtable*)
      (:preserve 'intern)
      (t (lambda (str) (intern (string-upcase str))))))

(defvar *test-template-1* (list  #\$ "$who is a $person." '(:who "Gu" :person "Lisper") "Gu is a Lisper."))
(defvar *test-template-2* (list  #\$ "${who} is a ${person}." '(:who "Gu" :person "Lisper") "Gu is a Lisper."))
(defvar *test-template-3* (list #\$ "$who is a $${person}." '(:who "Gu") "Gu is a ${person}."))
(defvar *test-template-4*
  (list #\$ "$$lisp-1 is $lisp-1, $$lisp-2 is $lisp-2." '(:lisp-1 "scheme" :lisp-2 "common-lisp")
        "$lisp-1 is scheme, $lisp-2 is common-lisp."))
(defvar *test-template-5* (list  #\# "#who is a #person." '(:who "Gu" :person "Lisper") "Gu is a Lisper."))
(defvar *test-template-6* (list #\% "%{who} is a %{person}." '(:who "Gu" :person "Lisper") "Gu is a Lisper."))
(defvar *test-template-7* (list #\@ "@who is a @@{person}." '(:who "Gu") "Gu is a @{person}."))
(defvar *test-template-8*
  (list #\& "&&lisp-1 is &lisp-1, &&lisp-2 is &lisp-2." '(:lisp-1 "scheme" :lisp-2 "common-lisp")
        "&lisp-1 is scheme, &lisp-2 is common-lisp."))

(defvar *test-templates*
  (loop for i from 1 to 8
     collect (symbol-value
              (funcall
               .to-symbol.
               (concatenate 'string "*test-template-" (write-to-string i) "*")))))

;;; test code
(plan nil)

(subtest "SUBSTITUTE-BASIC"
  (dolist (tmpl *test-templates*)
    (let ((*delimiter* (first tmpl))
          (template (second tmpl))
          (args (third tmpl))
          (expected (fourth tmpl)))
      (is (apply 'substitute template args) expected :test 'string=))))

(subtest "SAFE-SUBSTITUTE-BASIC"
  (dolist (tmpl *test-templates*)
    (let ((*delimiter* (first tmpl))
          (template (second tmpl))
          (args (third tmpl))
          (expected (fourth tmpl)))
      (is (apply 'safe-substitute template args) expected :test 'string=))))

(subtest "TEMPLATE-BASIC"
  (dolist (tmpl *test-templates*)
    (let* ((delimiter (first tmpl))
           (source-string (second tmpl))
           (args (third tmpl))
           (expected (fourth tmpl))
           (template (template source-string :delimiter delimiter)))
      (is (apply template args) expected :test 'string=))))

(subtest "DEFINE-TEMPLATE-BASIC"
  (dolist (tmpl *test-templates*)
    (let* ((delimiter (first tmpl))
           (template (second tmpl))
           (args (third tmpl))
           (expected (fourth tmpl))
           (obj (template template :delimiter delimiter)))
      (is (apply obj args) expected))))

(subtest "TEST-SAFE-MODE"
  (let ((tmpl "$who is a $person")
        (expected "Gu is a $person"))
    (is (safe-substitute tmpl :who "Gu") expected :test 'string=))
  (let ((tmpl "$who is a ${person}")
        (expected "Gu is a $person"))
    (is (safe-substitute tmpl :who "Gu") expected :test 'string=))
  (let* ((tmpl "$who is a $person")
         (expected "$who is a Lisper")
         (obj (template tmpl :safe t)))
    (is (funcall obj :person "Lisper") expected :test 'string=))
  (let* ((tmpl "#{who} is a #{person}")
         (expected "#who is a Lisper")
         (obj (template tmpl :delimiter #\# :safe t)))
    (is (funcall obj :person "Lisper") expected :test 'string=))
  (let* ((tmpl "$who is a $person")
         (expected "$who is a Lisper"))
    (define-template tmpl-fn (:safe t) tmpl)
    (is (tmpl-fn :person "Lisper") expected :test 'string=))
  (let* ((tmpl "#{who} is a #{person}")
         (expected "#who is a Lisper"))
    (define-template tmpl-fn (:delimiter #\# :safe t) tmpl)
    (is (tmpl-fn :person "Lisper") expected :test 'string=)))

(subtest "TEMPLATE-ACCESSORS"
  (let ((tmpl (template "$who likes $me.")))
    (setf (safe tmpl) t)
    (is (funcall tmpl :me "David") "$who likes David." :test 'string=)
    (setf (source-string tmpl) "$lisp is $comment.")
    (is (funcall tmpl :lisp :|common-lisp| :comment :|great|) "common-lisp is great." :test 'string=)
    (setf (delimiter tmpl) #\@)
    (setf (source-string tmpl) "@lisp is @comment.")
    (is (funcall tmpl :lisp :|common-lisp| :comment :|great|) "common-lisp is great." :test 'string=)))
    
(finalize)
