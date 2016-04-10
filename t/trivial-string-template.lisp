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

(subtest "DEFINE-TEMPLATE-BASIC"
  (dolist (tmpl *test-templates*)
    (let* ((delimiter (first tmpl))
           (template (second tmpl))
           (args (third tmpl))
           (expected (fourth tmpl))
           (obj (template template :delimiter delimiter)))
        (is (apply obj args) expected))))

(finalize)
