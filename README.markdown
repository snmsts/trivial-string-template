# Trivial-String-Template - A trivial string template library, inspired by Python's string.Template

## Usage

### substitute

```common-lisp
(substitute "$who likes $what" :who "tim" :what "kung pao")
;; => "tim likes kung pao"
```

You can specify the delimiter by binding the *delimiter* lexcially:

```common-lisp
(let ((*delimiter* #\%))
  (substitute "%who likes %what" :who "tim" :what "kung pao"))
;; => "tim likes kung pao"
```

You can escape a delimiter:

```common-lisp
(substitute "Give $who $$100" :who "tim")
;; => "Give tim $100"
```

And you can use '{}' to set a so called placeholder:

```common-lisp
(substitute "$who likes ${what}--is it for real?" :who "tim" :what "kung pao")
;; => "tim likes kung pao--is it for real?"
```

### safe-substitute

```common-lisp
(safe-substitute "$who likes $what" :what "kung pao")
;; => "$who likes kung pao"
```

### template

It will produce a template class, which is funcallable.

```commmon-lisp
(defvar *tmpl* (template "$who likes $what"))
(funcall *tmpl* :who "a" :what "b")
;; => "a likes b"
(setf (source-string *tmpl*) "Give $who $$100")
(funcall *tmpl* :who "me")
;; => "Give me $100"
```

### define-template

Just a wrapper around template class.

```common-lisp
(define-template tmpl (:delimiter #\& :safe t) "&who likes &what")
;; => #<TRIVIAL-STRING-TEMPLATE:TEMPLATE {1003B1FC7B}>
(tmpl :who tim) ;; => "tim likes &what"
```

## TODO

- [] Provide a more detailed APIs documenatation.
- [] A comprehensive test.

## Author

* David Gu (david_guru@gty.org.in)

## Copyright

Copyright (c) 2016 David Gu (david_guru@gty.org.in)

## License

Licensed under the MIT License.
