CL-ARROWZ - simple threading macros from Clojure for Common Lisp

The CL-ARROWZ package implements the following macros.

`->`
:   Thread the result of each form as the second argument to
    subsequent forms.

        CL-ARROWZ> (macroexpand-1 '(-> 3 (expt 2)))
        (EXPT 3 2)
        T
        CL-ARROWZ> (macroexpand-1 '(-> 3 (expt 2) (/ 4)))
        (/ (EXPT 3 2) 4)
        T

`->>`
:   Thread the result of each form as the last argument to subsequent
    forms.  Particularly useful for mapping functions.

        CL-ARROWZ> (macroexpand-1 '(->> 3 (expt 2)))
        (EXPT 2 3)
        T
        CL-ARROWZ> (macroexpand-1 '(->> '(1 2 3 4)
                                    (mapcar #'1+)
                                    (mapcar (lambda (x) (* x 3)))
                                    (remove-if #'oddp)))
        (REMOVE-IF #'ODDP (MAPCAR (LAMBDA (X) (* X 3)) (MAPCAR #'1+ '(1 2 3 4))))
        T
        CL-ARROWZ> (->> '(1 2 3 4)
                        (mapcar #'1+)
                        (mapcar (lambda (x) (* x 3)))
                        (remove-if #'oddp))
        (6 12)
        CL-ARROWZ> 

`-<>`
:   Thread the result of each form as the last argument to subsequent
    forms.  Replace `<>` in subsequent forms with the value of the
    previous form.

        CL-ARROWZ> (macroexpand-1 '(-<> 3 /))
        (/ 3)
        T
        CL-ARROWZ> (macroexpand-1 '(-<> 3 (expt 2)))
        (EXPT 3 2)
        T
        CL-ARROWZ> (macroexpand-1 '(-<> 3 (list <> <>)))
        (LET ((#:ACC616 3))
          (LIST #:ACC616 #:ACC616))
        T

`-<>>`
:   Thread the result of each form as the last argument to subsequent
    forms.  Replace `<>` in subsequent forms with the value of the
    previous form.

        CL-ARROWZ> (macroexpand-1 '(-<>> '(1 2 3 4)
                                    (mapcar #'1+)
                                    (mapcar (lambda (x) (* x 3)))
                                    (remove-if #'oddp)
                                    (reduce #'+ <> :initial-value 2)))
        (LET ((#:ACC631
               (REMOVE-IF #'ODDP
                          (MAPCAR (LAMBDA (X) (* X 3)) (MAPCAR #'1+ '(1 2 3 4))))))
          (REDUCE #'+ #:ACC631 :INITIAL-VALUE 2))
        T
        CL-ARROWZ> (-<>> '(1 2 3 4)
                        (mapcar #'1+)
                        (mapcar (lambda (x) (* x 3)))
                        (remove-if #'oddp)
                        (reduce #'+ <> :initial-value 2))
        20

`&>`
:   Like `->`, but each form is guarded by a when.

        CL-ARROWZ> (macroexpand-1 '(&> 3 (expt 2) (/ 4)))
        (LET ((#:ACC633
               (LET ((#:ACC632 3))
                 (WHEN #:ACC632 (EXPT #:ACC632 2)))))
          (WHEN #:ACC633 (/ #:ACC633 4)))
        T

`&>>`
:   Like `->>`, but each form is guarded by a when.

        CL-ARROWZ> (macroexpand-1 '(&>> '(1 2 3 4)
                                    (mapcar #'1+)
                                    (mapcar (lambda (x) (* x 3)))
                                    (remove-if-not (lambda (el) (= 100 el)))
                                    (error "should get here with ~a")))
        (LET ((#:ACC662
               (LET ((#:ACC661
                      (LET ((#:ACC660
                             (LET ((#:ACC659 '(1 2 3 4)))
                               (WHEN #:ACC659 (MAPCAR #'1+ #:ACC659)))))
                        (WHEN #:ACC660 (MAPCAR (LAMBDA (X) (* X 3)) #:ACC660)))))
                 (WHEN #:ACC661 (REMOVE-IF-NOT (LAMBDA (EL) (= 100 EL)) #:ACC661)))))
          (WHEN #:ACC662 (ERROR "should get here with ~a" #:ACC662)))
        T
        CL-ARROWZ> (&>> '(1 2 3 4)
                        (mapcar #'1+)
                        (mapcar (lambda (x) (* x 3)))
                        (remove-if-not (lambda (el) (= 100 el)))
                        (error "should get here with ~a"))
        NIL

`&<>>`
:   Like `-<>>`, but each form is guarded by a when.

`z>>`
:   Like `->>`, but compose loops.  Prior forms always placed in loop
    position.

        CL-ARROWZ> (->> '(1 2 3 4)
                        (mapcar #'1+)
                        (mapcar (lambda (x) (* x 3))))
        (6 9 12 15)
        CL-ARROWZ> (z>> '(1 2 3 4)
                        (mapcar #'1+)
                        (mapcar (lambda (x) (* x 3))))
        (6 9 12 15)
        CL-ARROWZ> (macroexpand-1 '(->> '(1 2 3 4)
                                    (mapcar #'1+)
                                    (mapcar (lambda (x) (* x 3)))))
        (MAPCAR (LAMBDA (X) (* X 3)) (MAPCAR #'1+ '(1 2 3 4)))
        T
        CL-ARROWZ> (macroexpand-1 '(z>> '(1 2 3 4)
                                    (mapcar #'1+)
                                    (mapcar (lambda (x) (* x 3)))))
        (LET (#:RESULT675)
          (DOLIST (#:TOP676 '(1 2 3 4) (NREVERSE #:RESULT675))
            (PUSH (FUNCALL (LAMBDA (X) (* X 3)) (FUNCALL #'1+ #:TOP676)) #:RESULT675)))
        T

This is intended to be a drop-in replacement for
[CL-ARROWS](https://github.com/nightfly19/cl-arrows).  However unlike
CL-ARROWS this package is actively maintained and has a license.
