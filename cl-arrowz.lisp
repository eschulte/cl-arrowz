(defpackage :cl-arrowz/cl-arrowz
  (:documentation "Drop in replacement for cl-arrows with some bonus features")
  (:nicknames :cl-arrowz)
  (:use :common-lisp)
  (:export :-> :->> :-<> :-<>>
           :&> :&>> :&<> :&<>>
           :z> :z>> :z<> :z<>>
           :<>))
(in-package :cl-arrowz/cl-arrowz)

(defun ensure-lists (list)
  (mapcar (lambda (el) (if (listp el) el (list el))) list))

(defmacro -> (&rest forms)
  "Thread the result of each form as the second argument to subsequent forms."
  (reduce (lambda (acc form) (cons (car form) (cons acc (cdr form))))
          (ensure-lists (cdr forms)) :initial-value (car forms)))

(defmacro ->> (&rest forms)
  "Thread the result of each form as the last argument to subsequent forms."
  (reduce (lambda (acc form) (append form (list acc)))
          (ensure-lists (cdr forms)) :initial-value (car forms)))

(defun replace-<> (tree form &aux foundp)
  (labels ((r<> (tree)
             (if (consp tree)
                 (cons (r<> (car tree)) (r<> (cdr tree)))
                 (if (eq '<> tree)
                     (prog1 form (setf foundp t))
                     tree))))
    (values (r<> tree) foundp)))

(defmacro -<> (&rest forms)
  "Thread the result of each form as the last argument to subsequent forms."
  (reduce (lambda (acc form)
            (let ((acc-symbol (gensym "ACC")))
              (multiple-value-bind (w/acc foundp)
                  (replace-<> form acc-symbol)
                (if foundp
                    `(let ((,acc-symbol ,acc)) ,w/acc)
                    (cons (car form) (cons acc (cdr form)))))))
          (ensure-lists (cdr forms)) :initial-value (car forms)))

(defmacro -<>> (&rest forms)
  "Thread the result of each form as the last argument to subsequent forms."
  (reduce (lambda (acc form)
            (let ((acc-symbol (gensym "ACC")))
              (multiple-value-bind (w/acc foundp)
                  (replace-<> form acc-symbol)
                (if foundp
                    `(let ((,acc-symbol ,acc)) ,w/acc)
                    (append form (list acc))))))
          (ensure-lists (cdr forms)) :initial-value (car forms)))


;;;; Tests
#|
(-> 3 /)
(-> 3 (expt 2))
(->> 3 /)
(->> 3 (expt 2))
(-<> 3 /)
(-<> 3 (expt 2))
(-<> 3 (list <> <>))
(-<>> 3 /)
(-<>> 3 (expt 2))
(-<>> 3 (list <> <>))
|#
