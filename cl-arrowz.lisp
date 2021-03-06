(defpackage :cl-arrowz/cl-arrowz
  (:documentation "Drop in replacement for cl-arrows with some bonus features")
  (:nicknames :cl-arrowz)
  (:use :common-lisp)
  (:export :-> :->> :-<> :-<>>
           :&> :&>> :&<> :&<>>
           :z>>
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


;;;; & Versions which short circuit.
(defmacro &> (&rest forms)
  "Like `->', but each form is guarded by a when."
  (reduce (lambda (acc form)
            (let ((acc-symbol (gensym "ACC")))
              `(let ((,acc-symbol ,acc))
                 (when ,acc-symbol
                   ,(cons (car form) (cons acc-symbol (cdr form)))))))
          (ensure-lists (cdr forms)) :initial-value (car forms)))

(defmacro &>> (&rest forms)
  "Like `->', but each form is guarded by a when."
  (reduce (lambda (acc form)
            (let ((acc-symbol (gensym "ACC")))
              `(let ((,acc-symbol ,acc))
                 (when ,acc-symbol
                   ,(append form (list acc-symbol))))))
          (ensure-lists (cdr forms)) :initial-value (car forms)))

(defmacro &<> (&rest forms)
  "Thread the result of each form as the last argument to subsequent forms."
  (reduce (lambda (acc form)
            (let ((acc-symbol (gensym "ACC")))
              (multiple-value-bind (w/acc foundp)
                  (replace-<> form acc-symbol)
                `(let ((,acc-symbol ,acc))
                   (when ,acc-symbol
                     ,(if foundp
                          w/acc
                          (cons (car form) (cons acc (cdr form)))))))))
          (ensure-lists (cdr forms)) :initial-value (car forms)))

(defmacro &<>> (&rest forms)
  "Thread the result of each form as the last argument to subsequent forms."
  (reduce (lambda (acc form)
            (let ((acc-symbol (gensym "ACC")))
              (multiple-value-bind (w/acc foundp)
                  (replace-<> form acc-symbol)
                `(let ((,acc-symbol ,acc))
                   (when ,acc-symbol
                     ,(if foundp
                          w/acc
                          (append form (list acc-symbol))))))))
          (ensure-lists (cdr forms)) :initial-value (car forms)))


;;;; Z Versions with map composition.
(defmacro z>> (&rest forms)
  "Like `->>', but compose loops.  Prior forms always placed in loop position."
  (let* ((result (gensym "RESULT"))
         (current (gensym "TOP"))
         (functions (reduce (lambda (acc form)
                              (ecase (car form)
                                (mapcar `(funcall ,(second form) ,acc))
                                (mapc `(prog1 ,acc
                                         (funcall ,(second form) ,acc)))))
                            (cdr forms) :initial-value current)))
    `(let (,result)
       (dolist (,current ,(car forms) (nreverse ,result))
         (push ,functions ,result)))))


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

(&> 3 /)
(&> nil ((lambda (x) (error "Should never run on ~a~%" x))))
(&>> 3 /)
(&>> nil (error "Should never run on ~a~%"))

(&<> 3 /)
(&<> nil ((lambda (x) (error "Should never run on ~a~%" x))))
(&<>> 3 /)
(&<>> nil (error "Should never run on ~a~%"))

(z>> '(1 2 3 4)
     (mapcar #'1+)
     (mapc (lambda (el) (format t "~a~%" el)))
     (mapcar (lambda (x) (cons x x)))
     (mapcar #'car))
|#
