(defpackage :cl-arrowz/tests/cl-arrowz
  (:use :cl
	:rove
	:cl-arrowz/cl-arrowz))
(in-package :cl-arrowz/tests/cl-arrowz)

(deftest test/->
  (ok (= (-> 3 /) (/ 3)))
  (ok (expands '(-> 3 /) '(/ 3))) 
  (ok (= (-> 3 (expt 2)) (expt 3 2)))
  (ok (expands '(-> 3 (expt 2)) '(expt 3 2))))

(deftest test/->>
  (ok (= (->> 3 /) (/ 3)))
  (ok (expands '(->> 3 /) '(/ 3)))
  (ok (= (->> 3 (expt 2)) (expt 2 3)))
  (ok (expands '(->> 3 (expt 2)) '(expt 2 3))))

(deftest test/-<>
  (ok (= (-<> 3 /) (/ 3)))
  (ok (expands '(-<> 3 /) '(/ 3)))
  (ok (= (-<> 3 (expt 2)) (expt 3 2)))
  (ok (expands '(-<> 3 (expt 2)) '(expt 3 2)))
  (ok (equalp (-<> 3 (list <> <>)) '(3 3)))
  (ok (expands '(-<> 3 (list <> <>)) `(let ((#:ACC 3))
					(list #:ACC #:ACC)))))

(deftest test/&>
  (ok (= (&> 3 /) (/ 3)))
  (ok (expands '(&> 3 /) `(let ((#:ACC 3))
			    (when #:ACC (/ #:ACC)))))
  (ok (eq (&> nil ((lambda (x) (error "Should never run on ~a~%" x)))) nil))
  (ok (expands '(&> nil ((lambda (x) (error "Should never run on ~a~%" x))))
	       `(let ((#:ACC nil))
		  (when #:ACC ((lambda (x) (error "Should never run on ~a~%" x)) #:ACC))))))

(deftest test/&>>
  (ok (= (&>> 3 /) (/ 3)))
  (ok (expands '(&>> 3 /) `(let ((#:ACC 3))
			     (when #:ACC (/ #:ACC)))))
  (ok (eq (&>> nil (error "Should never run on ~a~%")) nil))
  (ok (expands '(&>> nil (error "Should never run on ~a~%"))
	       `(let ((#:ACC nil))
		  (when #:ACC (error "Should never run on ~a~%" #:ACC))))))

(deftest test/&<>
  (ok (= (&<> 3 /) (/ 3)))
  (ok (expands '(&<> 3 /) `(let ((#:ACC 3))
			     (when #:ACC (/ 3)))))
  (ok (eq (&<> nil ((lambda (x) (error "Should never run on ~a~%" x)))) nil))
  (ok (expands '(&<> nil ((lambda (x) (error "Should never run on ~a~%" x))))
	       `(let ((#:ACC nil))
		  (when #:ACC ((lambda (x) (error "Should never run on ~a~%" x)) nil))))))

(deftest test/&<>>
  (ok (= (&<>> 3 /) (/ 3)))
  (ok (expands '(&<>> 3 /) `(let ((#:ACC 3))
			      (when #:ACC (/ #:ACC)))))
  (ok (eq (&<>> nil (error "Should never run on ~a~%")) nil))
  (ok (expands '(&<>> nil (error "Should never run on ~a~%"))
	       `(let ((#:ACC nil))
		  (when #:ACC (error "Should never run on ~a~%" #:ACC))))))

(deftest test/z>>
  (ok (equalp (z>> '(1 2 3 4)
		   (mapcar #'1+)
		   (mapc (lambda (el) (format t "~a~%" el)))
		   (mapcar (lambda (x) (cons x x)))
		   (mapcar #'car))
	      '(2 3 4 5)))
  (ok (outputs (z>> '(1 2 3 4)
		    (mapcar #'1+)
		    (mapc (lambda (el) (format t "~a~%" el)))
		    (mapcar (lambda (x) (cons x x)))
		    (mapcar #'car))
	       (format nil "2~%3~%4~%5~%")))
  (ok (expands '(z>> '(1 2 3 4)
		 (mapcar #'1+)
		 (mapc (lambda (el) (format t "~a~%" el)))
		 (mapcar (lambda (x) (cons x x)))
		 (mapcar #'car))
	       `(let (#:RESULT)
		  (dolist (#:TOP '(1 2 3 4) (NREVERSE #:RESULT))
		    (push
		     (funcall #'car
			      (funcall (lambda (x) (cons x x))
				       (prog1 (funcall #'1+ #:TOP)
					 (funcall (lambda (el) (format t "~a~%" el))
						  (funcall #'1+ #:TOP)))))
		     #:RESULT))))))
