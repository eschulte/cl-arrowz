(defsystem "cl-arrowz"
  :description "Drop in replacement for cl-arrows with some bonus features"
  :long-description "Drop in replacement for cl-arrows with some bonus features.
The primary features of this replacement are a maintainer and a license."
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :version "1.0.0"
  :licence "Public Domain"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (cl-arrowz/cl-arrowz)
  :in-order-to ((test-op (test-op "cl-arrowz/tests"))))

(defsystem "cl-arrowz/tests"
  :class :package-inferred-system
  :depends-on (prove cl-arrowz/tests/cl-arrowz)
  ;; :build-operation "asdf:program-op"
  ;; :build-pathname "test-cl-arrowz"
  ;; :entry-point "cl-arrowz/tests/cl-arrowz::test"
  ;; :defsystem-depends-on (:prove-asdf)
  ;; :components ((:test-file "tests/cl-arrowz"))
  :components ((:file "tests/cl-arrowz"))
  :perform (test-op (o c)
                    (uiop:symbol-call :cl-arrowz/tests/cl-arrowz '#:test)))
