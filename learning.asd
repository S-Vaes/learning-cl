(defclass md (cl-source-file)
  ((type :initform "md")))

(defclass org (cl-source-file)
  ((type :initform "org")))

(defclass pod (cl-source-file)
  ((type :initform "pod")))

(defsystem tutorial
  :version "0.1"
  :author "Sil Vaes"
  :license "MIT"
  :depends-on (#:papyrus #:named-readtables)
  :components ((:org "src/ch1.org"))
  :description "Learning Common Lisp")
