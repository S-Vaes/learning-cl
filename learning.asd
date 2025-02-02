(asdf:defsystem "learning"
  :version "0.1.0"
  :author "Sil Vaes"
  :license "MIT"
  :depends-on (#:iterate
               #:alexandria)
  :description "Learning Common Lisp"
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main")))))
