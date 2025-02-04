(use-modules (guix packages)
             (my-channel papyrus)
             (gnu packages lisp)
             (gnu packages lisp-xyz))

(package
  (name "learning-dev")
  (version "0.1.0")
  (source #f)
  (build-system asdf-build-system/sbcl)
  (inputs
   (list sbcl
         cl-asdf
         sbcl-named-readtables
         sbcl-papyrus
         sbcl-iterate
         sbcl-alexandria))
  (native-inputs
   (list sbcl-rove))
  (synopsis "Learning Common Lisp")
  (description "Learning Common Lisp")
  (home-page #f)
  (license #f))
