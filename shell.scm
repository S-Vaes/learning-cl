(use-modules (guix packages)
             (my-channel papyrus)
             (my-channel alive-lsp)
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
         sbcl-alexandria
         sbcl-alive-lsp))
  (native-inputs
   (list sbcl-rove
         sbcl-lisp-critic))
  (synopsis "Learning Common Lisp")
  (description "Learning Common Lisp")
  (home-page #f)
  (license #f))
