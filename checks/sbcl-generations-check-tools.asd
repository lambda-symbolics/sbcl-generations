;;;; -- Source Check Tools --

;;; qlot installs project deps by reading ASDF system definitions.
;;; This system declares cheque, the source checker loaded by the ./script/check script.
;;; This is so that installing checks/qlfile also installs cheque's own dependencies.

(asdf:defsystem #:sbcl-generations-check-tools
  :description "The devel tools loaded for sbcl-generations's source checks."
  :depends-on  (#:cheque))
