(in-package #:sbcl-generations/tests)

(defvar *test-count* 0
  "The number of assertions executed by the current test run.")

(defun test-assert (condition description)
  "Record and require CONDITION for DESCRIPTION."
  (incf *test-count*)
  (unless condition
    (error "sbcl-generations test failed: ~A" description))
  t)

(defun checkpoint-stage (thunk)
  "Return the stage THUNK's CHECKPOINT-ERROR reports, or NIL when it returns."
  (handler-case
      (progn (funcall thunk) nil)
    (checkpoint-error (condition)
      (checkpoint-error-stage condition))))

(defun tests--temporary-root ()
  "Create and return a fresh temporary test directory."
  (let ((root (merge-pathnames
               (format nil "sbcl-generations-tests-~36R-~36R/"
                       (get-universal-time)
                       (random most-positive-fixnum))
               (uiop:temporary-directory))))
    (ensure-directories-exist root)
    root))

(defun tests--write-core (pathname bytes)
  "Write a placeholder core file of BYTES octets at PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sequence (make-array bytes :element-type '(unsigned-byte 8)
                                      :initial-element 0)
                    stream))
  pathname)
