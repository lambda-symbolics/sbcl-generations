(in-package #:sbcl-generations)

;;;; -- Generation Store --

(defclass generation-store ()
  ((root
    :initarg :root
    :reader generation-store-root
    :type pathname
    :documentation "The directory holding one subdirectory per generation.")
   (current-pathname
    :initarg :current-pathname
    :reader generation-store-current-pathname
    :type pathname
    :documentation "The pointer file naming the generation selected for restart.")
   (core-name
    :initarg :core-name
    :reader generation-store-core-name
    :type string
    :documentation "The published core file name inside a generation directory.")
   (temporary-core-name
    :initarg :temporary-core-name
    :reader generation-store-temporary-core-name
    :type string
    :documentation "The unpublished core file name the saver child writes.")
   (manifest-version
    :initarg :manifest-version
    :reader generation-store-manifest-version
    :type (integer 1)
    :documentation "The manifest version this store writes, owned by the host.")
   (accepted-manifest-versions
    :initarg :accepted-manifest-versions
    :reader generation-store-accepted-manifest-versions
    :type list
    :documentation "Every manifest version this store still loads.")
   (manifest-validator
    :initarg :manifest-validator
    :reader generation-store-manifest-validator
    :type (or null function)
    :documentation "A host check of one loaded manifest plist, or NIL for none.")
   (publish-validator
    :initarg :publish-validator
    :reader generation-store-publish-validator
    :type (or null function)
    :documentation "A host check of one generation's artifacts before publication."))
  (:documentation "Where retained generations live and how their manifests read."))

(defun make-generation-store
    (&key
       root
       current-pathname
       (core-name "generation.core")
       (temporary-core-name ".generation.core.tmp")
       (manifest-version 1)
       (accepted-manifest-versions nil)
       manifest-validator
       publish-validator)
  "Create a validated generation store beneath ROOT.

CURRENT-PATHNAME names the pointer file recording which generation should be
booted next; keep it outside ROOT when the host wants selection to survive
clearing the generations themselves.

MANIFEST-VERSION is written into every new manifest and belongs to the host,
because the host owns the metadata the manifest carries.
ACCEPTED-MANIFEST-VERSIONS lists every version still readable and defaults to
MANIFEST-VERSION alone. MANIFEST-VALIDATOR receives one loaded manifest plist
and signals when the host's own fields are missing or malformed; the library
checks only its own fields. PUBLISH-VALIDATOR receives one generation just
before its core is renamed into place and signals when a host artifact the
generation depends on is missing."
  (unless (and root current-pathname)
    (generations--fail :selection
                       "A generation store needs a root and a pointer pathname."))
  (unless (typep manifest-version '(integer 1))
    (generations--fail :selection
                       "A generation manifest version must be a positive integer."))
  (make-instance 'generation-store
                 :root (uiop:ensure-directory-pathname root)
                 :current-pathname (pathname current-pathname)
                 :core-name core-name
                 :temporary-core-name temporary-core-name
                 :manifest-version manifest-version
                 :accepted-manifest-versions
                 (or accepted-manifest-versions (list manifest-version))
                 :manifest-validator manifest-validator
                 :publish-validator publish-validator))


;;;; -- Generations --

(defclass generation ()
  ((identifier
    :initarg :identifier
    :reader generation-identifier
    :type string
    :documentation "The stable generation identifier, also its directory name.")
   (directory
    :initarg :directory
    :reader generation-directory
    :type pathname
    :documentation "The directory containing this generation's artifacts.")
   (core-pathname
    :initarg :core-pathname
    :reader generation-core-pathname
    :type pathname
    :documentation "The atomically published core pathname.")
   (temporary-core-pathname
    :initarg :temporary-core-pathname
    :reader generation-temporary-core-pathname
    :type pathname
    :documentation "The unpublished core pathname the saver child writes.")
   (manifest-pathname
    :initarg :manifest-pathname
    :reader generation-manifest-pathname
    :type pathname
    :documentation "The portable manifest pathname.")
   (metadata
    :initarg :metadata
    :initform nil
    :reader generation-metadata
    :type list
    :documentation "Host properties spliced into the manifest and probe record.")
   (created-at
    :initarg :created-at
    :reader generation-created-at
    :type (integer 0)
    :documentation "The creation time as a Common Lisp universal time.")
   (status
    :initarg :status
    :initform :pending
    :accessor generation-status
    :type (member :pending :ready :failed)
    :documentation "This process's view of the publication outcome.")
   (coordinator-pid
    :initform nil
    :accessor generation-coordinator-pid
    :type (or null integer)
    :documentation "The coordinator process identifier while publication runs."))
  (:documentation "One saved image paired with the identity it was saved at."))

(defun generation--create-record (store identifier &key metadata)
  "Return a pending generation named IDENTIFIER within STORE."
  (let ((directory (merge-pathnames (format nil "~A/" identifier)
                                    (generation-store-root store))))
    (make-instance 'generation
                   :identifier identifier
                   :directory directory
                   :core-pathname
                   (merge-pathnames (generation-store-core-name store) directory)
                   :temporary-core-pathname
                   (merge-pathnames (generation-store-temporary-core-name store)
                                    directory)
                   :manifest-pathname (merge-pathnames "manifest.sexp" directory)
                   :metadata (copy-list metadata)
                   :created-at (get-universal-time)
                   :status :pending)))

(defun generation--runtime-properties ()
  "Return the runtime identity a saved core must be booted by to be usable."
  (list :sbcl-version (lisp-implementation-version)
        :operating-system (software-type)
        :operating-system-version (software-version)
        :architecture (machine-type)))

(defun generation-manifest-form (generation store)
  "Return the portable ready manifest for GENERATION within STORE.

Host metadata is spliced in flat rather than nested, so a host replacing its own
reader with this library keeps reading the manifests it already wrote."
  (append (list :generation
                :version (generation-store-manifest-version store)
                :id (generation-identifier generation)
                :core (namestring (generation-core-pathname generation)))
          (copy-list (generation-metadata generation))
          (generation--runtime-properties)
          (list :created-at (generation-created-at generation))))

(defun generation-core-probe-record (generation)
  "Return the exact identity GENERATION's saved core must report when booted."
  (append (list :sbcl-generations-core
                :version 1
                :generation-id (generation-identifier generation))
          (copy-list (generation-metadata generation))))

(defun generation-core-probe-output (record)
  "Return canonical one-line output for a saved-core probe RECORD.

Every printer variable is bound, so two processes agree on the text regardless
of the printer state either of them inherited."
  (with-output-to-string (stream)
    (let ((*print-base* 10)
          (*print-case* :upcase)
          (*print-circle* nil)
          (*print-length* nil)
          (*print-level* nil)
          (*print-pretty* nil)
          (*print-radix* nil)
          (*print-readably* t))
      (write record :stream stream)
      (terpri stream))))

(defun generation--write-form (pathname form)
  "Atomically publish portable FORM at PATHNAME."
  (snapshot-write pathname form)
  pathname)

(defun generation--read-form (pathname)
  "Return the single portable form stored at PATHNAME."
  (multiple-value-bind (form sole-form-p)
      (snapshot-read pathname)
    (unless sole-form-p
      (generations--fail :manifest
                         "A generation record does not hold exactly one form."
                         :pathname pathname))
    form))

(defun generation-record-failure (generation stage detail)
  "Record a bounded checkpoint failure at STAGE beside GENERATION's artifacts.

The detail is truncated, because it may quote a condition report that a host
would rather not grow without bound in its state directory."
  (let ((pathname (merge-pathnames "failure.sexp"
                                   (generation-directory generation)))
        (text (let ((printed (princ-to-string detail)))
                (if (> (length printed) 4000)
                    (subseq printed 0 4000)
                    printed))))
    (generation--write-form pathname
                            (list :checkpoint-failure
                                  :version 1
                                  :id (generation-identifier generation)
                                  :time (get-universal-time)
                                  :stage stage
                                  :detail text))
    pathname))


;;;; -- Manifest Loading --

(defun generation-load-manifest (pathname store)
  "Load and validate one ready generation manifest from PATHNAME within STORE."
  (let* ((pathname (pathname pathname))
         (form (generation--read-form pathname)))
    (unless (and (listp form)
                 (eq (first form) :generation)
                 (member (getf (rest form) :version)
                         (generation-store-accepted-manifest-versions store))
                 (stringp (getf (rest form) :id))
                 (plusp (length (getf (rest form) :id)))
                 (stringp (getf (rest form) :core)))
      (generations--fail :manifest
                         (format nil "Invalid generation manifest at ~A." pathname)
                         :pathname pathname))
    (let* ((properties (rest form))
           (directory (uiop:pathname-directory-pathname pathname))
           (core-pathname (pathname (getf properties :core))))
      (unless (uiop:subpathp core-pathname directory)
        (generations--fail :manifest
                           "A generation core is outside its artifact directory."
                           :pathname pathname))
      (let ((validator (generation-store-manifest-validator store)))
        (when validator
          (funcall validator properties pathname)))
      (make-instance 'generation
                     :identifier (getf properties :id)
                     :directory directory
                     :core-pathname core-pathname
                     :temporary-core-pathname
                     (merge-pathnames (generation-store-temporary-core-name store)
                                      directory)
                     :manifest-pathname pathname
                     :metadata (copy-list properties)
                     :created-at (or (getf properties :created-at) 0)
                     :status :ready))))

(defun generation-compatible-p (generation)
  "Return true when GENERATION has a plausible core for this exact runtime.

A core saved by a different SBCL build, operating system release, or machine
type cannot be booted here, so an incompatible generation is reported rather
than offered for selection."
  (not (null
        (handler-case
            (let ((manifest (generation--read-form
                             (generation-manifest-pathname generation))))
              (and (probe-file (generation-core-pathname generation))
                   (with-open-file (stream (generation-core-pathname generation)
                                           :direction :input
                                           :element-type '(unsigned-byte 8))
                     (> (file-length stream) 1048576))
                   (loop for (key value) on (generation--runtime-properties)
                           by #'cddr
                         always (equal (getf (rest manifest) key) value))
                   t))
          (error ()
            nil)))))
