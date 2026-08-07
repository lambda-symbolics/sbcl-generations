(in-package #:sbcl-generations)

;;;; -- Selection --

(defvar *checkpoint-in-progress-p* nil
  "True while this process has one checkpoint that has not finished publishing.")

(defun generation-list (store)
  "Return STORE's valid retained generations, newest first.

A directory whose manifest is missing or unreadable is skipped rather than
reported: an interrupted checkpoint leaves one behind, and listing must keep
working after that."
  (let ((root (generation-store-root store)))
    (if (probe-file root)
        (sort (loop for directory in (uiop:subdirectories root)
                    for manifest = (merge-pathnames "manifest.sexp" directory)
                    for generation = (and (probe-file manifest)
                                          (handler-case
                                              (generation-load-manifest manifest
                                                                        store)
                                            (error ()
                                              nil)))
                    when generation
                      collect generation)
              #'>
              :key #'generation-created-at)
        nil)))

(defun generation-find (store identifier)
  "Return STORE's retained generation IDENTIFIER, or NIL when it is unknown."
  (find identifier (generation-list store)
        :key #'generation-identifier
        :test #'string=))

(defun generation--select-form (generation)
  "Return the pointer form naming GENERATION as the next boot."
  (list :current-generation
        :version 1
        :id (generation-identifier generation)
        :manifest (namestring (generation-manifest-pathname generation))))

(defun generation-select (store generation)
  "Select compatible GENERATION for STORE's next startup and return it."
  (when *checkpoint-in-progress-p*
    (generations--fail :selection
                       "A generation cannot be selected while a checkpoint publishes."
                       :pathname (generation-store-current-pathname store)))
  (unless (generation-compatible-p generation store)
    (generations--fail :selection
                       (format nil "Generation ~A is not compatible with this runtime."
                               (generation-identifier generation))
                       :pathname (generation-manifest-pathname generation)))
  (generation--write-form store
                          (generation-store-current-pathname store)
                          (generation--select-form generation))
  generation)

(defun generation-selected (store)
  "Return STORE's selected generation, or NIL when the pointer is not usable.

The pointer is confined to the store root, so a rewritten pointer cannot make a
process boot a core from anywhere else on the filesystem."
  (let ((pathname (generation-store-current-pathname store)))
    (when (probe-file pathname)
      (handler-case
          (let* ((record (generation--read-form store pathname))
                 (identifier (and (listp record) (getf (rest record) :id)))
                 (manifest (and (listp record) (getf (rest record) :manifest))))
            (when (and (listp record)
                       (eq (first record) :current-generation)
                       (stringp identifier)
                       (plusp (length identifier))
                       (stringp manifest)
                       (plusp (length manifest))
                       (uiop:subpathp (pathname manifest)
                                      (generation-store-root store))
                       (probe-file manifest))
              (let ((generation (generation-load-manifest manifest store)))
                (and (string= identifier (generation-identifier generation))
                     generation))))
        (error ()
          nil)))))

(defun generation-request-rollback (store identifier)
  "Select generation IDENTIFIER in STORE and signal ROLLBACK-REQUESTED.

The signal is the whole point: this library cannot know how to restart the
calling process, so it selects the generation durably and then hands control to
the host's handler."
  (let ((generation (generation-find store identifier)))
    (unless generation
      (generations--fail :selection
                         (format nil "Unknown retained generation ~A." identifier)))
    (generation-select store generation)
    (error 'rollback-requested
           :message (format nil "Rollback requested for retained generation ~A."
                            (generation-identifier generation))
           :generation-identifier (generation-identifier generation))))
