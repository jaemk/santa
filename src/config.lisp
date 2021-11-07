(defpackage santa.config
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :value
    :*values*
    :load-values))
(in-package :santa.config)
(named-readtables:in-readtable :interpol-syntax)


(defun s->log-level (s)
  (alexandria:eswitch (s :test #'equal)
    ("TRACE" :trace)
    ("DEBUG" :debug)
    ("INFO" :info)
    ("WARN" :warn)
    ("ERROR" :error)
    ("FATAL" :fatal)))


(defun env (var &key (default nil) (required nil))
  (or
    (->
      (some->
        (sb-ext:posix-getenv var)
        (santa.utils:trim-to-nil))
      ((lambda (v)
         (if (and required (not v))
           (error (format nil "env var is nil for required var ~a" var))
           v))))
    default))


(defun parse-boolean (s)
  (bind ((b (some-> s str:trim string-downcase)))
    (or
      (equal b "true")
      (equal b "t"))))


(defmacro get-version ()
  (when (uiop:probe-file* "commit_hash.txt")
    (->
      (str:from-file "commit_hash.txt")
      (str:trim)
      ((lambda (s)
         (format t "~&WARNING: loaded commit hash: ~a - should only be at build time!~%" s)
         s)))))


(defvar *values* nil)


(defun load-values ()
  (cl-dotenv:load-env ".env")
  (->
    (list
      (list :log-level
        (some-> (env "LOG_LEVEL" :default "info") (string-upcase) (s->log-level)))
      (list :dry-run
        (some-> (env "DRY_RUN" :default "true") (parse-boolean)))
      (list :load-local-people
        (some-> (env "LOAD_LOCAL_PEOPLE" :default "false") (parse-boolean)))
      (list :twilio-account-sid
        (env "TWILIO_ACCOUNT_SID" :required t))
      (list :twilio-sid
        (env "TWILIO_SID" :required t))
      (list :twilio-secret
        (env "TWILIO_SECRET" :required t))
      (list :twilio-number
        (env "TWILIO_NUMBER" :required t))
      (list :version
        (get-version))
      )
    ((lambda (vals)
       (setf *values* (make-hash-table))
       (loop for (k v) in vals do
         (setf (gethash k *values*) v))))))


(defun value (key)
  (when (null *values*)
    (load-values))
  (gethash key *values*))

