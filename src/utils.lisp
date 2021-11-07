(defpackage santa.utils
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :now-millis
    :get-error-backtrace
    :with-timing
    :aget
    :trim-to-nil
    :insert-at
    :shuffle-list
    :interactive-restart-prompt-new-value))
(in-package :santa.utils)
(named-readtables:in-readtable :interpol-syntax)


(defun now-millis ()
  (bind ((now (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix now))
     (local-time:timestamp-millisecond now))))


(defun aget (key alist)
  (->> (assoc key alist) rest))


(defun trim-to-nil (s)
  (some->
    s
    str:trim
    ((lambda (s)
       (if (zerop (length s))
         nil
         s)))))


(defun interative-restart-prompt-new-value (prompt)
  "for prompting for values when restarting from the debugger"
  (format *query-io* prompt)  ;; *query-io*: the special stream to make user queries
  (force-output *query-io*)   ;; ensure the user sees what's typed
  (list (read *query-io*)))   ;; interative restart expects a list


(defun insert-at (lst item index)
  "In-place insert, except when inserting a new head at 0
   the original variable will still be pointing at the old head"
  (cond
    ((< index 0)
       (error "Index too small ~A" index))
    ((= index 0)
       (cons item lst))
    (t
       (push item (cdr (nthcdr (- index 1) lst)))
       lst)))


(defun shuffle-list (lst)
  "return a new shuffled list of the original"
  (bind ((res nil))
    (loop for v in lst
          for i from 0
          do (bind ((at (ironclad:strong-random (1+ i))))
               (setf res
                     (insert-at res v at))))
    res))


(defmacro get-error-backtrace (e)
  (bind ((s (gensym)))
    `(bind ((,s (make-string-output-stream)))
       (progn
         (trivial-backtrace:print-backtrace ,e :output ,s)
         (get-output-stream-string ,s)))))


(defmacro with-timing (&rest forms)
  (bind ((s (gensym))
         (r (gensym))
         (ms (gensym)))
    `(bind ((,s (santa.utils:now-millis))
            (,r (progn ,@forms))
            (,ms (- (santa.utils:now-millis) ,s)))
       (values ,r ,ms))))

