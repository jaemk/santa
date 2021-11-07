(defpackage santa
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :main
    :run-main))
(in-package :santa)
(named-readtables:in-readtable :interpol-syntax)


(define-condition pair-failure (error)
  ((str
     :initarg :str
     :reader str
    ))
  (:report
    (lambda (c stream)
      (format stream "failed to pair: ~a" (str c)))))


(define-condition sms-error (error)
  ((str
     :initarg :str
     :reader str
    ))
  (:report
    (lambda (c stream)
      (format stream "failed to send sms: ~a" (str c)))))


(defun send-message (phone-number msg &key (dry-run nil))
  (restart-case
    (bind ((twilio-account-sid (santa.config:value :twilio-account-sid))
           (twilio-sid (santa.config:value :twilio-sid))
           (twilio-secret (santa.config:value :twilio-secret))
           (twilio-number (santa.config:value :twilio-number))
           (dry-msg (if dry-run "[DRY RUN] " "")))
      (log:info "~asending msg to number ~a~%~a" dry-msg phone-number msg)
      (when (not dry-run)
        (->>
          (drakma:http-request
            #?"https://api.twilio.com/2010-04-01/Accounts/${twilio-account-sid}/Messages.json"
            :method :post
            :basic-authorization (list twilio-sid twilio-secret)
            :parameters
              (list
                (cons "From" twilio-number)
                (cons "To" phone-number)
                (cons "Body" msg)))
          (flexi-streams:octets-to-string)
          (cl-json:decode-json-from-string)
          ((lambda (r)
             (log:debug "sms response: ~a" r)
             r))
          ((lambda (r)
             (bind ((st (santa.utils:aget :status r)))
               (when (not (or (equal "accepted" st)
                              (equal "queued" st)
                              (equal "sending" st)
                              (equal "sent" st)
                              (equal "delivered" st)))
                 (error 'sms-error
                   :str (format nil "unexpected sms status: ~a" st)))))))))
    (return-nil
      ()
      :report "Return nil"
      nil)
    (retry-sms
      ()
      :report "Retrying sms"
      (send-message phone-number msg :dry-run dry-run))
    (retry-sms-with-number
      (new-number)
      :report "Retrying sms with new number"
      :interactive (lambda ()
                     (santa.utils:interactive-restart-prompt-new-value
                       "Please enter a new phone-number (as a string!): "))
      (send-message new-number msg :dry-run dry-run))))


(defun get-phone-number (person people-numbers)
  (second (assoc person people-numbers :test #'equal)))


(defun format-admin-message (pairs)
  (bind ((header "Secret Santa Time!")
         (str-pairs (format nil "~{~{~a~^: ~}~^~%~}" pairs))
         (admin "You are receiving this because you are an admin.")
         (footer "Do not reply to this number."))
    #?"\n${header}\n\n${str-pairs}\n\n${admin}\n\n${footer}"))


(defun format-message (giver receiver blurbs)
  (bind ((tgiver (str:title-case giver))
         (treceiver (str:title-case receiver))
         (header "Secret Santa Time!")
         (assign #?"Hi, ${tgiver}! You have been assigned to ${treceiver}!")
         (likes (second (assoc receiver blurbs :test #'equal)))
         (money "Spend target: $50ish")
         (footer "Do not reply to this number")
         )
    #?"\n${header}\n\n${assign}\n\n${likes}\n\n${money}\n\n${footer}"))


(defun get-allowed (person people forbidden-pairs)
  (as-> person v
    (assoc v forbidden-pairs :test #'equal)
    (second v)
    (adjoin person v :test #'equal)
    (set-difference people v :test #'equal)
    (santa.utils:shuffle-list v)))


(defun check-reciprocal-pairs (pairs)
  ; I have you and you have me and he has she and she has he
  (loop for (gifter giftee) in pairs
        do (bind ((giftee-giftee (second (assoc giftee pairs :test #'equal))))
             (when (equal gifter giftee-giftee)
               (error 'pair-failure
                      :str (format
                             nil "found reciprocal pair ~a <> ~a out of ~a" gifter giftee pairs)))))
  pairs)


(defun pair-with-restarts (people forbidden-pairs)
  (restart-case
    (->
      (bind ((people (santa.utils:shuffle-list people))
             (remaining-receivers (copy-list people))
             (result nil)
             (allowed (loop for p in people
                            collect (list
                                      p
                                      (get-allowed p people forbidden-pairs)))))
        (loop for giver in people
              do (bind ((allow (second (assoc giver allowed :test #'equal)))
                        (allow (intersection allow remaining-receivers))
                        (pick (first allow)))
                   (when (null pick)
                     (error 'pair-failure
                            :str (format nil "no possibilities remaining for person ~a~%existing-pairs: ~a" giver result)))
                   (push (list giver pick) result)
                   (setf remaining-receivers
                         (set-difference remaining-receivers
                                         (list pick)
                                         :test #'equal))))
        result)
      (check-reciprocal-pairs)
      )
    (return-nil
      ()
      :report "Return nil"
      nil)
    (retry-pair
      ()
      :report "Retry pairing"
      (pair-with-restarts people forbidden-pairs))))


(defun pair (people forbidden-pairs)
  (bind ((attempts 1))
    (handler-bind
      ((pair-failure
         (lambda (c)
           (when (> attempts 10)
             (error (format nil "failed to pair after ~a attempts" attempts)))
           (log:error "pair failure (~a), will retry~%... ~a" attempts (str c))
           (incf attempts)
           (invoke-restart 'retry-pair))))
      (pair-with-restarts people forbidden-pairs))))


(defun run-main ()
  (log:info "people:~%~{~a~^~%~}" santa.people:*people-numbers*)
  (log:info "forbidden pairs:~%~{~a~^~%~}" santa.people:*forbidden-pairs*)
  (bind ((people (loop for (name num) in santa.people:*people-numbers* collect name))
         (pairs (pair people santa.people:*forbidden-pairs*))
         (messages (mapcar
                     (lambda (gr)
                       (bind (((g r) gr))
                         (format-message g r santa.people:*blurbs*)))
                     pairs))
         (admin-message (format-admin-message pairs))
         (dry-run (santa.config:value :dry-run))
         )
    (log:info "pairs:~%~{~{~a~^: ~}~^~%~}" pairs)
    (mapc
      (lambda (gr msg)
        (bind (((g r) gr)
               (giver-number (get-phone-number g santa.people:*people-numbers*)))
          (send-message giver-number msg :dry-run dry-run)))
      pairs
      messages)
    (mapc
      (lambda (admin-number)
        (send-message admin-number admin-message :dry-run dry-run))
      santa.people:*admin-numbers*)))


(defun main (argvs)
  ;; handle any errors if they aren't cause by the catch-all handler in 'main
  (setf
    *debugger-hook*
    (lambda (c old-hook)
      (declare (ignore old-hook))
      (format *error-output* "~&Unhandled error: ~a~%" (santa.utils:get-error-backtrace c))
      (sb-ext:quit :unix-status 1)))

  (handler-bind
    (
      ;; C-c
      (sb-sys:interactive-interrupt
        (lambda (c)
          (format t "~&Aborting...~%")
          (sb-ext:quit :unix-status 1)))

      ;; everything else
      (error
        (lambda (c)
          (format *error-output* "~&Error: ~a~%" (santa.utils:get-error-backtrace c))
          (sb-ext:quit :unix-status 1)))
    )
      (progn
        (log:config (santa.config:value :log-level))
        (log:config :sane2)
        (log:config :nofile)
        (log:debug "args: ~a" argvs)
        (run-main))))

