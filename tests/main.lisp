(defpackage santa/tests
  (:use :cl
        :metabang-bind
        :arrow-macros
        :fiveam
        :santa)
  (:export :all))
(in-package :santa/tests)

(def-suite all
  :description "Tests")

(setf fiveam:*on-failure* :backtrace)
(setf fiveam:*on-error* :backtrace)

(log:config (santa.config:value :log-level))
(log:config :sane2)
(log:config :nofile)

