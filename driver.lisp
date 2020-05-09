;;;; driver.lisp

(uiop:define-package :scripts/driver
  (:nicknames :scripts)
  (:use :uiop/common-lisp)
  (:use-reexport #:scripts/common
                 #:scripts/misc
                 #:scripts/touchpad
                 #:scripts/general
                 #:scripts/apps
                 #:scripts/unix
                 #:scripts/mksum
                 #:scripts/webcam
                 #:scripts/touchring
                 #:scripts/main))

(provide "scripts")
(provide "SCRIPTS")
