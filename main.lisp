;;;; main.lisp

(uiop:define-package #:scripts/main
  (:use #:cl))

(in-package #:scripts/main)

(defun main (argv)
  (declare (ignore argv))
  (success))
