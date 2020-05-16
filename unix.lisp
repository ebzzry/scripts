;;;; unix.lisp

(uiop:define-package #:scripts/unix
  (:use #:cl
        #:inferior-shell
        #:cl-scripting
        #:optima
        #:optima.ppcre
        #:marie
        #:scripts/common))

(in-package #:scripts/unix)

(% md "mkdir -p")
(% rm! "rm -rf")
(% ln! "ln -sf")

(% l  "ls -tr -A -F --color")
(% ll "l -l")
(% la "ls -A -F --color")
(% lk "la -l")

(% l! "l -R")
(% lh "l -H")
(% l1 "l -1")

(defun* lv (&rest args)
  (run/i `(pipe (l ,@args) (less)))
  (success))

(defun* sush (&rest args)
  (run/i `(sudo "sh" "-c" ,(fmt "~{~A~^ ~}" args)))
  (success))

(register-commands :scripts/unix)
