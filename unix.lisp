;;;; unix.lisp

(uiop:define-package #:scripts/unix
    (:use #:cl
          #:fare-utils
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:optima
          #:optima.ppcre
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/utils)
  (:export #:md
           #:rm!
           #:ln!
           #:la
           #:lk
           #:l
           #:ll
           #:l!
           #:lh
           #:l1
           #:f
           #:lv
           #:sush))

(in-package #:scripts/unix)

(exporting-definitions
 (% md "mkdir -p")
 (% rm! "rm -rf")
 (% ln! "ln -sf")

 (% l  "exa --all --time modified --sort newest")
 (% ll "l --long --git --time-style long-iso --group-directories-first")
 (% la "exa --all ")
 (% lk "la --long --git --time-style long-iso --group-directories-first")

 (% l! "l -R")
 (% lh "l -H")
 (% l1 "l -1"))

(exporting-definitions
 (defun lv (&rest args)
   (run/i `(pipe (l ,@args) (less)))
   (success))

 (defun sush (&rest args)
   (run/i `(sudo "sh" "-c" ,(mof:fmt "~{~A~^ ~}" args)))
   (success)))

(register-commands :scripts/unix)
