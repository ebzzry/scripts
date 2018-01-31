;;; unix.lisp

(uiop:define-package
    :scripts/unix
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
  (:export #:x
           #:c
           #:v

           #:md
           #:rm!
           #:ln!

           #:g
           #:gi

           #:la
           #:lk

           #:l
           #:ll
           #:l!
           #:lh
           #:l1

           #:f
           #:lv))

(in-package :scripts/unix)

(exporting-definitions
 (% x "zsh -c")
 (% c "cat")
 (% v "less")

 (% md "mkdir -p")
 (% rm! "rm -rf")
 (% ln! "ln -sf")

 (% g "egrep --color")
 (% gi "g -i")

 (% la "ls -A -F --color ")
 (% lk "la -l")

 (% l  "la -tr")
 (% ll "l -l")
 (% l! "l -R")
 (% lh "l -H")
 (% l1 "l -1"))

(exporting-definitions
 (defun f (arg)
   (run/i `(find "." -iname ,(format nil "*~A*" arg)))
   (success))

 (defun lv (&rest args)
   (run/i `(pipe (l ,@args) (less)))
   (success)))

(register-commands :scripts/unix)
