(uiop:define-package
    :scripts/unix
    (:use
     :cl
     :fare-utils
     :uiop
     :inferior-shell
     :cl-scripting
     :optima
     :optima.ppcre
     :cl-ppcre
     :local-time
     :cl-launch/dispatch
     :scripts/misc
     :scripts/utils)
  (:export #:x
           #:c
           #:v
           #:f
           #:md
           #:rm!
           #:sl

           #:l0
           #:la
           #:lk
           #:l
           #:ll
           #:lr
           #:lh
           #:l1
           #:lv

           #:g
           #:gi))

(in-package :scripts/unix)

(exporting-definitions
 (defun x (&rest args)
   (run/i `(zsh -c ,@args)))
 (defun c (&rest args)
   (run/i `(cat ,@args))
   (success))
 (defun v (&rest args)
   (run/i `(less ,@args))
   (success))
 (defun f (arg)
   (run/i `(find "." -iname ,(format nil "*~A*" arg)))
   (success))
 (defun md (&rest args)
   (run/i `(mkdir -p ,@args))
   (success))
 (defun rm! (&rest args)
   (run/i `(rm -rf ,@args))
   (success))
 (defun sl (&rest args)
   (run/i `(ln -sf ,@args))
   (success))

 (defun l0 (&rest args)
   (run/i `(ls "-A" "-F" "--color" ,@args))
   (success))
 (defun la (&rest args)
   (apply-args-1 'l0 args :options '("-A" "-F" "--color")))
 (defun lk (&rest args)
   (apply-args-1 'la args :options '("-l")))
 (defun l (&rest args)
   (apply-args-1 'la args :options '("-tr")))
 (defun ll (&rest args)
   (apply-args-1 'l args :options '("-l")))
 (defun lr (&rest args)
   (apply-args-1 'l args :options '("-R")))
 (defun lh (&rest args)
   (apply-args-1 'l args :options '("-H")))
 (defun l1 (&rest args)
   (apply-args-1 'l args :options '("-1")))
 (defun lv (&rest args)
   (run/i `(pipe (l ,@args) (less)))
   (success))

 (defun g (&rest args)
   (run/i `(egrep "--color" ,@args))
   (success))
 (defun gi (&rest args)
   (apply-args-1 'g args :options '("-i"))))

(register-commands :scripts/unix)
