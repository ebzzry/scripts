;;; misc.lisp

(uiop:define-package :scripts/misc
    (:use #:cl
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:fare-utils
          #:cl-launch/dispatch)
  (:export #:getuid
           #:create-symlinks
           #:help))

(in-package :scripts/misc)

(exporting-definitions
 (defun getuid ()
   #+sbcl (sb-posix:getuid)
   #-sbcl (error "no getuid")) ;; use iolib?

 (defun create-symlinks (src)
   (let ((binarch (resolve-absolute-location `("~/bin") :ensure-directory t)))
     (with-current-directory (binarch)
       (dolist (i (cl-launch/dispatch:all-entry-names))
         (run `(ln -sf ,src ,i)))))
   (success))

 (defun help ()
   (format! t "~A commands: ~{~A~^ ~}~%" (get-name) (all-entry-names))
   (success)))

(register-commands :scripts/misc)
