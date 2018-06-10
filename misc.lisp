;;;; misc.lisp

(uiop:define-package #:scripts/misc
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
    (let* ((directory (or (getenv "BIN") "~/bin"))
           (destination (uiop:truenamize directory)))
      (uiop:ensure-all-directories-exist (list (concatenate 'string (uiop:native-namestring destination) "/")))
      (with-current-directory (destination)
        (dolist (i (cl-launch/dispatch:all-entry-names))
          (run `(ln -sf ,src ,i)))))
    (success))

  (defun help ()
    (format! t "~A commands: ~{~A~^ ~}~%" (get-name) (all-entry-names))
    (success)))

(register-commands :scripts/misc)
