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

(in-package #:scripts/misc)

(exporting-definitions
  (defun getuid ()
    #+sbcl (sb-posix:getuid)
    #+cmu (unix:unix-getuid)
    #+clisp (posix:uid)
    #+ecl (ext:getuid)
    #+ccl (ccl::getuid)
    #+allegro (excl.osi:getuid)
    #-(or sbcl cmu clisp ecl ccl allegro) (error "no getuid"))

  (defun create-symlinks (src)
    (let* ((directory (or (getenv "DEST") "~/bin"))
           (destination (uiop:truenamize directory)))
      (with-current-directory (destination)
        (dolist (i (cl-launch/dispatch:all-entry-names))
          (run `(ln -sf ,src ,i)))))
    (success))

  (defun help ()
    (format! t "~A commands: ~{~A~^ ~}~%" (get-name) (all-entry-names))
    (success)))

(register-commands :scripts/misc)
