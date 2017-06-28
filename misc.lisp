(uiop:define-package :cl-scripts/misc
    (:use :cl
          :uiop
          :inferior-shell
          :cl-scripting
          :fare-utils
          :cl-launch/dispatch)
  (:export #:getuid
           #:create-symlinks
           #:help))

(in-package :cl-scripts/misc)

(exporting-definitions
 (defun getuid ()
   #+sbcl (sb-posix:getuid)
   #-sbcl (error "no getuid")) ;; use iolib?

 ;; (defun create-symlinks ()
 ;;   (let ((binarch (resolve-absolute-location `(,(getenv "BINDIR")) :ensure-directory t)))
 ;;     (with-current-directory (binarch)
 ;;       (dolist (i (cl-launch/dispatch:all-entry-names))
 ;;         (unless (file-exists-p i)
 ;;           (format t "linking file ~A~%" i)
 ;;           (run `(ln -s cl-scripts ,i))))))
 ;;   (success))

  (defun create-symlinks (src)
   (let ((binarch (resolve-absolute-location `(,(getenv "BINDIR")) :ensure-directory t)))
     (with-current-directory (binarch)
       (dolist (i (cl-launch/dispatch:all-entry-names))
         (format t "linking file ~A~%" i)
         (run `(ln -sf ,src ,i)))))
   (success))

 (defun help ()
   (format! t "~A commands: ~{~A~^ ~}~%" (get-name) (all-entry-names))
   (success)))

(register-commands :cl-scripts/misc)
