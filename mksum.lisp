;;;; mksum.lisp

(uiop:define-package :scripts/mksum
    (:use #:cl
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:fare-utils
          #:net.didierverna.clon
          #:ironclad)
  (:export #:mksum))

(in-package :scripts/mksum)

(defsynopsis (:postfix "FILES...")
    (text :contents "A program to print either MD5, SHA-1, or SHA-256 checksums. With no options
provided, prints the SHA-256 checksum by default.")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit."))
  (group (:header "Other options:")
         (stropt :short-name "a" :long-name "algorithm"
                 :description "Select algorithm to use.")))

(defun digest-files (name &rest files)
  (unless files
    (error "no files given to digest"))
  (loop :with buffer = (make-array 8192 :element-type '(unsigned-byte 8))
     :with digest = (make-array (digest-length name)
                                :element-type '(unsigned-byte 8))
     :for file :in files
     :for digester = (make-digest name)
     :then (reinitialize-instance digester)
     :do (digest-file digester file :buffer buffer :digest digest)
       (format t "~A ~A~%" (file-namestring file)
               (byte-array-to-hex-string digest))))

(defun check-context (algorithm)
  (string= (getopt :short-name "a" :context (make-context)) algorithm))

(defun digest-to (algorithm)
  (apply #'digest-files algorithm (remainder))
  (exit))

(exporting-definitions
 (defun mksum (&rest args)
   (declare (ignore args))
   (cond ((getopt :short-name "h"
                  :context (make-context)) (help)
          (exit))
         ((check-context "md5") (digest-to 'md5))
         ((check-context "sha256") (digest-to 'sha256))
         ((check-context "sha1") (digest-to 'sha1))
         ((remainder) (digest-to 'sha256))
         (t (help)
            (exit)))))

(register-commands :scripts/mksum)

