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

(defun digest-sum-files (digest-name &rest files)
  (unless files
    (error "no files given to digest"))
  (loop :with buffer = (make-array 8192 :element-type '(unsigned-byte 8))
     :with digest = (make-array (ironclad:digest-length digest-name)
                                :element-type '(unsigned-byte 8))
     :for file :in files
     :for digester = (ironclad:make-digest digest-name)
     :then (reinitialize-instance digester)
     :do (ironclad:digest-file digester file :buffer buffer :digest digest)
       (format t "~A ~A~%" (file-namestring file)
               (ironclad:byte-array-to-hex-string digest))))

(exporting-definitions
 (defun mksum (&rest args)
   (declare (ignore args))
   (cond ((getopt :short-name "h"
                  :context (make-context)) (help) (exit))
         ((string= (getopt :short-name "a"
                           :context (make-context)) "md5") (eval `(digest-sum-files 'md5
                                                                                    ,@(remainder)))
                                                           (exit))
         ((string= (getopt :short-name "a"
                           :context (make-context)) "sha256") (eval `(digest-sum-files 'sha256
                                                                                       ,@(remainder)))
                                                              (exit))
         ((string= (getopt :short-name "a"
                           :context (make-context)) "sha1") (eval `(digest-sum-files 'sha1
                                                                                     ,@(remainder)))
                                                            (exit))
         
         ((remainder) (eval `(digest-sum-files 'sha256
                                               ,@(remainder)))
                      (exit))
         (t (help)
            (exit)))))

(register-commands :scripts/mksum)

