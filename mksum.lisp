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
  (text :contents "Print either MD5, SHA-1, or SHA-256 file/directory checksums. With no options
provided, prints the SHA-256 checksum by default.")
  (group (:header "Options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help.")
         (stropt :short-name "t" :long-name "type"
                 :description "Specify hash function to use.")))

(defun multi-digest (type &rest files)
  "Compute the TYPE checksums of FILES."
  (loop :with buffer = (make-array 8192 :element-type '(unsigned-byte 8))
        :with digest = (make-array (digest-length type)
                                   :element-type '(unsigned-byte 8))
        :for file :in files
        :for digester = (make-digest type)
        :do (digest-file digester file :buffer buffer :digest digest)
            (format t "~A ~A~%" (byte-array-to-hex-string digest) (file-namestring file))))

(defun single-digest (type file)
  "Compute the TYPE checksum of FILE."
  (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8)))
        (digest (make-array (digest-length type)
                            :element-type '(unsigned-byte 8)))
        (digester (make-digest type)))
    (digest-file digester file :buffer buffer :digest digest)
    (format nil "~A ~A" (byte-array-to-hex-string digest) (file-namestring file))))

(defun hash (type string)
  "Compute the TYPE checksum of STRING."
  (byte-array-to-hex-string (digest-sequence type (ascii-string-to-byte-array string))))

(defun mksum-dir (type directory)
  "Compute the TYPE checksum of the concatenated checksums of the files inside DIRECTORY."
  (let* ((list-of-string (mapcar #'first
                                 (mapcar #'(lambda (string) (cl-ppcre:split #\space string))
                                         (mapcar #'(lambda (file) (single-digest type file))
                                                 (mof:files directory)))))
         (long-string (reduce #'(lambda (str1 str2) (concatenate 'string str1 str2))
                              list-of-string)))
    (format nil "~A ~A" (hash type long-string) (merge-pathnames* directory
                                                                  (user-homedir-pathname)))))

(exporting-definitions
  (defun mksum (&rest args)
    "Compute the checksum of the given file(s) and directory(ies)."
    (declare (ignorable args))
    (labels ((check-context-p (type)
               (string= (getopt :short-name "t" :context (make-context)) type))
             (check-file5-p (arg)
               (and (file-exists-p arg)
                    (check-context-p "md5")))
             (check-file256-p (arg)
               (and (file-exists-p arg)
                    (check-context-p "sha256")))
             (check-file1-p (arg)
               (and (file-exists-p arg)
                    (check-context-p "sha1")))
             (check-dir5-p (arg)
               (and (directory-exists-p arg)
                    (check-context-p "md5")))
             (check-dir256-p (arg)
               (and (directory-exists-p arg)
                    (check-context-p "sha256")))
             (check-dir1-p (arg)
               (and (directory-exists-p arg)
                    (check-context-p "sha1")))
             (m-s (arg)
               (cond ((null arg) nil)
                     ((check-file5-p (first arg)) (cons (single-digest 'md5 (first arg))
                                                        (m-s (rest arg))))
                     ((check-file256-p (first arg)) (cons (single-digest 'sha256 (first arg))
                                                          (m-s (rest arg))))
                     ((check-file1-p (first arg)) (cons (single-digest 'sha1 (first arg))
                                                       (m-s (rest arg))))
                     ((check-dir5-p (first arg)) (cons (mksum-dir 'md5 (first arg))
                                                       (m-s (rest arg))))
                     ((check-dir256-p (first arg)) (cons (mksum-dir 'sha256 (first arg))
                                                         (m-s (rest arg))))
                     ((check-dir1-p (first arg)) (cons (mksum-dir 'sha1 (first arg))
                                                       (m-s (rest arg))))
                     ((file-exists-p (first arg)) (cons (single-digest 'sha256 (first arg))
                                                        (m-s (rest arg))))
                     ((directory-exists-p (first arg)) (cons (mksum-dir 'sha256 (first arg))
                                                             (m-s (rest arg))))
                     (t (help)
                        (exit)))))
      (cond ((getopt :short-name "h"
                     :context (make-context)) (help) (exit))
            (t (m-s (remainder))))))

  #|(defun mksum (&rest args)
    "Compute the checksum of the given file(s)."
    (declare (ignorable args))
    (labels ((check-context-p (type)
               (string= (getopt :short-name "t" :context (make-context)) type))
             (digest-to (type)
               (reduce/strcat (remainder) :key #'(lambda (file) (multi-digest type file)))
               (exit)))
      (cond ((getopt :short-name "h"
                     :context (make-context)) (help)
             (exit))
            ((check-context-p "md5") (digest-to 'md5))
            ((check-context-p "sha256") (digest-to 'sha256))
            ((check-context-p "sha1") (digest-to 'sha1))
            ((remainder) (digest-to 'sha256))
            (t (help)
               (exit)))))|#)

(register-commands :scripts/mksum)

