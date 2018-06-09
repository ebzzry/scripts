;;;; mksum.lisp

(uiop:define-package :scripts/mksum
    (:use #:cl
          #:cl-scripting
          #:fare-utils
          #:net.didierverna.clon)
  (:export #:mksum
           #:ms))

(in-package :scripts/mksum)

(defvar *default-hash* :sha256 "Default hash function")

(defsynopsis (:postfix "FILE...")
  (text :contents "Prints the checksums of files and directories. Uses SHA256 by default.")
  (group (:header "Options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help.")
         (flag :short-name "l" :long-name "list"
               :description "List supported hash functions.")
         (stropt :short-name "t" :long-name "type" :argument-name "HASH"
                 :description "Specify hash function to use.")))

(defun checksum (type file)
  "Compute the TYPE checksum of FILE."
  (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8)))
        (digest (make-array (ironclad:digest-length type) :element-type '(unsigned-byte 8)))
        (digester (ironclad:make-digest type)))
    (ironclad:digest-file digester file :buffer buffer :digest digest)
    (format nil "~A ~A" (ironclad:byte-array-to-hex-string digest) (file-namestring file))))

(defun hash (type string)
  "Compute the TYPE checksum of STRING."
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence type
                                                               (ironclad:ascii-string-to-byte-array
                                                                string))))

(defun slash-string (directory)
  "Convert directory to its truename."
  (let* ((length (length directory))
         (last (elt directory (1- length))))
    (if (char= last #\/)
        directory
        (concatenate 'string directory "/"))))

(defun list-dir-checksum (type directory)
  "List the TYPE checksums of the files inside DIRECTORY."
  (mapcar #'first
          (mapcar #'(lambda (string) (cl-ppcre:split #\space string))
                  (mapcar #'(lambda (file) (checksum type file))
                          (mof:files directory)))))

(defun concat (&rest args)
  "Concatenate strings."
  (reduce #'(lambda (x y) (concatenate 'string x y)) args))

(defun directory-checksum (type directory)
  "Compute the TYPE checksum of the concatenated checksums of the files inside DIRECTORY."
  (when (uiop:directory-exists-p directory)
    (let ((value (reduce #'(lambda (string-1 string-2) (concat string-1 string-2))
                         (list-dir-checksum type directory))))
      (format nil "~A ~A" (hash type value) (slash-string directory)))))

(defun get-opt (option)
  "Get the value of OPTION from the context."
  (getopt :short-name option :context (make-context)))

(defun print-list (list)
  "Output formatted string from LIST"
  (format t "~{~A~%~}" list))

(defun context-p ()
  "Check membership of option value in supported digests."
  (member (intern (string-upcase (get-opt "t")) "IRONCLAD")
          (ironclad:list-all-digests)))

(defun first-context ()
  "Get first element of (CONTEXT-P)"
  (first (context-p)))

(defun file-really-exists-p (arg)
  "Check if file really exists."
  (and (uiop:file-exists-p arg) (uiop:probe-file* arg)))

(defun file-context-p (arg)
  "Check if file really exists and option value is valid."
  (and (context-p) (file-really-exists-p arg)))

(defun directory-context-p (arg)
  "Check if directory exists and option value is valid."
  (and (context-p) (uiop:directory-exists-p arg)))

(defun option-with (arg)
  "Create list of the given type of checksums of files and directories"
  (cond ((null arg) nil)
        ((file-context-p (first arg))
         (cons (checksum (first-context) (first arg)) (option-with (rest arg))))
        ((directory-context-p (first arg))
         (cons (directory-checksum (first-context) (first arg)) (option-with (rest arg))))
        (t nil)))

(defun option-without (arg)
  "Create list of SHA256 checksums of files and directories"
  (cond ((null arg) nil)
        ((file-really-exists-p (first arg))
         (cons (checksum *default-hash* (first arg)) (option-without (rest arg))))
        ((uiop:directory-exists-p (first arg)) (cons (directory-checksum *default-hash*
                                                                         (first arg))
                                                     (option-without (rest arg))))
        (t nil)))

(defun aapsdoify (&rest args)
  )

(exporting-definitions
  (defun mksum (&rest args)
    "Compute the checksum of the given file(s) and directory(ies)."
    (declare (ignorable args))
    (cond ((get-opt "h") (help) (exit))
          ((get-opt "l") (print-list (ironclad:list-all-digests))
           (exit))
          ((null (remainder)) (help) (exit))
          ((get-opt "t") (print-list (option-with (remainder)))
           (exit))
          (t (print-list (option-without (remainder)))
             (exit)))))

(defun ms (&rest args)
  (apply #'mksum args))

(register-commands :scripts/mksum)
