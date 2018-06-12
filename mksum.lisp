;;;; mksum.lisp

(uiop:define-package #:scripts/mksum
    (:use #:cl
          #:cl-scripting
          #:fare-utils
          #:net.didierverna.clon)
  (:export #:mksum))

(in-package :scripts/mksum)

(defvar *default-hash* :sha256 "Default hash function")

(defsynopsis (:postfix "(FILE...|STRING)")
  (text :contents "Prints the checksums of files and directories. Uses SHA256 by default.
")
  (group (:header "Options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help.")
         (flag :short-name "l" :long-name "list"
               :description "List supported hash functions.")
         (flag :short-name "s" :long-name "string"
               :description "Treat arguments as literal strings.")
         (flag :short-name "v" :long-name "verbose"
               :description "Print files of files when handling directories.")
         (stropt :short-name "t" :long-name "type" :argument-name "HASH"
                 :description "Specify hash function to use.")))

(defun slash-string (directory)
  ""
  (let* ((length (length directory))
         (last (elt directory (1- length))))
    (if (char= last #\/)
        directory
        (concatenate 'string directory "/"))))

(defun format-two (arg-1 arg-2)
  "Print the two arguments in aesthetic form."
  (format nil "~A ~A" arg-1 arg-2))

(defun print-list (list)
  "Output formatted string from LIST."
  (format t "~{~A~%~}" list))

(defun print-help ()
  "Print help text."
  (help)
  (exit))

(defun print-digests ()
  "Print list of supported digests."
  (print-list (ironclad:list-all-digests)))

(defun print-exit (list)
  "Prints LIST then exit."
  (print-list list)
  (exit))

(defun file-fast-print (arg)
  "Print file ARG immediately."
  (format *query-io* "~a" arg)
  (force-output *query-io*)
  (terpri))

(defun dir-fast-print (arg)
  "Print contents of ARG immediately."
  (let ((file (mof:files (uiop:truenamize arg))))
    (labels ((dir (file)
               (cond (file (file-fast-print (first file))
                           (dir (rest file)))
                     ((or (null file) t) (terpri)))))
      (dir file))))

(defun concat (&rest args)
  "Concatenate strings."
  (reduce #'(lambda (x y) (concatenate 'string x y)) args))

(defun print-preserve (ffunction)
  "Invoke FFUNCTION on (read)."
  (setf (readtable-case *readtable*) :preserve)
  (print-list (apply ffunction (list (list (symbol-name (read))))))
  (exit))

(defun file-really-exists-p (arg)
  "Check if file really exists."
  (and (uiop:file-exists-p arg)
       (uiop:probe-file* (uiop:truenamize arg))))

(defun get-opt (option)
  "Get the value of OPTION from the context."
  (getopt :short-name option :context (make-context)))

(defun context-p (option)
  "Check membership of option value in supported digests."
  (member (intern (string-upcase (get-opt option)) "IRONCLAD")
          (ironclad:list-all-digests)))

(defun first-context ()
  "Get first element of (CONTEXT-P)."
  (first (context-p "t")))

(defun weird-p (arg)
  "Check if last arg is a valid digest."
  (member (intern (string-upcase (second (or (member "-t" arg :test #'equal)
                                             (member "--type" arg :test #'equal)))) "IRONCLAD")
          (ironclad:list-all-digests)))

(defun first-weird (arg)
  "Get first element of (WEIRD-P)."
  (first (weird-p arg)))

(defun weird-with (arg type)
  "Create list, of the given type, of checksums of files and directories."
  (cond ((null arg) nil)
        (t (cons (format-two (hash type (first arg)) (first arg))
                 (weird-with (rest arg) type)))))

(defun file-context-p (arg)
  "Check if file really exists and option value is valid."
  (and (context-p "t") (file-really-exists-p arg)))

(defun directory-context-p (arg)
  "Check if directory exists and option value is valid."
  (and (context-p "t") (uiop:directory-exists-p arg)))

(defun file-checksum (type file)
  "Compute the TYPE checksum of FILE."
  (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8)))
        (digest (make-array (ironclad:digest-length type) :element-type '(unsigned-byte 8)))
        (digester (ironclad:make-digest type)))
    (ironclad:digest-file digester file :buffer buffer :digest digest)
    (format-two (ironclad:byte-array-to-hex-string digest) (uiop:truenamize file))))

(defun hash (type string)
  "Compute the TYPE checksum of STRING."
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence type
                                                               (ironclad:ascii-string-to-byte-array
                                                                string))))

(defun collect-files (directory)
  "Collect valid existing files under DIRECTORY."
  (loop :for file
        :in (mof:files directory)
        :when (file-really-exists-p file)
        :collect file))

(defun list-dir-checksum (type directory)
  "List the TYPE checksums of the files inside DIRECTORY."
  (mapcar #'first
          (mapcar #'(lambda (string) (cl-ppcre:split #\space string))
                  (mapcar #'(lambda (file) (file-checksum type file))
                          (collect-files directory)))))

(defun directory-checksum (type directory)
  "Compute the TYPE checksum of the concatenated checksums of the files inside DIRECTORY."
  (when (uiop:directory-exists-p directory)
    (let* ((path (uiop:truenamize directory))
           (value (reduce #'(lambda (string-1 string-2) (concat string-1 string-2))
                          (list-dir-checksum type path))))
      (format-two (hash type value) path))))

(defun append-contents (arg type function)
  "Append directory TYPE checksum to its file contents."
  (append (list "")
          (mof:files (uiop:truenamize (first arg)))
          (cons (directory-checksum type (first arg)) (funcall function (rest arg) t))))

(defun option-without (arg verbose-p)
  "Create list of SHA256 checksums of files and directories."
  (cond ((null arg) nil)
        ((and (file-really-exists-p (first arg)) verbose-p) (file-fast-print (first arg))
         (cons (file-checksum *default-hash* (first arg)) (option-without (rest arg) verbose-p)))
        ((file-really-exists-p (first arg)) (cons (file-checksum *default-hash* (first arg))
                                                  (option-without (rest arg) verbose-p)))
        ((and (uiop:directory-exists-p (first arg)) verbose-p) (dir-fast-print (first arg))
         (append-contents arg *default-hash* #'option-without))
        ((uiop:directory-exists-p (first arg)) (cons (directory-checksum *default-hash* (first arg))
                                                     (option-without (rest arg) verbose-p)))
        (t nil)))

(defun option-with (arg verbose-p)
  "Create list, of the given type, of checksums of files and directories."
  (cond ((null arg) nil)
        ((and (file-context-p (first arg)) verbose-p) (file-fast-print (first arg))
         (cons (file-checksum (first-context) (first arg)) (option-with (rest arg) verbose-p)))
        ((and (directory-context-p (first arg)) verbose-p) (append-contents arg (first-context)
                                                                            #'option-with))
        ((file-context-p (first arg)) (cons (file-checksum (first-context) (first arg))
                                            (option-with (rest arg) verbose-p)))
        ((directory-context-p (first arg))
         (cons (directory-checksum (first-context) (first arg)) (option-with (rest arg) verbose-p)))
        (t nil)))

(defun string-without (arg)
  "Create list of SHA256 checksums of strings."
  (cond ((null arg) nil)
        (t (cons (format-two (hash *default-hash* (first arg)) (first arg))
                 (string-without (rest arg))))))

(defun string-with (arg)
  "Create list, of the given type, of checksums of files and directories."
  (cond ((null arg) nil)
        (t (cons (format-two (hash (first-context) (first arg)) (first arg))
                 (string-with (rest arg))))))

(exporting-definitions
  (defun mksum (&rest args)
    "The top-level function"
    (cond ((and (get-opt "s") (get-opt "t") (remainder)) (print-exit (string-with (remainder))))
          ((and (or (member "-s" args :test #'equal) (member "--string" args :test #'equal))
                (weird-p args) (>= (length args) 4))
           (print-exit (weird-with (rest args) (first-weird args))))
          ((and (get-opt "s") (get-opt "t")) (print-preserve #'string-with))
          ((and (get-opt "s") (remainder)) (print-exit (string-without (remainder))))
          ((and (get-opt "t") (get-opt "v") (remainder)) (print-exit (option-with (remainder) t)))
          ((and (get-opt "t") (remainder)) (print-exit (option-with (remainder) nil)))
          ((and (remainder) (get-opt "v")) (print-exit (option-without (remainder) t)))
          ((remainder) (print-exit (option-without (remainder) nil)))
          ((get-opt "s") (print-preserve #'string-without))
          ((get-opt "l") (print-exit (ironclad:list-all-digests)))
          ((or (get-opt "h") (null (remainder))) (print-help))
          ((and (get-opt "s") (get-opt "t")) (print-exit (string-with (remainder))))
          ((get-opt "s") (print-exit (string-without (remainder))))
          ((get-opt "t") (print-exit (option-with (remainder))))
          (t (print-exit (option-without (remainder)))))))

(register-commands :scripts/mksum)
