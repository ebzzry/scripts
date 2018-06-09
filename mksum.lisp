;;;; mksum.lisp

(uiop:define-package :scripts/mksum
    (:use #:cl
          #:uiop
          #:cl-scripting
          #:fare-utils
          #:net.didierverna.clon)
  (:export #:mksum))

(in-package :scripts/mksum)

(defsynopsis (:postfix "FILE...")
  (text :contents "Prints the checksums of files and directories. Uses SHA-256 by default.")
  (group (:header "Options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help.")
         (flag :short-name "l" :long-name "list"
               :description "List supported hash functions.")
         (stropt :short-name "t" :long-name "type"
                 :description "Specify hash function to use.")))

(defun single-digest (type file)
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

(defun create-context (type directory)
  "Compute the TYPE checksums of the files inside DIRECTORY"
  (mapcar #'first
          (mapcar #'(lambda (string) (cl-ppcre:split #\space string))
                  (mapcar #'(lambda (file) (single-digest type file))
                          (mof:files directory)))))

(defun concat (&rest args)
  "Concatenate strings."
  (reduce #'(lambda (x y) (concatenate 'string x y)) args))

(defun mksum-dir (type directory)
  "Compute the TYPE checksum of the concatenated checksums of the files inside DIRECTORY."
  (let* ((value (reduce #'(lambda (string-1 string-2) (concat string-1 string-2))
                        (create-context type directory))))
    (format nil "~A ~A" (hash type value) (merge-pathnames* directory
                                                            (user-homedir-pathname)))))

(defun get-opt (option)
  "Get the value of OPTION from the context."
  (getopt :short-name option :context (make-context)))

(defun print-list (list)
  "Output formatted string from LIST"
  (format t "~{~A~%~}" list))

(exporting-definitions
  (defun mksum (&rest args)
    "Compute the checksum of the given file(s) and directory(ies)."
    (declare (ignorable args))
    (labels ((context-p ()
               (member (intern (string-upcase (get-opt "t")) "IRONCLAD")
                       (ironclad:list-all-digests)))
             (first-context ()
               (first (context-p)))
             (for-sha ()
               (intern "SHA256" "IRONCLAD"))
             (option-with (arg)
               (cond ((null arg) nil)
                     ((and (context-p) (file-exists-p (first arg)))
                      (cons (single-digest (first-context) (first arg)) (option-with (rest arg))))
                     ((and (context-p) (directory-exists-p (first arg)))
                      (cons (mksum-dir (first-context) (first arg)) (option-with (rest arg))))))
             (option-without (arg)
               (cond ((null arg) nil)
                     ((file-exists-p (first arg)) (cons (single-digest (for-sha) (first arg))
                                                        (option-without (rest arg))))
                     ((directory-exists-p (first arg)) (cons (mksum-dir (for-sha) (first arg))
                                                             (option-without (rest arg)))))))
      (cond ((get-opt "h") (help) (exit))
            ((get-opt "l") (print-list (ironclad:list-all-digests))
             (exit))
            ((null (remainder)) (help) (exit))
            ((get-opt "t") (print-list (option-with (remainder)))
             (exit))
            (t (print-list (option-without (remainder)))
               (exit))))))

(register-commands :scripts/mksum)

