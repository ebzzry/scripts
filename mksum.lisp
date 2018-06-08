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
  (text :contents "Print the file/directory checksums (for supported algorithms, provide the \"-l\"
option flag). With no options provided, prints the SHA-256 checksum by default.")
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
        (digest (make-array (digest-length type) :element-type '(unsigned-byte 8)))
        (digester (make-digest type)))
    (digest-file digester file :buffer buffer :digest digest)
    (format nil "~A ~A" (byte-array-to-hex-string digest) (file-namestring file))))

(defun hash (type string)
  "Compute the TYPE checksum of STRING."
  (byte-array-to-hex-string (digest-sequence type (ascii-string-to-byte-array string))))

(defun create-context (type directory)
  "Compute the TYPE checksums of the files inside DIRECTORY"
  (mapcar #'first
          (mapcar #'(lambda (string) (cl-ppcre:split #\space string))
                  (mapcar #'(lambda (file) (single-digest type file))
                          (mof:files directory)))))

(defun mksum-dir (type directory)
  "Compute the TYPE checksum of the concatenated checksums of the files inside DIRECTORY."
  (let* ((long-string (reduce #'(lambda (string-1 string-2) (concat string-1 string-2))
                              (create-context type directory))))
    (format nil "~A ~A" (hash type long-string) (merge-pathnames* directory
                                                                  (user-homedir-pathname)))))

(defun get-opt (option)
  "Get the value of OPTION from the context."
  (getopt :short-name option :context (make-context)))

(defun iron-hash ()
  "Output how ironclad shows its hash function through the command line."
  (read-from-string (concat "ironclad:" (get-opt "t"))))

(defun iron-d-hash ()
  "Output default mksum hash in ironclad format."
  (read-from-string (concat "ironclad:" "sha256")))

(defun pretty-splice (list)
  (format t "~{~A~%~}" list))

(defun concat (&rest args)
  (reduce #'(lambda (x y) (concatenate 'string x y)) args))

(exporting-definitions
  (defun mksum (&rest args)
    "Compute the checksum of the given file(s) and directory(ies)."
    (declare (ignorable args))
    (labels ((context-p ()
               (member (iron-hash)
                       (list-all-digests)))
             (option-with (arg)
               (cond ((null arg) nil)
                     ((and (context-p) (file-exists-p (first arg)))
                      (cons (single-digest (iron-hash)
                                           (first arg))
                            (option-with (rest arg))))
                     
                     ((and (context-p)
                           (directory-exists-p (first arg)))
                      (cons (mksum-dir (iron-hash)
                                       (first arg))
                            (option-with (rest arg))))))
             (option-without (arg)
               (cond ((null arg) nil)
                     ((file-exists-p (first arg)) (cons (single-digest (iron-d-hash)
                                                                       (first arg))
                                                        (option-without (rest arg))))
                     ((directory-exists-p (first arg)) (cons (mksum-dir (iron-d-hash)
                                                                        (first arg))
                                                             (option-without (rest arg)))))))
      (cond ((get-opt "h") (help) (exit))
            ((get-opt "l") (pretty-splice (list-all-digests))
             (exit))
            ((null (remainder)) (help) (exit))
            ((get-opt "t") (pretty-splice (option-with (remainder)))
             (exit))
            (t (pretty-splice (option-without (remainder)))
               (exit))))))

(register-commands :scripts/mksum)

