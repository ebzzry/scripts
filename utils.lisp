;;;; utils.lisp

(uiop:define-package #:scripts/utils
    (:use #:cl
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:fare-utils
          #:cl-ppcre
          #:cl-launch/dispatch)
  (:export #:char-display-char
           #:battery-status
           #:wine
           #:home
           #:err
           #:apply-args
           #:apply-args-1
           #:string-first
           #:psg-lines
           #:find-binary
           #:%))

(in-package #:scripts/utils)

(defun char-display-char (c)
  (if (or (member c '(127 155))
          (< c 32)
          (<= 128 c 159))
      #\space
      (code-char c)))

(defun battery-status ()
  (let ((base-dir "/sys/class/power_supply/*")
        (exclude-string "/AC/"))
    (with-output (s nil)
      (loop
        :for dir :in (remove-if #'(lambda (path)
                                    (search exclude-string (native-namestring path)))
                                (directory* base-dir))
        :for battery = (first (last (pathname-directory dir)))
        :for capacity = (read-file-line (subpathname dir "capacity"))
        :for status = (read-file-line (subpathname dir "status"))
        :do (format s "~A: ~A% (~A)~%" battery capacity status)))))

(defun wine (path &rest args)
  (run/i `(wine ,path ,@args)))

(defun home (path)
  (subpathname (user-homedir-pathname) path))

(defun err (message)
  (die 1 (format t "Error: ~A~%" message)))

(defun apply-args (function options args)
  (apply function (append (list options) args)))

(defun apply-args-1 (function args &key (options nil))
  (apply function (append options args)))

(defun string-first (string)
  (let* ((space (position #\  string :test #'equal)))
    (subseq string 0 space)))

(defun psg-lines (&rest args)
  (run/lines `(pgrep "--list-full" "--list-name" "--full" "--ignore-case" ,@args)))

(defun find-binary (binary)
  (run/ss `(readlink -f ,(run/ss `(which ,binary)))))

(defmacro % (name command)
  `(defun ,name (&rest args)
     (run/i (append (split "\\s+" ,command) args))
     (success)))
