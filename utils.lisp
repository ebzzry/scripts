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
           #:err
           #:apply-args
           #:apply-args-1
           #:string-first
           #:find-binary

           #:run-with-locale
           #:run-with-nix-system
           #:run-with-nix-user
           #:run-with-xdg
           #:with-qt
           #:%
           #:$))

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

(defun err (message)
  (die 1 (format t "Error: ~A~%" message)))

(defun apply-args (function options args)
  (apply function (append (list options) args)))

(defun apply-args-1 (function args &key (options nil))
  (apply function (append options args)))

(defun string-first (string)
  (let* ((space (position #\  string :test #'equal)))
    (subseq string 0 space)))

(defun find-binary (binary)
  (run/ss `(readlink -f ,(run/ss `(which ,binary)))))

(defun run-with-locale (locale &rest args)
  "Run args with locale set to LOCALE"
  (setf (getenv "LANG") locale)
  (run/i `(,@(first args)))
  (success))

(defun run-with-nix-system (binary &rest args)
  "Run binary without user paths"
  (setf (getenv "PATH") "/var/setuid-wrappers:/run/wrappers/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin")
  (run/i `(,binary ,@args))
  (success))

(defun run-with-nix-user (profile binary &rest args)
  "Run binary under a separate profile"
  (let ((bin (mof:home (mof:fmt ".baf/profiles/~A/bin" profile))))
    (setf (getenv "PATH") (unix-namestring bin))
    (run/i `(,binary ,@args))
    (success)))

(defun run-with-xdg (binary &rest args)
  "Run binary under a custom XDG_DATA_DIRS path"
  (setf (getenv "XDG_DATA_DIRS")
        (uiop:native-namestring (mof:home ".local/share/mime")))
  (run/i `(,binary ,@args))
  (success))

(defmacro % (name command)
  `(defun ,name (&rest args)
     (run/i (append (split "\\s+" ,command) args))
     (success)))

(defun with-qt (name args)
  (setf (getenv "QT_QPA_PLATFORMTHEME") "qt5ct")
  (run-with-nix-user "qt" name args))

(defmacro $ (name command)
  `(defun ,name (&rest args)
     (with-qt ,command args)))
