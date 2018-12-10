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
           #:run-with-wine
           #:with-qt
           #:%
           #:@
           #:$))

(in-package #:scripts/utils)

(defun char-display-char (c)
  "Display character C to stdout."
  (if (or (member c '(127 155))
          (< c 32)
          (<= 128 c 159))
      #\space
      (code-char c)))

(defun battery-status ()
  "Display battery status."
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
  "Run PATH to wine."
  (run/i `(wine ,path ,@args)))

(defun err (message)
  "Exit with MESSAGE."
  (die 1 (format t "Error: ~A~%" message)))

(defun apply-args (function options args)
  "Apply FUNCTION to ARGS."
  (apply function (append (list options) args)))

(defun apply-args-1 (function args &key (options nil))
  "Apply FUNCTION to ARGS."
  (apply function (append options args)))

(defun string-first (string)
  "Return the first string from STRING."
  (let* ((space (position #\  string :test #'equal)))
    (subseq string 0 space)))

(defun find-binary (binary)
  "Find BINARY in PATH."
  (run/ss `(readlink -f ,(run/ss `(which ,binary)))))

(defun run-with-locale (locale &rest args)
  "Run args with locale set to LOCALE."
  (setf (getenv "LANG") locale)
  (run/i `(,@(first args)))
  (success))

(defun run-with-nix-system (binary &rest args)
  "Run binary without user paths."
  (setf (getenv "PATH") "/var/setuid-wrappers:/run/wrappers/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin")
  (run/i `(,binary ,@args))
  (success))

(defun run-with-xdg (binary &rest args)
  "Run binary under a custom XDG_DATA_DIRS path."
  (setf (getenv "XDG_DATA_DIRS")
        (uiop:native-namestring (mof:home ".local/share/mime")))
  (run/i `(,binary ,@args))
  (success))

(defun run-with-wine (location)
  "Run LOCATION using Wine."
  (setf (getenv "WINEDEBUG") "-all")
  (run/i `("wine" ,location))
  (success))

(defun run-with-wine-program-files (path)
  "Run PATH under Program Files using Wine."
  (run-with-wine (mof:home (mof:fmt ".wine/drive_c/Program Files/~A" path))))

(defmacro % (name command)
  "Define a normal command runner."
  `(defun ,name (&rest args)
     (run/i (append (split "\\s+" ,command) args))
     (success)))

(defmacro @ (name location)
  "Define a wine runner."
  `(defun ,name (&rest args)
     (run-with-wine-program-files ,location)))

(defun run-with-nix-user (profile binary args)
  "Run binary under a separate profile."
  (let ((bin (mof:home (mof:fmt ".baf/profiles/~A/bin" profile))))
    (setf (getenv "PATH") (unix-namestring bin))
    (run/i `(,binary ,@args))
    (success)))

(defun with-qt (command args)
  "Run a program in the QT profile."
  (setf (getenv "QT_QPA_PLATFORMTHEME") "qt5ct")
  (run-with-nix-user "qt" command args))

(defmacro $ (command name &optional alias)
  "Define a runner in the QT profile."
  `(progn
     (defun ,name (&rest args)
       (with-qt ,command args))
     ,(when alias
        `(defun ,alias (&rest args)
           (with-qt ,command args)))))
