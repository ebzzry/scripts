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
           #:xdg-dir
           #:run-with-wine
           #:run-with-libgl-always-software
           #:%
           #:@
           #:@+
           #:$
           #:$$
           #:$%
           #:run-with-docker-x
           #:build-command))

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
      (loop :for dir :in (remove-if #'(lambda (path)
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

(defun run-with-locale-en (args)
  "Run args with locale set to "
  (run-with-locale "en_US.UTF-8" args))

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

(defun run-with-libgl-always-software (binary &rest args)
  "Run BINARY using some LIBGL flags"
  (setf (getenv "LIBGL_ALWAYS_SOFTWARE") "1")
  (run/i `(,binary ,@args))
  (success))

(defmacro % (name command)
  "Define a normal command runner."
  `(defun ,name (&rest args)
     (run (append (split "\\s+" ,command) args)
          :output :interactive :input :interactive
          :error-output t :on-error nil)
     (success)))

(defmacro @ (name command)
  "Define a command with wine."
  `(defun ,name (&rest args)
     (run-with-wine ,command)))

(defmacro @+ (name location)
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
  (setf (getenv "QT_QPA_PLATFORMTHEME") "gtk2")
  (setf (getenv "QT_STYLE_OVERRIDE") "gtk2")
  (run-with-nix-user "qt" command args))

(defmacro $ (name command)
  "Define a runner with the QT_QPA environment set to GTK2."
  `(defun ,name (&rest args)
     (setf (getenv "QT_QPA_PLATFORMTHEME") "gtk2")
     (run (append (split "\\s+" ,command) args)
          :output :interactive :input :interactive
          :error-output t :on-error nil)
     (success)))

(defmacro $$ (name command)
  "Define a runner with the QT_QPA environment."
  `(defun ,name (&rest args)
     (setf (getenv "QT_QPA_PLATFORMTHEME") "qt5ct")
     (run (append (split "\\s+" ,command) args)
          :output :interactive :input :interactive
          :error-output t :on-error nil)
     (success)))

(defmacro $% (name command)
  "Define a runner without the QT_QPA environment."
  `(defun ,name (&rest args)
     (setf (getenv "QT_QPA_PLATFORMTHEME") "")
     (setf (getenv "QT_STYLE_OVERRIDE") "")
     (run (append (split "\\s+" ,command) args)
          :output :interactive :input :interactive
          :error-output t :on-error nil)
     (success)))

(defun xdg-dir (spec)
  "Return the appropriate XDG directory specified by SPEC."
  (mof:trim-whitespace (inferior-shell:run/s `("xdg-user-dir" ,spec))))

(defun paths (x y)
  "Merge path namestrings."
  (mof:cat (uiop:native-namestring (mof:expand-pathname x))
           ":"
           y))

(defun run-with-docker-x (name &rest args)
  "Run command with Docker."
  (let* ((id (mof:trim-whitespace
              (inferior-shell:run/s
               `("docker" "inspect" "--format={{ .Config.Hostname }}" ,name))))
         (permissions (mof:cat "local:" id)))
    (run/i `("xhost" ,(mof:cat "+" permissions)))
    (run/i `("docker" "run" "--rm" "-e" "DISPLAY"
                      "--name" ,name
                      "--device=/dev/dri:/dev/dri"
                      "-v" "/tmp/.X11-unix:/tmp/.X11-unix"
                      "-v" ,(paths "~/.ViberPC/" "/root/.ViberPC/")
                      "-v" ,(paths (xdg-dir "DESKTOP") "/root/Desktop/")
                      "-v" ,(paths (xdg-dir "DOWNLOAD") "/root/Downloads/")
                      ,name ,@args))
    (run/i `("xhost" ,(mof:cat "-" permissions))))
  (success))

(defun build-command (command args)
  "Return a string suitable for RUN/I."
  (if args
      (mof:cat command " " (reduce #'(lambda (x y) (concatenate 'string x " " y)) args))
      command))
