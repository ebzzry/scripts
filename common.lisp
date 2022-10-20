;;;; common.lisp

(uiop:define-package #:scripts/common
  (:use #:cl
        #:inferior-shell
        #:cl-scripting
        #:marie))

(in-package #:scripts/common)

(def (char-display-char) (c)
  "Display character C to stdout."
  (if (or (member c '(127 155))
          (< c 32)
          (<= 128 c 159))
      #\space
      (code-char c)))

(def battery-status ()
  "Display battery status."
  (let ((base-dir "/sys/class/power_supply/*")
        (exclude-string "/AC/"))
    (uiop:with-output (s nil)
      (loop :for dir :in (remove-if (λ (path)
                                      (search exclude-string (uiop:native-namestring path)))
                                    (uiop:directory* base-dir))
            :for battery = (first (last (pathname-directory dir)))
            :for capacity = (uiop:read-file-line (uiop:subpathname dir "capacity"))
            :for status = (uiop:read-file-line (uiop:subpathname dir "status"))
            :do (format s "~A: ~A% (~A)~%" battery capacity status)))))

(def wine (path &rest args)
  "Run PATH with wine."
  (run/i `(wine ,path ,@args)))

(def err (message)
  "Exit with MESSAGE."
  (die 1 (format t "Error: ~A~%" message)))

(def apply-args (function options args)
  "Apply FUNCTION to ARGS."
  (apply function (append (list options) args)))

(def apply-args-1 (function args &key (options nil))
  "Apply FUNCTION to ARGS."
  (apply function (append options args)))

(def string-first (string)
  "Return the first string from STRING."
  (let* ((space (position #\  string :test #'equal)))
    (subseq string 0 space)))

(def run-with-locale (locale &rest args)
  "Run args with locale set to LOCALE."
  (setf (uiop:getenv "LANG") locale)
  (run/i `(,@(first args)))
  (success))

(def run-with-locale-en (args)
  "Run args with locale set to "
  (run-with-locale "en_US.UTF-8" args))

(def run-with-nix-system (binary &rest args)
  "Run binary without user paths."
  (setf (uiop:getenv "PATH") "/var/setuid-wrappers:/run/wrappers/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin")
  (run/i `(,binary ,@args))
  (success))

(def run-with-xdg (binary &rest args)
  "Run binary under a custom XDG_DATA_DIRS path."
  (setf (uiop:getenv "XDG_DATA_DIRS")
        (uiop:native-namestring (home ".local/share/mime")))
  (run/i `(,binary ,@args))
  (success))

(def run-with-wine (location)
  "Run LOCATION using Wine."
  (setf (uiop:getenv "WINEDEBUG") "-all")
  (run/i `("wine" ,location))
  (success))

(def run-with-wine-program-files (path)
  "Run PATH under Program Files using Wine."
  (run-with-wine (home (fmt ".wine/drive_c/Program Files/~A" path))))

(def run-with-libgl-always-software (binary &rest args)
  "Run BINARY using some LIBGL flags"
  (setf (uiop:getenv "LIBGL_ALWAYS_SOFTWARE") "1")
  (run/i `(,binary ,@args))
  (success))

(defm defcommand (name args &rest body)
  "Define a function with SIGINT handler."
  `(progn
     (defun ,name ,args
       (handler-case (progn ,@body)
         (#+sbcl sb-sys:interactive-interrupt
          #+ccl ccl:interrupt-signal-condition
          #+clisp system::simple-interrupt-condition
          #+ecl ext:interactive-interrupt
          #+allegro excl:interrupt-signal
          #+lispworks mp:process-interrupt
          () nil)
         (error (c)
           (format t "Oops, an unknown error occured:~&~A~&" c)))
       (cl-scripting:success))
     (export ',name)))

(def run-with-nix-user (profile binary args)
  "Run binary under a separate profile."
  (let ((bin (home (fmt ".baf/profiles/~A/bin" profile))))
    (setf (uiop:getenv "PATH") (unix-namestring bin))
    (run/i `(,binary ,@args))
    (success)))

(def with-qt (command args)
  "Run a program in the QT profile."
  (setf (uiop:getenv "QT_QPA_PLATFORMTHEME") "gtk2")
  (setf (uiop:getenv "QT_STYLE_OVERRIDE") "gtk2")
  (run-with-nix-user "qt" command args))

(def xdg-dir (spec)
  "Return the appropriate XDG directory specified by SPEC."
  (string-trim '(#\space #\newline) (inferior-shell:run/s `("xdg-user-dir" ,spec))))

(def run-with-docker-x (docker-args name &rest program-args)
  "Run command with Docker."
  (let* ((id (string-trim '(#\space #\newline)
                          (inferior-shell:run/s
                           `("docker" "inspect" "--format={{ .Config.Hostname }}" ,name))))
         (permissions (cat "local:" id)))
    (run/i `("xhost" ,(cat "+" permissions)))
    (run/i `("docker" "run" "--rm" "-e" "DISPLAY" "--name" ,name
                      "-v" "/tmp/.X11-unix:/tmp/.X11-unix" "--device=/dev/dri:/dev/dri"
                      "--memory" "1024m" "--cpus" ".5"
                      ,@docker-args ,name ,@program-args))
    (run/i `("xhost" ,(cat "-" permissions))))
  (success))

(defm % (&rest args)
  "Define a normal command runner."
  `(progn
     ,@(loop :for arg :in (partition args 2)
             :collect (destructuring-bind (name command)
                          arg
                        `(defcommand ,name (&rest args)
                           (run (append (cl-ppcre:split "\\s+" ,command) args)
                                :output :interactive :input :interactive
                                :error-output t :on-error nil)
                           (success))))))

(defm $ (&rest args)
  "Define a runner with the QT_QPA environment set to GTK2."
  `(progn
     ,@(loop :for arg :in (partition args 2)
             :collect (destructuring-bind (name command)
                          arg
                        `(defcommand ,name (&rest args)
                           (setf (uiop:getenv "QT_QPA_PLATFORMTHEME") "gtk2")
                           (run (append (cl-ppcre:split "\\s+" ,command) args)
                                :output :interactive :input :interactive
                                :error-output t :on-error nil)
                           (success))))))

(defm $$ (&rest args)
  "Define a runner with the QT_QPA environment."
  `(progn
     ,@(loop :for arg :in (partition args 2)
             :collect (destructuring-bind (name command)
                          arg
                        `(defcommand ,name (&rest args)
                           (setf (uiop:getenv "QT_QPA_PLATFORMTHEME") "qt5ct")
                           (run (append (cl-ppcre:split "\\s+" ,command) args)
                                :output :interactive :input :interactive
                                :error-output t :on-error nil)
                           (success))))))

(defm $% (&rest args)
  "Define a runner without the QT_QPA environment."
  `(progn
     ,@(loop :for arg :in (partition args 2)
             :collect (destructuring-bind (name command)
                          arg
                        `(defcommand ,name (&rest args)
                           (setf (uiop:getenv "QT_QPA_PLATFORMTHEME") "")
                           (setf (uiop:getenv "QT_STYLE_OVERRIDE") "")
                           (run (append (cl-ppcre:split "\\s+" ,command) args)
                                :output :interactive :input :interactive
                                :error-output t :on-error nil)
                           (success))))))

(defm @ (&rest args)
  "Define a command with wine."
  `(progn
     ,@(loop :for arg :in (partition args 2)
             :collect (destructuring-bind (name command)
                          arg
                        `(defcommand ,name (&rest args)
                           (run-with-wine ,command))))))

(defm @+ (&rest args)
  "Define a wine runner."
  `(progn
     ,@(loop :for arg :in (partition args 2)
             :collect (destructuring-bind (name command)
                          arg
                        `(defcommand ,name (&rest args)
                           (run-with-wine-program-files ,command))))))
