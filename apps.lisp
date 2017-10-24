(uiop:define-package
    :scripts/apps
    (:use :cl
          :fare-utils
          :uiop
          :inferior-shell
          :cl-scripting
          :optima
          :optima.ppcre
          :cl-ppcre
          :local-time
          :cl-launch/dispatch
          :scripts/misc
          :scripts/utils)
    (:export #:xrsync
             #:ra
             #:raz
             #:raz!

             #:chrome
             #:chrome-stable
             #:chrome-beta
             #:chrome-unstable
             #:kill-chrome
             #:stop-chrome
             #:continue-chrome
             #:tele
             #:kill-tele

             #:suma
             #:kill-suma
             #:qpdf
             #:kill-qpdf

             #:term
             #:e
             #:cl!
             #:screenshot
             #:sg2e
             #:sg2eb))

(in-package :scripts/apps)

(defvar +screenshots-dir+ (home "hejmo/elsx/bil/ekrankopioj"))

(defparameter +funs+
  `((e0 . "emacsclient -nw")
    (t0 . "terminator")))

(defmacro def-fun (name command)
  `(defun ,name (&rest args)
     (run/i (append (cl-ppcre:split "\\s+" ,command) args))
     (success)))

;; (defun def-funs (funs)
;;   (loop :for (name . command) :in +funs+ :do
;;      (def-fun name command)))

(defmacro def-funs ()
  `(loop :for (name . command) :in +funs+ :do
      (def-fun name command)))

(exporting-definitions
  (def-funs)

  (def-fun e "emacsclient -nw")
  (def-fun term "terminator")
  (def-fun xrsync "rsync -rlptgoDHSx")
  (def-fun ra "xrsync")
  (def-fun raz "ra -z")

  ;; (defun ra (&rest args)
  ;;   (apply 'xrsync args)
  ;;   (success))

  ;; (defun raz (&rest args)
  ;;   (apply-args-1 'ra args :options '("-z"))
  ;;   (success))

  (defun raz! (&rest args)
    (apply-args-1 'raz args :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
    (success))

  (defun chrome (&rest args)
    (apply #'chrome-unstable args))

  (defun chrome-stable (&rest args)
    (run/i `(google-chrome-stable ,@args)))

  (defun chrome-beta (&rest args)
    (run/i `(google-chrome-beta ,@args)))

  (defun chrome-unstable (&rest args)
    (run/i `(google-chrome-unstable ,@args)))

  (defun kill-chrome (&rest args)
    (run `(killall ,@args chromium-browser chromium google-chrome chrome)
         :output :interactive :input :interactive :error-output nil :on-error nil)
    (success))

  (defun stop-chrome ()
    (kill-chrome "-STOP"))

  (defun continue-chrome ()
    (kill-chrome "-CONT"))

  (defun tele (&rest args)
    (run/i `(telegram-desktop ,@args)))

  (defun kill-tele (&rest args)
    (run `(killall ,@args telegram-desktop) :output :interactive :input :interactive :error-output nil :on-error nil)
    (success))

  (defun suma (&rest args)
    (run/nil `(wine ,(home ".wine/drive_c/Program Files/SumatraPDF/SumatraPDF.exe") ,@args) :on-error nil)
    (success))

  (defun kill-suma (&rest args)
    (run `(killall ,@args SumatraPDF.exe) :output :interactive :input :interactive :error-output nil :on-error nil)
    (success))

  (defun qpdf (&rest args)
    (run/i `(qpdfview ,@args)))

  (defun kill-qpdf (&rest args)
    (run `(killall ,@args qpdfview) :output :interactive :input :interactive :error-output nil :on-error nil)
    (success))

  (defun cl! (&rest args)
    (let* ((arguments (mapcar #'(lambda (s) (format nil "\'~A\'" s)) args))
           (list-arguments (append '("sbcl") arguments))
           (string-arguments (format nil "~{~a~^ ~}" list-arguments))
           (dir (pathname-directory-pathname (find-binary (argv0)))))
      (chdir dir)
      (run/i `(nix-shell --pure --command ,string-arguments))
      (success)))

  (defun screenshot (mode)
    (let* ((dir +screenshots-dir+)
           (file (format nil "~A.png" (local-time:format-timestring nil (now))))
           (dest (format nil "mv $f ~A" dir))
           (image (format nil "~A/~A" dir file)))
      (flet ((scrot (file dest &rest args)
               (run/i `(scrot ,@args ,file -e ,dest))))
        (match mode
          ((ppcre "(full|plena)") (scrot file dest))
          ((ppcre "(region|parta)") (scrot file dest '-s))
          (_ (err (format nil "invalid mode ~A~%" mode))))
        (run `(xclip -selection clipboard) :input (list image))
        (success)))))

(exporting-definitions
  (defun sg2e (&rest args)
    (declare (ignore args))
    (run/i `(stem "-X" ,(argv0) "steam://rungameid/245170"))
    (success))

  (defun sg2eb (&rest args)
    (declare (ignore args))
    (run/i `(stem "-X" ,(argv0) "steam://rungameid/208610"))
    (success)))

(register-commands :scripts/apps)
