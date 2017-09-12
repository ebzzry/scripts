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

             #:chrome-stable
             #:chrome-beta
             #:chrome-unstable
             #:kill-chrome
             #:stop-chrome
             #:continue-chrome
             #:tele
             #:kill-tele
             #:suma
             #:term
             #:cl!
             #:screenshot
             #:sg2e
             #:sg2eb))

(in-package :scripts/apps)

(defvar *screenshots-dir* (home "hejmo/elsx/bil/ekrankopioj"))

(exporting-definitions
 (defun xrsync (&rest args)
   (run/i `(rsync "-rlptgoDHSx" ,@args))
   (success))

 (defun ra (&rest args)
   (apply 'xrsync args)
   (success))

 (defun raz (&rest args)
   (apply-args-1 'ra args :options '("-z"))
   (success))

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
   (run `(killall ,@args telegram-desktop)
        :output :interactive :input :interactive :error-output nil :on-error nil)
   (success))

 (defun suma (&rest args)
   (run/nil `(wine ,(home ".wine/drive_c/Program Files/SumatraPDF/SumatraPDF.exe") ,@args) :on-error nil)
   (success))

 (defun term (&rest args)
   (run/nil `(terminator ,@args) :on-error nil)
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
   (let* ((dir *screenshots-dir*)
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
       (success))))

 (defun sg2e (&rest args)
   (declare (ignore args))
   (run/i `(stem "-X" ,(argv0) "steam://rungameid/245170"))
   (success))

 (defun sg2eb (&rest args)
   (declare (ignore args))
   (run/i `(stem "-X" ,(argv0) "steam://rungameid/208610"))
   (success)))

(register-commands :scripts/apps)
