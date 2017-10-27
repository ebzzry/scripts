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
  (:export #:s

           #:e
           #:term
           #:term1
           #:xrsync
           #:ra
           #:raz
           #:raz!

           #:chrome
           #:tele
           #:qpdf
           #:@

           #:v
           #:xv
           #:rm+

           #:kill-chrome
           #:stop-chrome
           #:continue-chrome
           #:kill-tele
           #:lisp!
           #:screenshot

           #:sg2e
           #:sg2eb))

(in-package :scripts/apps)

(defvar +screenshots-dir+ (home "hejmo/elsx/bil/ekrankopioj"))

(defmacro % (name command)
  `(defun ,name (&rest args)
     (run/i (append (split "\\s+" ,command) args))
     (success)))

(exporting-definitions
 (% s "sudo")
 (% e "emacsclient -nw")
 (% term "terminator")
 (% xrsync "rsync -rlptgoDHSx")
 (% ra "xrsync")
 (% raz "ra -z")
 (% raz! "-e `ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null'")
 (% chrome "google-chrome-unstable")
 (% stop-chrome "kill-chrome -STOP")
 (% continue-chrome "kill-chrome -CONT")
 (% tele "telegram-desktop")
 (% qpdf "qpdfview")
 (% @ "len")

 (% rm+ "shred -vfzun 10")

 (% v "less")
 (% xv "xzless"))

(exporting-definitions
 (defun kill-chrome (&rest args)
   (run `(killall ,@args chromium-browser chromium google-chrome chrome)
        :output :interactive :input :interactive :error-output nil :on-error nil)
   (success))

 (defun kill-tele (&rest args)
   (run `(killall ,@args telegram-desktop) :output :interactive :input :interactive :error-output nil :on-error nil)
   (success))

 (defun lisp! (&rest args)
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
