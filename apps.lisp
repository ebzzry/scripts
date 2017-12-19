(uiop:define-package
    :scripts/apps
    (:use :cl
          :fare-utils
          :uiop
          :inferior-shell
          :cl-scripting
          :optima
          :optima.ppcre
          :cl-launch/dispatch
          :scripts/misc
          :scripts/utils)
  (:export #:s

           #:e
           #:term
           #:xrsync
           #:ra
           #:raz
           #:raz!

           #:chrome
           #:qpdf
           #:rt
           #:rm@
           #:par
           #:v
           #:xv
           #:bt!

           #:tele
           #:tox
           #:vbox

           #:kill-chrome
           #:stop-chrome
           #:continue-chrome
           #:kill-tele
           #:lisp!
           #:screenshot

           #:len
           #:@
           #:leo

           #:sg2e))

(in-package :scripts/apps)

(defvar +screenshots-dir+ (home "hejmo/elsx/bil/ekrankopioj"))

(exporting-definitions
 (% s "sudo")
 (% e "emacsclient -nw")
 (% term "len urxvt")
 (% xrsync "rsync -rlptgoDHSx")
 (% ra "xrsync")
 (% raz "ra -z")
 (% chrome "google-chrome-unstable")
 (% stop-chrome "kill-chrome -STOP")
 (% continue-chrome "kill-chrome -CONT")
 (% qpdf "qpdfview")
 (% rt "rtorrent")
 (% rm@ "shred -vfzun 10")
 (% par "parallel --will-cite")
 (% v "less")
 (% xv "xzless")
 (% bt! "pacmd set-default-sink bluez_sink.04_FE_A1_31_0B_7E.a2dp_sink"))

(exporting-definitions
 (defun len (&rest args)
   (setf (getenv "LANG") "en_US.UTF-8")
   (run/i `(,@args)))

 (defun leo (&rest args)
   (setf (getenv "LANG") "eo.utf8")
   (run/i `(,@args)))

 (defun tele (&rest args)
   (setf (getenv "PATH") (unix-namestring (home ".baf/profiles/tdesktop/bin")))
   (run/i `(telegram-desktop ,@args)))

 (defun tox (&rest args)
   (setf (getenv "PATH") (unix-namestring (home ".baf/profiles/qtox/bin")))
   (run/i `(qtox ,@args)))

 (defun vbox (&rest args)
   (setf (getenv "PATH") "/var/setuid-wrappers:/run/wrappers/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin")
   (run/i `("VirtualBox" ,@args)))

 (defun raz! (&rest args)
   (apply-args-1 'raz args :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
   (success))

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
          (file (format nil "~A.png" (local-time:format-timestring nil (local-time:now))))
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
   (success)))

(register-commands :scripts/apps)
