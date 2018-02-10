;;; apps.lisp

(uiop:define-package
    :scripts/apps
    (:use #:cl
          #:fare-utils
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:optima
          #:optima.ppcre
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/utils)
  (:export #:s
           #:e
           #:term
           #:fire
           #:keep
           #:xrsync
           #:ra
           #:raz
           #:raz!
           #:chrome
           #:qp
           #:rt
           #:rm@
           #:par
           #:v
           #:xv
           #:bt!
           #:c@

           #:len
           #:leo
           #:tele
           #:vibe
           #:tox
           #:vbox

           #:shell
           #:shell
           #:screenshot
           #:sg2e))

(in-package :scripts/apps)

(defvar +screenshots-dir+ (home "hejmo/elsx/bil/ekrankopioj"))

(exporting-definitions
 (% s "sudo")
 (% e "emacsclient -nw")
 (% term "len urxvt")
 (% fire "firefox")
 (% keep "keepassxc")
 (% xrsync "rsync -rlptgoDHSx")
 (% ra "xrsync")
 (% raz "ra -z")
 (% chrome "google-chrome-unstable")
 (% qp "qpdfview")
 (% rt "rtorrent")
 (% rm@ "shred -vfzun 10")
 (% par "parallel --will-cite")
 (% v "less")
 (% xv "xzless")
 (% bt! "pacmd set-default-sink bluez_sink.04_FE_A1_31_0B_7E.a2dp_sink")
 (% c@ "xclip -selection clipboard"))

(defun run-locale (locale &rest args)
  "Run args with locale set to LOCALE"
  (setf (getenv "LANG") locale)
  (run/i `(,@args))
  (success))

(defun run-nix-user (profile binary &rest args)
  "Run binary under a separate profile"
  (let ((bin (home (format nil ".baf/profiles/~A/bin" profile))))
    (setf (getenv "PATH") (unix-namestring bin))
    (run/i `(,binary ,@args))))

(defun run-nix-system (binary &rest args)
  "Run binary without user paths"
  (setf (getenv "PATH") "/var/setuid-wrappers:/run/wrappers/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin")
  (run/i `(,binary ,@args)))

(exporting-definitions
 (defun len (&rest args) (run-locale "en_US.UTF-8" args))
 (defun leo (&rest args) (run-locale "eo.utf8" args))

 (defun tele (&rest args) (run-nix-user "tdesktop" "telegram-desktop" args))
 (defun vibe (&rest args) (run-nix-user "viber" "viber" args))
 (defun tox (&rest args) (run-nix-user "qtox" "qtox" args))

 (defun vbox (&rest args) (run-nix-system "VirtualBox"))

 (defun raz! (&rest args)
   (apply-args-1 'raz args :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
   (success)))

(exporting-definitions
 (defun shell (&rest args)
   (let ((directory (pathname-directory-pathname (find-binary (argv0)))))
     (run/i `(nix-shell --pure ,(format nil "~A/default.nix" directory) ,@args))
     (success)))

 (defun shell@ (command)
   (shell "--command" (format nil " rlwrap -s 1000000 -c -b \"(){}[].,=&^%0\;|\" ~A" command)))

 (defun screenshot (mode)
   (let* ((dir +screenshots-dir+)
          (file (format nil "~A.png" (local-time:format-timestring nil (local-time:now))))
          (dest (format nil "mv $f ~A" dir))
          (image (format nil "~A/~A" dir file)))
     (flet ((scrot (file dest &rest args)
              (run/i `(scrot ,@args ,file -e ,dest))))
       (match mode
         ((ppcre "(full|tuta)") (scrot file dest))
         ((ppcre "(region|parta)") (scrot file dest '-s))
         (_ (err (format nil "invalid mode ~A~%" mode))))
       (run `(xclip -selection clipboard) :input (list image))
       (success))))

 (defun sg2e (&rest args)
   (declare (ignore args))
   (run/i `(stem "-X" ,(argv0) "steam://rungameid/245170"))
   (success)))

(register-commands :scripts/apps)
