;;;; apps.lisp

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
           #:skype

           #:xrsync
           #:ra
           #:raz
           #:raz!
           #:chrome
           #:tele
           #:vibe
           #:qp
           #:rt
           #:rm@
           #:par
           #:v
           #:xv

           #:bt
           #:bt0
           #:bt1

           #:len
           #:leo
           #:tox
           #:vbox

           #:shell
           #:rshell
           #:screenshot
           #:sg2e
           #:smb))

(in-package :scripts/apps)

(defvar +screenshots-dir+ (home "hejmo/elsx/bil/ekrankopioj"))

(exporting-definitions
  (% s "sudo")
  (% e "emacsclient -nw")
  (% term "len urxvt")
  (% fire "firefox")
  (% chrome "google-chrome-unstable")
  (% tele "telegram-desktop")
  (% keep "keepassxc")
  (% xrsync "rsync -rlptgoDHSx")
  (% ra "xrsync")
  (% raz "ra -z")
  (% qp "qpdfview")
  (% rt "rtorrent")
  (% rm@ "shred -vfzun 10")
  (% par "parallel --will-cite")
  (% v "less")
  (% xv "xzless")
  (% bt "bluetoothctl")
  (% kt "len krita"))

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
  (defun tox (&rest args) (run-nix-user "qtox" "qtox" args))
  (defun vbox () (run-nix-system "VirtualBox"))

  (defun raz! (&rest args)
    (apply-args-1 'raz args :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
    (success)))

(exporting-definitions
  (defun bt0 ()
    (run/i `(pacmd "set-default-sink" "bluez_sink.B8_D5_0B_8D_77_EB.a2dp_sink"))
    (run/i `(pacmd "set-default-source" "bluez_sink.B8_D5_0B_8D_77_EB.a2dp_sink.monitor"))
    (run/i `(pacmd "set-port-latency-offset" "bluez_card.B8_D5_0B_8D_77_EB" "headphone-output" "250000"))
    (success))

  (defun bt1 ()
    (run/i `(pacmd "set-default-sink" "bluez_sink.04_FE_A1_31_0B_7E.a2dp_sink"))
    (run/i `(pacmd "set-default-source" "bluez_sink.04_FE_A1_31_0B_7E.a2dp_sink.monitor"))
    (success))

  (defun shell (&rest args)
    (let ((directory (pathname-directory-pathname (find-binary (argv0)))))
      (run/i `(nix-shell --pure ,(format nil "~A/default.nix" directory) ,@args))
      (success)))

  (defun rshell (command)
    (shell "--command" (format nil " rlwrap -s 1000000 -c -b \"(){}[].,=&^%0\;|\" ~A" command)))

  (defun screenshot (mode)
    (let* ((dir +screenshots-dir+)
           (file (format nil "~A.png" (local-time:format-timestring nil (local-time:now))))
           (dest (format nil "mv $f ~A" dir))
           (image (format nil "~A/~A" dir file)))
      (flet ((scrot (file dest &rest args)
               (run/i `(scrot ,@args ,file -e ,dest))))
        (match mode
          ((ppcre "(full|tute)") (scrot file dest))
          ((ppcre "(region|parte)") (scrot file dest '-s))
          (_ (err (format nil "invalid mode ~A~%" mode))))
        (run `(xclip -selection clipboard) :input (list image))
        (success)))))

(exporting-definitions
  (defun sg2e (&rest args)
    (declare (ignore args))
    (run/i `(stem "-X" ,(argv0) "steam://rungameid/245170"))
    (success))

  (defun smb (&rest args)
    (declare (ignore args))
    (run/i `(stem "-s"  "steam://rungameid/40800"))
    (success)))

(register-commands :scripts/apps)
