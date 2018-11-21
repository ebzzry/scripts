;;;; apps.lisp

(uiop:define-package #:scripts/apps
    (:use #:cl
          #:fare-utils
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:optima
          #:optima.ppcre
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/utils
          #:scripts/unix)
  (:export #:s
           #:e
           #:cmd
           #:fb
           #:cb
           #:ra
           #:raz
           #:rt
           #:rm@
           #:par
           #:bt
           #:xo
           #:pm
           #:lu
           #:dg
           #:wt
           #:cv
           #:lx
           #:au
           #:vl!
           #:dx
           #:oa
           #:tb
           #:o
           #:p
           #:b
           #:lo
           #:gpg
           #:fs
           #:dv
           #:v
           #:f
           #:za
           #:ir

           #:qt5ct
           #:qtox
           #:calibre
           #:ebook-viewer #:eb
           #:vlc
           #:telegram-desktop #:td
           #:keepassxc #:kp
           #:krita #:kt
           #:obs
           #:sw
           #:viber
           #:qtrecordmydesktop #:rmd
           #:scribus
           #:masterpdfeditor
           #:qutebrowser #:qb
           #:phototonic
           #:pencil
           #:sqlitebrowser
           #:xpdf
           #:wpsd
           #:wpsp
           #:wpss

           #:xu
           #:re
           #:ni

           #:lc
           #:len
           #:leo
           #:vb

           #:raz!
           #:lispworks-chroot-gui
           #:lispworks-chroot-cli
           #:lispworks-docker-gui

           #:shell
           #:rshell
           #:screenshot
           #:sg2e
           #:smb

           #:bt-air
           #:bt-pulse))

(in-package #:scripts/apps)

(defvar +screenshots-dir+ (mof:home ".screenshots"))

(exporting-definitions
 (% s "sudo")
 (% e "emacsclient -nw")
 (% cmd "len urxvt")
 (% fb "firefox")
 (% cb "google-chrome-stable")
 (% pm "pulsemixer")
 (% bt "bluetoothctl")
 (% ra "rsync -rlptgoDHSx")
 (% raz "ra -z")
 (% rt "rtorrent")
 (% rm@ "shred -vfzun 10")
 (% par "parallel --will-cite")
 (% xo "xournal")
 (% lu "o https://limnu.com/d/user.html")
 (% dg "deluge")
 (% wt "weechat")
 (% cv "guvcview")
 (% lx "lxappearance")
 (% au "audacity")
 (% vl! "vlc --playlist-autostart")
 (% dx "Discord")
 (% oa "opera")
 (% tb "tor-browser")
 (% o "qutebrowser")
 (% p "xpdf")
 (% b "phototonic")
 (% lo "libreoffice")
 (% gpg "gpg2")
 (% fs "gtk2fontsel")
 (% dv "gdrive upload --recursive")
 (% v "less")
 (% f "fd")
 (% za "zathura")
 (% ir "gimagereader"))

(exporting-definitions
 ($ "qt5ct" qt5ct)
 ($ "qtox" qtox)
 ($ "calibre" calibre)
 ($ "ebook-viewer" ebook-viewer eb)
 ($ "vlc" vlc)
 ($ "telegram-desktop" telegram-desktop td)
 ($ "keepassxc" keepassxc kp)
 ($ "krita" krita kt)
 ($ "obs" obs)
 ($ "Write" sw)
 ($ "viber" viber)
 ($ "qt-recordMyDesktop" qtrecordmydesktop rmd)
 ($ "scribus" scribus)
 ($ "masterpdfeditor5" masterpdfeditor)
 ($ "qutebrowser" qutebrowser qb)
 ($ "phototonic" phototonic)
 ($ "pencil" pencil)
 ($ "sqlitebrowser" sqlitebrowser)
 ($ "xpdf" xpdf)
 ($ "wps" wpsd)
 ($ "wpp" wpsp)
 ($ "et" wpss))

(exporting-definitions
 (@ xu "Xenu/Xenu.exe")
 (@ re "The Regex Coach/The Regex Coach.exe")
 (@ ni "Neat Image Standalone/NeatImage.exe"))

(exporting-definitions
 (defun lc (&rest args) (run-with-locale "C" args))
 (defun len (&rest args) (run-with-locale "en_US.UTF-8" args))
 (defun leo (&rest args) (run-with-locale "eo.utf8" args))
 (defun vb () (run-with-nix-system "VirtualBox")))

(exporting-definitions
 (defun raz! (&rest args)
   (apply-args-1 'raz args
                 :options
                 '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
   (success))

 (defun lispworks-chroot-gui (&rest args)
   (run/i `(zsh "-c" "cr /usr/local/lib/LispWorks/lispworks-7-0-0-x86-linux" ,@args))
   (success))

 (defun lispworks-chroot-cli (&rest args)
   (run/i `(zsh "-c" "cr /home/pub/hejmo/apoj/lispworks/save-image/lispworks-cli" ,@args))
   (success))

 (defun lispworks-docker-gui (&rest args)
   (run/i `(sh "-c" ,(mof:home "hejmo/fkd/sxelo/lispworks/lispworks70_linux_x86") ,@args))
   (success)))

(exporting-definitions
 (defun shell (&rest args)
   (let ((directory (pathname-directory-pathname (find-binary (argv0)))))
     (run/i `(nix-shell --pure ,(mof:fmt "~A/default.nix" directory) ,@args))
     (success)))

 (defun rshell (command)
   (shell "--command" (mof:fmt " rlwrap -s 1000000 -c -b \"(){}[].,=&^%0\;|\" ~A" command)))

 (defun screenshot (mode)
   (let* ((dir (uiop:truenamize +screenshots-dir+))
          (file (mof:fmt "~A.png" (local-time:format-timestring nil (local-time:now))))
          (dest (mof:fmt "mv $f ~A" dir))
          (image (mof:fmt "~A~A" dir file)))
     (flet ((scrot (file dest &rest args)
              (run/i `(scrot ,@args ,file -e ,dest))))
       (match mode
              ((ppcre "(full)") (scrot file dest))
              ((ppcre "(region)") (scrot file dest '-s))
              (_ (err (mof:fmt "invalid mode ~A~%" mode))))
       (run `("xclip" "-selection" "clipboard") :input (list image))
       (success))))

 (defun bt-air ()
   (run/i `(pacmd "set-default-sink" "bluez_sink.B8_D5_0B_8D_77_EB.a2dp_sink"))
   (run/i `(pacmd "set-default-source" "bluez_sink.B8_D5_0B_8D_77_EB.a2dp_sink.monitor"))
   (run/i `(pacmd "set-port-latency-offset" "bluez_card.B8_D5_0B_8D_77_EB" "headphone-output" "250000"))
   (success))

 (defun bt-pulse ()
   (run/i `(pacmd "set-default-sink" "bluez_sink.04_FE_A1_31_0B_7E.a2dp_sink"))
   (run/i `(pacmd "set-default-source" "bluez_sink.04_FE_A1_31_0B_7E.a2dp_sink.monitor"))
   (success))

 (defun sg2e ()
   (run/i `("stem" "-X" ,(argv0) "steam://rungameid/245170"))
   (success))

 (defun smb ()
   (run/i `("stem" "-s"  "steam://rungameid/40800"))
   (success)))

(register-commands :scripts/apps)
