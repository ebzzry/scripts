;;; apps.lisp

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
           #:e@
           #:v
           #:f
           #:g
           #:gi
           #:p
           #:p@
           #:rxvt
           #:ob
           #:cb
           #:tb
           #:pm
           #:pc
           #:pe
           #:bt
           #:bm
           #:ra
           #:raz
           #:rm@
           #:par
           #:xo
           #:lu
           #:wt
           #:cv
           #:lx
           #:au
           #:lo
           #:gpg
           #:fs
           #:dv
           #:za
           #:ev
           #:av
           #:zu
           #:sm
           #:sp
           #:earth
           #:vv
           #:rl
           #:zx
           #:us
           #:ds
           #:xscan

           #:o
           #:b
           #:qct
           #:qtx
           #:qbt
           #:eb
           #:td
           #:kp
           #:kt
           #:rmd
           #:vl
           #:vl@
           #:skan

           #:xu
           #:ni
           #:ui

           #:lc
           #:len
           #:leo
           #:vb

           #:raz!
           #:shell
           #:screenshot
           #:xmsg
           #:xrun

           #:sg2e
           #:smb
           #:fightcade))

(in-package #:scripts/apps)

(defvar +screenshots-dir+ (mof:home ".screenshots"))

(exporting-definitions
 (% s "sudo")
 (% e "emacsclient -nw")
 (% e@ "emacs -nw -Q")
 (% v "less")
 (% f "fd")
 (% g "rg --color auto")
 (% gi "g -i")
 (% p "mpv --fs")
 (% p@ "p --mute")
 (% rxvt "len urxvt")
 (% ob "opera --private")
 (% cb "google-chrome-stable")
 (% tb "tor-browser")
 (% pm "pulsemixer")
 (% pc "pavucontrol")
 (% pe "pulseeffects")
 (% bt "bluetoothctl")
 (% bm "blueman-manager")
 (% ra "rsync -rlptgoDHSx")
 (% raz "ra -z")
 (% rm@ "shred -vfzun 10")
 (% par "parallel")
 (% xo "xournal")
 (% lu "o https://limnu.com/d/user.html")
 (% wt "weechat")
 (% cv "guvcview")
 (% lx "lxappearance")
 (% au "audacity")
 (% lo "libreoffice")
 (% gpg "gpg2")
 (% fs "gtk2fontsel")
 (% dv "gdrive upload --recursive")
 (% za "zathura")
 (% ev "evince")
 (% av "ahoviewer")
 (% zu "zoom-us")
 (% sm "stellarium")
 (% sp "speedcrunch")
 (% earth "googleearth")
 (% vv "vncviewer")
 (% rl "rlwrap -s 1000000 -c -b \"(){}[].,=&^%$#@\\;|\"")
 (% zx "zsh -c")
 (% us "usync --one-way --prefer-local")
 (% ds "Discord")
 (% xscan "simple-scan"))

(exporting-definitions
 ($ o "qutebrowser")
 ($ b "phototonic")
 ($ qct "qt5ct")
 ($ qtx "qtox")
 ($ qbt "qbittorrent")
 ($ eb "ebook-viewer")
 ($ td "telegram-desktop")
 ($ kp "keepassxc")
 ($ kt "krita")
 ($ rmd "qt-recordMyDesktop")
 ($ vl "vlc -I ncurses --playlist-autostart")
 ($ vl@ "vlc -I qt --playlist-autostart")
 ($ skan "skanlite"))

(exporting-definitions
 (@ ui "uninstaller")
 (@+ xu "Xenu/Xenu.exe")
 (@+ ni "Neat Image Standalone/NeatImage.exe"))

(exporting-definitions
 (defun lc (&rest args) (run-with-locale "C" args))
 (defun len (&rest args) (run-with-locale "en_US.UTF-8" args))
 (defun leo (&rest args) (run-with-locale "eo.utf8" args))
 (defun vb () (run-with-nix-system "VirtualBox")))

(exporting-definitions
 (defun raz! (&rest args)
   (apply-args-1
    'raz args
    :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
   (success))

 (defun shell (&rest args)
   (let ((directory (pathname-directory-pathname (find-binary (argv0)))))
     (run/i `("nix-shell" "--pure" ,(mof:fmt "~A/default.nix" directory) ,@args))
     (success)))

 (defun screenshot (mode)
   (let* ((dir (uiop:truenamize +screenshots-dir+))
          (file (mof:fmt "~A.png" (local-time:format-timestring nil (local-time:now))))
          (dest (mof:fmt "mv $f ~A" dir))
          (image (mof:fmt "~A~A" dir file)))
     (flet ((scrot (file dest &rest args)
              (run/i `("scrot" ,@args ,file -e ,dest))))
       (match mode
              ((ppcre "(full)") (scrot file dest))
              ((ppcre "(region)") (scrot file dest '-s))
              (_ (err (mof:fmt "invalid mode ~A~%" mode))))
       (run `("xclip" "-selection" "clipboard" "-t" "image/png" ,image))
       (success))))

 (defun xmsg (&rest args)
   (run/i `("xmessage"
            "-fn" "-*-speedy-*-*-*-*-12-*-*-*-*-*-*-*"
            "-fg" "white" "-bg" "black"
            "-timeout" "2" "-buttons" ""
            ,@args))
   (success))

 (defun xrun (&rest args)
   (run/i `("rofi" "-show" "run" ,@args))
   (success)))

(exporting-definitions
 (defun sg2e ()
   (run/i `("stem" "-X" ,(argv0) "--" "-applaunch" "245170"))
   (success))

 (defun fightcade ()
   (run/i `("stem" "-x" "fightcade"))
   (run-with-wine "/pub/ludoj/emu/fightcade/FightCade.exe")
   (success)))

(register-commands :scripts/apps)
