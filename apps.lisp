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
  (:export #:bt
           #:dv
           #:e
           #:f
           #:g
           #:gi
           #:gpg
           #:par
           #:pm
           #:rz
           #:rl
           #:rm@
           #:s
           #:us
           #:v
           #:vg
           #:zx

           #:ae
           #:au
           #:av
           #:bm
           #:cb
           #:cv
           #:ds
           #:earth
           #:ev
           #:fs
           #:lo
           #:lx
           #:mx
           #:ob
           #:p
           #:pc
           #:pe
           #:rxvt
           #:sm
           #:sp
           #:tb
           #:vv
           #:vvv
           #:xb
           #:xo
           #:xs
           #:za

           #:b
           #:ca
           #:eb
           #:kp
           #:kt
           #:mb
           #:o
           #:ok
           #:qbt
           #:qt4
           #:qt5
           #:qtx
           #:rmd
           #:sw
           #:td
           #:vb
           #:vl

           #:xu
           #:ni
           #:ui

           #:lc
           #:len
           #:leo
           #:vb@
           #:zu

           #:rz!
           #:screenshot
           #:xmsg
           #:xrun

           #:sg2e
           #:smb
           #:fightcade))

(in-package #:scripts/apps)

(defvar +screenshots-dir+ (mof:home ".screenshots"))

(exporting-definitions
 (% bt "bluetoothctl")
 (% dv "gdrive upload --recursive")
 (% e "emacsclient -nw")
 (% f "fd")
 (% g "rg --color auto")
 (% gi "g -i")
 (% gpg "gpg2")
 (% par "parallel")
 (% pm "pulsemixer")
 (% rz "rsync -rlptgoD -HAX -x -z")
 (% rl "rlwrap -s 1000000 -c -b \"(){}[].,=&^%$#@\\;|\"")
 (% rm@ "shred -vfzun 10")
 (% s "sudo")
 (% us "usync --one-way --prefer-local")
 (% v "less")
 (% vg "vagrant")
 (% zx "zsh -c"))

(exporting-definitions
 (% ae "aegisub")
 (% au "audacity")
 (% av "ahoviewer")
 (% bm "blueman-manager")
 (% cb "google-chrome-stable")
 (% cv "guvcview")
 (% ds "Discord")
 (% earth "googleearth")
 (% ev "evince")
 (% fs "gtk2fontsel")
 (% lo "libreoffice")
 (% lx "lxappearance")
 (% mx "len wxmaxima")
 (% ob "opera --private")
 (% p "mpv --fs --mute")
 (% pc "pavucontrol")
 (% pe "pulseeffects")
 (% rxvt "len urxvt")
 (% sm "stellarium")
 (% sp "speedcrunch")
 (% tb "tor-browser")
 (% vv "vncviewer")
 (% vvv "vncviewer -ViewOnly=1")
 (% xb "chromium")
 (% xo "xournal")
 (% xs "simple-scan")
 (% za "zathura")
 (% zu "zoom-us"))

(exporting-definitions
 ($ b "phototonic")
 ($ ca "calibre")
 ($ eb "ebook-viewer")
 ($ kp "keepassxc")
 ($ kt "krita")
 ($ mb "mumble")
 ($ o "qutebrowser")
 ($ ok "okular")
 ($ qbt "qbittorrent")
 ($ qt4 "qtconfig")
 ($ qt5 "qt5ct")
 ($ qtx "qtox")
 ($ rmd "qt-recordMyDesktop")
 ($ sw "Write")
 ($ td "telegram-desktop")
 ($ vb "VirtualBox")
 ($ vl "vlc -I ncurses --playlist-autostart"))

(exporting-definitions
 (@ ui "uninstaller")
 (@+ xu "Xenu/Xenu.exe")
 (@+ ni "Neat Image Standalone/NeatImage.exe"))

(exporting-definitions
 (defun lc (&rest args) (run-with-locale "C" args))
 (defun len (&rest args) (run-with-locale "en_US.UTF-8" args))
 (defun leo (&rest args) (run-with-locale "eo.utf8" args))
 (defun vb@ () (run-with-nix-system "VirtualBox"))
 (defun zu (&rest args) (run-with-libgl-always-software "zoom-us" args)))

(exporting-definitions
 (defun rz! (&rest args)
   (apply-args-1
    'rz args
    :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
   (success))

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
   (run/i `("gmrun" "-geometry" "+0+0" ,@args))
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
