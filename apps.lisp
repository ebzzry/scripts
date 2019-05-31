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
           #:gpg
           #:par
           #:pm
           #:rz
           #:rl
           #:rm@
           #:s
           #:us
           #:vg
           #:xf
           #:xm
           #:zx

           #:ae
           #:au
           #:av
           #:bm
           #:bb
           #:cb
           #:cv
           #:dc
           #:earth
           #:ev
           #:fb
           #:fs
           #:lo
           #:lx
           #:mx
           #:ob
           #:p
           #:pc
           #:pe
           #:sg2e
           #:smb
           #:tx
           #:ty
           #:sm
           #:sp
           #:tb
           #:xb
           #:xo
           #:xs
           #:za

           #:b
           #:ca
           #:demu
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
           #:vl

           #:fcade
           #:ui
           #:ni
           #:xu

           #:lc
           #:len
           #:leo
           #:vb
           #:vr
           #:zu

           #:rz!
           #:screenshot
           #:xmsg
           #:xrun
           #:xm))

(in-package #:scripts/apps)

(defvar +screenshots-dir+ (mof:home ".screenshots"))

(exporting-definitions
 (% bt "bluetoothctl")
 (% dv "gdrive upload --recursive")
 (% e "emacsclient -nw")
 (% gpg "gpg2")
 (% par "parallel")
 (% pm "pulsemixer")
 (% rz "rsync -rlptgoD -HAX -x -z")
 (% rl "rlwrap -s 1000000 -c -b \"(){}[].,=&^%$#@\\;|\"")
 (% rm@ "shred -vfzun 10")
 (% s "sudo")
 (% us "usync --one-way --prefer-local")
 (% vg "vagrant")
 (% xf "xmllint --format")
 (% zx "zsh -c"))

(exporting-definitions
 (% ae "aegisub")
 (% au "audacity")
 (% av "ahoviewer")
 (% bm "blueman-manager")
 (% bb "brave")
 (% cb "google-chrome-stable")
 (% cv "guvcview")
 (% dc "Discord")
 (% earth "googleearth")
 (% ev "evince")
 (% fb "firefox")
 (% fs "gtk2fontsel")
 (% lo "libreoffice")
 (% lx "lxappearance")
 (% mx "len wxmaxima")
 (% ob "opera --private")
 (% p "mpv --mute")
 (% pc "pavucontrol")
 (% pe "pulseeffects")
 (% sg2e "steam -applaunch 245170")
 (% smb "steam -applaunch 40800")
 (% tx "len urxvt")
 (% ty "terminator")
 (% sm "stellarium")
 (% sp "speedcrunch")
 (% tb "tor-browser")
 (% xb "chromium")
 (% xo "xournal")
 (% xs "simple-scan")
 (% za "zathura"))

(exporting-definitions
 ($ b "phototonic")
 ($ ca "calibre")
 ($ demu "dolphin-emu-master")
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
 ($ vl "vlc -I ncurses --playlist-autostart"))

(exporting-definitions
 (@ fcade "/pub/ludoj/emu/fightcade/FightCade.exe")
 (@ ui "uninstaller")
 (@+ ni "Neat Image Standalone/NeatImage.exe")
 (@+ xu "Xenu/Xenu.exe"))

(exporting-definitions
 (defun lc (&rest args) (run-with-locale "C" args))
 (defun len (&rest args) (run-with-locale "en_US.UTF-8" args))
 (defun leo (&rest args) (run-with-locale "eo.utf8" args))
 (defun vb () (run-with-nix-system "VirtualBox"))
 (defun vr () (run-with-docker-x "viber"))
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
            "-timeout" "5" "-buttons" ""
            ,@args))
   (success))

 (defun xrun (&rest args)
   (run/i `("gmrun" "-geometry" "+0+0" ,@args))
   (success))

 (defun xm (&rest args)
   (run/i `("xmonad" "--recompile"))
   (run/i `("xmonad" "--restart"))
   (success)))

(register-commands :scripts/apps)
