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
  (:export #:bt
           #:dv
           #:ds
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
           #:b
           #:bm
           #:cv
           #:dc
           #:earth
           #:ev
           #:fs
           #:lx
           #:mx
           #:p
           #:pc
           #:pe
           #:tx
           #:ty
           #:sg2e
           #:sm
           #:sp
           #:tb
           #:vbm
           #:vl
           #:vl!
           #:xb
           #:xmind
           #:xo
           #:xs
           #:za

           #:bb
           #:cb
           #:fb
           #:qb

           #:ca
           #:eb
           #:kp
           #:kt
           #:mb
           #:ok
           #:qbt
           #:qt4
           #:qt5
           #:qtx
           #:rd
           #:sw
           #:vp
           #:td

           #:fcade
           #:ui
           #:ni
           #:xu

           #:lc
           #:len
           #:leo
           #:vb

           #:viber

           #:rz!
           #:screenshot
           #:xmsg
           #:xrun
           #:xm

           #:kb
           #:lispworks
           #:lispworks-cli
           #:ts
           #:tresorit
           #:edraw
           #:shell

           #:smallcaps))

(in-package #:scripts/apps)

(defvar +screenshots-dir+ (mof:home ".screenshots"))

(exporting-definitions
 (% bt "bluetoothctl")
 (% dv "gdrive upload --recursive")
 (% e "emacsclient -nw")
 (% gpg "gpg2")
 (% par "parallel")
 (% pm "pulsemixer")
 (% rz "rsync -az")
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
 (% b "gqview")
 (% bm "blueman-manager")
 (% cv "guvcview")
 (% dc "Discord")
 (% earth "googleearth")
 (% ev "evince")
 (% fs "gtk2fontsel")
 (% lx "lxappearance")
 (% mx "len wxmaxima")
 (% p "mpv --mute")
 (% pc "pavucontrol")
 (% pe "pulseeffects")
 (% tx "len urxvt")
 (% ty "terminator")
 (% sg2e "steam -applaunch 245170")
 (% sm "stellarium")
 (% sp "speedcrunch")
 (% tb "tor-browser")
 (% vbm "VBoxManage")
 (% vl "vlc -I ncurses")
 (% vl! "vl --random --loop --playlist-autostart")
 (% xb "chromium")
 (% xmind "XMind")
 (% xo "xournal")
 (% xs "simple-scan")
 (% za "zathura"))

(exporting-definitions
 (% bb "brave")
 (% cb "google-chrome-stable")
 (% fb "firefox")
 ($ qb "qutebrowser"))

(exporting-definitions
 ($ ca "calibre")
 ($ eb "ebook-viewer")
 ($ kp "keepassxc")
 ($ kt "krita")
 ($ mb "mumble")
 ($ ok "okular")
 ($ qbt "qbittorrent")
 ($ qt4 "qtconfig")
 ($ qt5 "qt5ct")
 ($ qtx "qtox")
 ($ rd "qt-recordMyDesktop")
 ($ sw "Write")
 ($ vp "vlc")
 ($ td "telegram-desktop"))

(exporting-definitions
 (@ fcade "/pub/ludoj/emu/fightcade/FightCade.exe")
 (@ ui "uninstaller")
 (@+ ni "Neat Image Standalone/NeatImage.exe")
 (@+ xu "Xenu/Xenu.exe"))

(exporting-definitions
 (defcommand lc (&rest args) (run-with-locale "C" args))
 (defcommand len (&rest args) (run-with-locale "en_US.UTF-8" args))
 (defcommand leo (&rest args) (run-with-locale "eo.utf8" args))
 (defcommand vb () (run-with-nix-system "VirtualBox")))

(defun paths (x y)
  "Merge path namestrings."
  (mof:cat (uiop:native-namestring (mof:expand-pathname x)) ":" y))

(exporting-definitions
 (defcommand ts (&rest args)
   (run-with-chroot (mof:home ".local/share/tresorit/tresorit") args))

 (defcommand tresorit ()
   (run-with-docker-x
    `("-v" ,(paths "~/.local/share/tresorit/Profiles" "/home/tresorit/.local/share/tresorit/Profiles")
      "-v" ,(paths "~/.local/share/tresorit/Logs" "/home/tresorit/.local/share/tresorit/Logs")
      "-v" ,(paths "~/.local/share/tresorit/Reports" "/home/tresorit/.local/share/tresorit/Reports")
      "-v" ,(paths "~/.local/share/tresorit/Temp" "/home/tresorit/.local/share/tresorit/Temp")
      "-v" ,(paths "~/.config/Tresorit" "/home/tresorit/.config/Tresorit")
      "-v" ,(paths "~/Tresors" "/home/tresorit/Tresors"))
    "tresorit"))

 (defcommand viber ()
   (run-with-docker-x
    `("-v" ,(paths "~/.ViberPC/" "/root/.ViberPC/")
      "-v" ,(paths (xdg-dir "DESKTOP") "/root/Desktop/")
      "-v" ,(paths (xdg-dir "DOWNLOAD") "/root/Downloads/"))
    "viber")))

(exporting-definitions
 (defcommand rz! (&rest args)
   (apply-args-1
    'rz args
    :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
   (success))

 (defcommand screenshot (mode)
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

 (defcommand xmsg (&rest args)
   (run/i `("xmessage"
            "-fn" "-*-speedy-*-*-*-*-12-*-*-*-*-*-*-*"
            "-fg" "white" "-bg" "black"
            "-timeout" "5" "-buttons" ""
            ,@args))
   (success))
 (defcommand xrun (&rest args)
   (run/i `("gmrun" "-geometry" "+0+0" ,@args))
   (success))

 (defcommand xm (&rest args)
   (run/i `("xmonad" "--recompile"))
   (run/i `("xmonad" "--restart"))
   (success))

 (defcommand ds (&rest args)
   (run `("sudo" "pkill" "-9" "ds4drv") :output :interactive :on-error nil)
   (run `("sudo" "rm" "-f" "/tmp/ds4drv.pid") :output :interactive :on-error nil)
   (run/i `("sudo" "ds4drv" "--daemon" "--config" ,(mof:expand-pathname "~/.config/ds4drv.conf")))
   (success)))

(defun run-with-chroot (program args)
  "Run PROGRAM inside the chroot."
  (run/i `("zsh" "-c" ,(mof:cat "cr " (namestring program)) ,args))
  (success))

(exporting-definitions
 (defcommand kb (&rest args)
   (setf (getenv "NIX_SKIP_KEYBASE_CHECKS") "1")
   (run/i `("keybase-gui" ,@args))
   (success))

 (defcommand lispworks (&rest args)
   (run-with-chroot "/usr/local/lib/LispWorks/lispworks-7-0-0-x86-linux" args))

 (defcommand lispworks-cli (&rest args)
   (run-with-chroot "/home/pub/hejmo/apoj/lispworks/save-image/lispworks-cli" args))

 (defcommand edraw (&rest args)
   (run-with-chroot "edrawmax" args))

 (defcommand shell (&rest args)
   (destructuring-bind (base &rest arguments)
       args
     (let* ((cwd (uiop:getcwd))
            (path (mof:cat (xdg-dir "TEMPLATES") "/shell/"))
            (directory (mof:cat path base))
            (default-command (or arguments "bash")))
       (when (uiop:directory-exists-p directory)
         (format t "Loading shell from ~A...~%" directory)
         (uiop:chdir directory)
         (run/i `("baf" "shell" "--run" ,(format nil "sh -c \"cd ~A; ~A\"" cwd default-command)))))
     (success))))

(defvar *smallcaps-alist*
  '((#\a . #\ᴀ)
    (#\b . #\ʙ)
    (#\c . #\ᴄ)
    (#\d . #\ᴅ)
    (#\e . #\ᴇ)
    (#\f . #\ꜰ)
    (#\g . #\ɢ)
    (#\h . #\ʜ)
    (#\i . #\ɪ)
    (#\j . #\ᴊ)
    (#\k . #\ᴋ)
    (#\l . #\ʟ)
    (#\m . #\ᴍ)
    (#\n . #\ɴ)
    (#\o . #\ᴏ)
    (#\p . #\ᴘ)
    (#\q . #\ǫ)
    (#\r . #\ʀ)
    (#\s . #\s)
    (#\t . #\ᴛ)
    (#\u . #\ᴜ)
    (#\v . #\ᴠ)
    (#\w . #\ᴡ)
    (#\x . #\x)
    (#\y . #\ʏ)
    (#\z . #\ᴢ)))

(exporting-definitions
 (defcommand smallcaps (text)
   "Output the smallcaps version of TEXT."
   (labels ((fn (base)
              "Return the smallcaps version of BASE."
              (let ((value (cdr (assoc base *smallcaps-alist*))))
                (if (and value (lower-case-p base))
                    value
                    base))))
     (loop :for char :across text :do (format t "~A" (fn char)))
     (success))))

(register-commands :scripts/apps)
