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
           #:o
           #:b
           #:p
           #:p@
           #:rxvt
           #:ob
           #:cb
           #:tb
           #:pm
           #:bt
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
           #:v
           #:f
           #:za
           #:ev
           #:av
           #:zu
           #:qct
           #:qtx
           #:eb
           #:vl
           #:vl@
           #:td
           #:kp
           #:kt
           #:rmd
           #:sm
           #:sp
           #:earth
           #:vv
           #:rl
           #:us
           #:ds
           #:zx

           #:sicp@
           #:lisp@

           #:xu
           #:re
           #:ni
           #:ui

           #:lc
           #:len
           #:leo
           #:vb

           #:raz!
           #:shell
           #:rshell
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
 (% o "qutebrowser")
 (% b "phototonic")
 (% p "mpv --fs")
 (% p@ "p --mute")
 (% rxvt "len urxvt")
 (% ob "opera --private")
 (% cb "google-chrome-stable")
 (% tb "tor-browser")
 (% pm "pulsemixer")
 (% bt "bluetoothctl")
 (% ra "rsync -rlptgoDHSx")
 (% raz "ra -z")
 (% rm@ "shred -vfzun 10")
 (% par "parallel --will-cite")
 (% xo "xournal")
 (% lu "o https://limnu.com/d/user.html")
 (% wt "weechat")
 (% cv "guvcview")
 (% lx "lxappearance")
 (% au "audacity")
 (% vl "vlc -I ncurses --playlist-autostart")
 (% vl@ "vlc -I qt --playlist-autostart")
 (% lo "libreoffice")
 (% gpg "gpg2")
 (% fs "gtk2fontsel")
 (% dv "gdrive upload --recursive")
 (% v "less")
 (% f "fd")
 (% za "zathura")
 (% ev "evince")
 (% av "ahoviewer")
 (% zu "zoom-us")
 (% qct "qt5ct")
 (% qtx "qtox")
 (% eb "ebook-viewer")
 (% td "telegram-desktop")
 (% kp "keepassxc")
 (% kt "krita")
 (% rmd "qt-recordMyDesktop")
 (% sm "stellarium")
 (% sp "speedcrunch")
 (% earth "googleearth")
 (% vv "vncviewer")
 (% rl "rlwrap -s 1000000 -c -b \"(){}[].,=&^%$#@\\;|\"")
 (% zx "zsh -c")
 (% us "usync --one-way --prefer-local")
 (% ds "Discord"))

(exporting-definitions
 (% sicp@ "zathura /home/ebzzry/l/dok/sicp.pdf")
 (% lisp@ "zathura /home/ebzzry/l/dok/lisp.pdf"))

(exporting-definitions
 (@ xu "Xenu/Xenu.exe")
 (@ re "The Regex Coach/The Regex Coach.exe")
 (@ ni "Neat Image Standalone/NeatImage.exe")
 ($ ui "uninstaller"))

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

 (defun xmsg (&rest args)
   (run/i `("xmessage"
            "-fn" "-*-speedy-*-*-*-*-12-*-*-*-*-*-*-*"
            "-fg" "white" "-bg" "black"
            "-timeout" "2" "-buttons" ""
            ,@args))
   (success))

 (defun xrun (&rest args)
   (run/i `("gmrun" "-geometry" "+0+0"
            ,@args))
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
