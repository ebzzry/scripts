;;;; apps.lisp

(uiop:define-package #:scripts/apps
  (:use #:cl
        #:inferior-shell
        #:cl-scripting
        #:optima
        #:optima.ppcre
        #:marie
        #:scripts/common))

(in-package #:scripts/apps)

(defp +screenshots-dir+ (home ".screenshots"))

(% bt "bluetoothctl"
   em "emacs --daemon"
   par "parallel"
   pm "pulsemixer"
   rz "rsync -az"
   rl "rlwrap -s 1000000 -c -b \"(){}[].,=&^%$#@\\;|\""
   s "sudo"
   us "usync --one-way --prefer-local"
   vg "vagrant"
   xf "xmllint --format"
   zx "zsh -c")

(% av "ahoviewer"
   b "gqview"
   bs "inkview"
   ev "evince"
   fs "gtk2fontsel"
   lo "libreoffice"
   lx "lxappearance"
   ms "musescore"
   pc "pavucontrol"
   pe "pulseeffects"
   sm "steam"
   syn "flatpak run --branch=master --arch=x86_64 com.symless.Synergy"
   tx "termite"
   vbm "VBoxManage"
   vl "vlc -I ncurses"
   vl! "vl --random --loop --playlist-autostart"
   vr "viber"
   xmind "XMind"
   xo "xournal"
   xs "simple-scan"
   za "zathura")

(% bb "brave"
   cb "google-chrome-stable"
   fb "firefox"
   nb "nyxt"
   tb "tor-browser")

($ ce "calibre"
   dj "djview"
   eb "ebook-viewer"
   kd "len kdenlive"
   kp "keepassxc"
   kt "krita"
   qbt "qbittorrent"
   qeq "qpaeq"
   qt4 "qtconfig"
   qtx "qtox"
   qj "qjoypad --notray"
   rd "qt-recordMyDesktop"
   sw "Write"
   wire "sudo -Hi QT_QPA_PLATFORMTHEME=gtk2 wireshark"
   td "telegram-desktop"
   vb "VirtualBox"
   zm "zoom-us")

($$ vp "vlc"
    qb "qutebrowser"
    qt5 "qt5ct")

(@ fightcade "/pub/ludoj/emu/fightcade/FightCade.exe"
   ui "uninstaller")

(@+ ni "Neat Image Standalone/NeatImage.exe"
    xu "Xenu/Xenu.exe")

(defcommand lc (&rest args) (run-with-locale "C" args))
(defcommand len (&rest args) (run-with-locale "en_US.UTF-8" args))
(defcommand leo (&rest args) (run-with-locale "eo.utf8" args))

(defun paths (x y)
  "Merge path namestrings."
  (cat (uiop:native-namestring (expand-pathname x)) ":" y))

(defcommand tresorit ()
  (run-with-docker-x
   `("-v" ,(paths "~/.local/share/tresorit/Profiles" "/home/tresorit/.local/share/tresorit/Profiles")
          "-v" ,(paths "~/.local/share/tresorit/Logs" "/home/tresorit/.local/share/tresorit/Logs")
          "-v" ,(paths "~/.local/share/tresorit/Reports" "/home/tresorit/.local/share/tresorit/Reports")
          "-v" ,(paths "~/.local/share/tresorit/Temp" "/home/tresorit/.local/share/tresorit/Temp")
          "-v" ,(paths "~/.config/Tresorit" "/home/tresorit/.config/Tresorit")
          "-v" ,(paths "~/Tresors" "/home/tresorit/Tresors"))
   "tresorit"))

(defcommand tresorit-chroot (&rest args)
  (run-with-chroot (home ".local/share/tresorit/tresorit") args))

(defcommand viber ()
  (run-with-docker-x
   `("-v" ,(paths "~/.ViberPC/" "/root/.ViberPC/")
          "-v" ,(paths (xdg-dir "DESKTOP") "/root/Desktop/")
          "-v" ,(paths (xdg-dir "DOWNLOAD") "/root/Downloads/"))
   "viber"))

(defcommand rz@ (&rest args)
  (apply-args-1
   'rz args
   :options '("-e" "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"))
  (success))

(defcommand rz! (&rest args)
  (apply-args-1
   'rz args
   :options '("--no-p" "--no-o" "--no-g" "--no-t" "--no-D"))
  (success))

(defcommand screenshot (mode)
  (let* ((dir (uiop:truenamize +screenshots-dir+))
         (file (fmt "~A.png" (local-time:format-timestring nil (local-time:now))))
         (dest (fmt "mv $f ~A" dir))
         (image (fmt "~A~A" dir file)))
    (flet ((scrot (file dest &rest args)
             (run/i `("scrot" ,@args ,file -e ,dest))))
      (match mode
        ((ppcre "(full)") (scrot file dest))
        ((ppcre "(region)") (scrot file dest '-s))
        (_ (err (fmt "invalid mode ~A~%" mode))))
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
  (run/i `("sudo" "ds4drv" "--config" ,(expand-pathname "~/.config/ds4drv.conf")))
  (success))

(defun run-with-chroot (program args)
  "Run PROGRAM with ARGS inside the chroot."
  (run/i `("zsh" "-c" ,(cat "cr " (namestring program)) ,args))
  (success))

(defcommand kb (&rest args)
  (setf (uiop:getenv "NIX_SKIP_KEYBASE_CHECKS") "1")
  (run/i `("keybase-gui" ,@args))
  (success))

(defcommand steam! (&rest args)
  (setf (uiop:getenv "NIX_PATH") "nixpkgs=https://github.com/nixos/nixpkgs/archive/refs/heads/release-20.09.tar.gz")
  (run/i `("nix-shell" "-p" "steam" "--run" "steam"))
  (success))

(defcommand edraw (&rest args)
  (run-with-chroot "edrawmax" args))

(defcommand gu (&rest args)
  "Run guvcview with the default device."
  (let ((device (scripts/webcam:webcam "default-device")))
    (run/i `("guvcview" "-d" ,device ,@args))))

(defcommand p (&rest args)
            "Run the media player."
  (flet ((cmd (args)
           (uiop:os-cond
            ((uiop:os-macosx-p) (run/i `("iina" "--mpv-mute" ,@args)))
            (t (run/i `("mpv" "--mute" ,@args))))))
    (if (¬ args)
        (cmd '("."))
        (cmd args))))

(defcommand clhs (&rest args)
  "Open the Common Lisp Hyperspec in the browser."
  (run/i `(,(uiop:getenv "BROWSER")
            (fmt "https://www.xach.com/clhs?q=~A" ,@args)))
  (success))

(defcommand backlight (host value)
  "Set the backlight level of HOST to VALUE."
  (run/i `("ssh" ,host ,(fmt "sudo light -S ~A -s sysfs/backlight/intel_backlight" value))))

(defcommand resolution (mode)
  "Set the screen resolution to MODE."
  (run/i `("xrandr" "--output" "eDP-1" "--mode" ,mode)))

(register-commands :scripts/apps)
