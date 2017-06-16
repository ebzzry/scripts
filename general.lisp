(uiop:define-package
    :cl-scripts/general
    (:use
     :cl
     :fare-utils
     :uiop
     :inferior-shell
     :cl-scripting
     :cl-scripts/commands
     :optima
     :optima.ppcre
     :cl-ppcre
     :cl-launch/dispatch)
  (:export #:*char-mode*
           #:*colon-mode*
           #:*normal-mode*
           #:*num-mode*
           #:display-ascii-hex-table
           #:ascii
           #:display-ascii-oct-table
           #:rot13
           #:xrsync

           #:run-chrome
           #:chrome
           #:kill-chrome
           #:stop-chrome
           #:continue-chrome
           #:run-fox
           #:fox
           #:kill-fox
           #:stop-fox
           #:continue-fox

           #:suma
           #:lisp

           #:battery
           #:battery-status

           #:trackpoint

           #:xdev-id
           #:xdev
           #:xmap
           #:xmr
           #:askpass

           #:xxx))

(in-package :cl-scripts/general)

(defun char-display-char (c)
  (if (or (member c '(127 155))
          (< c 32)
          (<= 128 c 159))
      #\space
      (code-char c)))

(defun battery ()
  (let ((base-dir "/sys/class/power_supply/*")
        (exclude-string "/AC/"))
    (with-output (s nil)
      (loop
        :for dir :in (remove-if #'(lambda (path)
                                    (search exclude-string (native-namestring path)))
                                (directory* base-dir))
        :for battery = (first (last (pathname-directory dir)))
        :for capacity = (read-file-line (subpathname dir "capacity"))
        :for status = (read-file-line (subpathname dir "status"))
        :do (format s "~A: ~A% (~A)~%" battery capacity status)))))

(exporting-definitions
  (defvar *num-mode* "[31m")
  (defvar *colon-mode* "[34m")
  (defvar *char-mode* "[0m[1m")
  (defvar *normal-mode* "[0m")

  (defun display-ascii-hex-table ()
    (loop for i from 32 to 255
          do (format t "~A~X~A:~A~A~A~:[ ~;~%~]"
                     *num-mode* i
                     *colon-mode* *char-mode*
                     (char-display-char i)
                     *normal-mode*
                     (zerop (mod (1+ i) 16))))
    (success))

  (defun ascii () (display-ascii-hex-table))

  (defun display-ascii-oct-table ()
    (loop for i from 32 to 255
          do (format t "~A~3O~A~A~A~:[ ~;~%~]"
                     *num-mode* i
                     *char-mode*
                     (char-display-char i)
                     *normal-mode*
                     (zerop (mod (1+ i) 16))))
    (success))

  (defun rot13 (&rest args)
    (run/i `(tr "[a-zA-Z]" "[n-za-mN-ZA-M]" ,@args))
    (success))

  (defun xrsync (args)
    (run/i `(rsync "-rlptgoDHSx" ,@args)))

  (defun run-chrome (args)
    (run/i `(google-chrome-stable ,@args)))

  (defun chrome (&rest args)
    (run-chrome args))

  (defun kill-chrome (&rest args)
    (inferior-shell:run
     `(killall ,@args chromium-browser chromium google-chrome chrome)
     :output :interactive :input :interactive :error-output nil :on-error nil))

  (defun stop-chrome ()
    (kill-chrome "-STOP"))

  (defun continue-chrome ()
    (kill-chrome "-CONT"))

  (defun run-fox (args)
    (run/i `(firefox ,@args)))

  (defun fox (&rest args)
    (run-fox args))

  (defun kill-fox (&rest args)
    (inferior-shell:run
     `(killall ,@args firefox iceweasel)
     :output :interactive :input :interactive :error-output nil :on-error nil))

  (defun stop-fox ()
    (kill-fox "-STOP"))

  (defun continue-fox ()
    (kill-fox "-CONT"))

  (defun suma (&rest args)
    (run/i `(wine ,(subpathname (user-homedir-pathname) ".wine/drive_c/Program Files/SumatraPDF/SumatraPDF.exe") ,@args)))

  (defun lisp (&rest args)
    (run/i `(rlwrap "-s" "1000000" "-c" "-b" "(){}[].,=&^%$#@\\;|" "sbcl" ,@args)))

  (defun battery-status ()
    (format t "~A" (battery))
    (values))

  (defun askpass ()
    (run/i `(git gui--askpass))
    (values))

  (defun trackpoint (arg)
    (let ((device arg))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation" 1))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Button" 2))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Timeout" 200))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Axes" 7 6 5 4))
      (success)))

  (defun xdev-id (name type)
    (format nil "~A"
            (cl-ppcre:regex-replace
             (cl-ppcre:create-scanner ".*id=(.*?)	+.*")
             (first (remove-if (complement
                                #'(lambda (line)
                                    (and (search name line) (search (format nil "slave  ~A" type) line))))
                               (uiop:run-program '("xinput" "list") :output :lines))) "\\1")))

  (defun xdev (name type command &rest args)
    (let ((id (xdev-id name type)))
      (when (not (string= id "NIL"))
        (run/i `(xinput ,command ,(parse-integer id) ,@args))
        (success))))

  (defun xmap (keymap)
    (run/i `(setxkbmap dvorak))
    (run/i `(xset r rate 250))
    (run/i `(xmodmap ,(subpathname (user-homedir-pathname) (format nil "hejmo/ktp/xmodmap/.Xmodmap.~A" keymap))))
    (success))

  (defun xmr (device)
    (if (remove-if (complement #'(lambda (line) (search device line)))
                   (uiop:run-program '("lsusb") :output :lines))
        (xmap "kadv.dvorak")
        (xmap "tpad.dvorak"))
    (success))

  (defun xxx ()
    (cl-scripts/touchpad:disable)
    (xmr "Kinesis Advantage PRO MPC/USB Keyboard")
    (trackpoint "TPPS/2 IBM TrackPoint")
    (xdev "Logitech USB Receiver" "pointer" "set-button-map" "1" "2" "3" "5" "4")
    (success)))

(register-commands :cl-scripts/general)
