(uiop:define-package
 :cl-scripts/misc
 (:use :cl
       :fare-utils
       :uiop
       :inferior-shell
       :cl-scripting
       :cl-scripts/commands
       :optima
       :optima.ppcre
       :cl-launch/dispatch)
 (:export #:*char-mode*
          #:*colon-mode*
          #:*normal-mode*
          #:*num-mode*
          #:char-display-char
          #:display-ascii-hex-table
          #:ascii
          #:display-ascii-oct-table
          #:rot13
          #:xrsync
          #:battery-status
          #:trackpoint
          #:run-chrome
          #:chrome
          #:kill-chrome
          #:stop-chrome
          #:continue-chrome))

(in-package :cl-scripts/misc)

(exporting-definitions
 (defun char-display-char (c)
   (if (or (member c '(127 155))
           (< c 32)
           (<= 128 c 159))
       #\space
       (code-char c)))

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

 ;; FIXME: loop over BAT*
 (defun battery-status ()
   (let* ((capacity (read-file-line "/sys/class/power_supply/BAT1/capacity"))
          (status (read-file-line "/sys/class/power_supply/BAT1/status")))
     (println (format nil "~A% (~A)" capacity status))))

 (defun trackpoint ()
   (run/i `(xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1))
   (run/i `(xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2))
   (run/i `(xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200))
   (run/i `(xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 7 6 5 4))
   (success))

 (defun run-chrome (args)
   (run/i `(google-chrome-stable "--disable-extensions" ,@args)))

 (defun chrome (args)
   (run-chrome args))

 (defun kill-chrome (&rest args)
   (inferior-shell:run
    `(killall ,@args chromium-browser chromium google-chrome chrome)
    :output :interactive :input :interactive :error-output nil :on-error nil))

 (defun stop-chrome ()
   (kill-chrome "-STOP"))

 (defun continue-chrome ()
   (kill-chrome "-CONT"))
)

(register-commands :cl-scripts/misc)
