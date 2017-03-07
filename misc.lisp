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
          #:display-ascii-hex-table
          #:ascii
          #:display-ascii-oct-table
          #:rot13
          #:xrsync
          #:battery
          #:battery-status
          #:trackpoint
          #:run-chrome
          #:chrome
          #:kill-chrome
          #:stop-chrome
          #:continue-chrome
          #:run-fox
          #:fox
          #:kill-fox
          #:stop-fox
          #:continue-fox))

(in-package :cl-scripts/misc)

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

 (defun battery-status ()
   (format t "~A" (battery))
   (values))

 (defun trackpoint (arg)
   (let ((device arg))
     (run/i `(xinput set-prop ,device "Evdev Wheel Emulation" 1))
     (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Button" 2))
     (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Timeout" 200))
     (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Axes" 7 6 5 4))
     (success)))

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
)

(register-commands :cl-scripts/misc)
