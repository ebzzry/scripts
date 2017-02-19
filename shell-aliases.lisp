(uiop:define-package :cl-scripts/shell-aliases
    (:use :cl
          :fare-utils
          :uiop
          :inferior-shell
          :cl-scripting
          :cl-scripts/commands
          :optima
          :optima.ppcre
          :cl-launch/dispatch)
  (:export
   #:*char-mode*
   #:*colon-mode*
   #:*normal-mode*
   #:*num-mode*
   #:ascii
   #:batt
   #:battery-status
   #:char-display-char
   #:display-ascii-hex-table
   #:display-ascii-oct-table
   #:rot13))

(in-package :cl-scripts/shell-aliases)

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

 (defun rot13 ()
   (run/i '(tr "[a-zA-Z]" "[n-za-mN-ZA-M]"))
   (success))

 (defun xrsync (args)
   (run/i `(rsync "-rlptgoDHSx" ,@args)))

 (defun battery-status ()
   (let* ((capacity (read-file-line "/sys/class/power_supply/BAT1/capacity"))
          (status (read-file-line "/sys/class/power_supply/BAT1/status")))
     (format nil "~A% (~A)" capacity status)))

 (defun batt ()
   (println (battery-status))
   (values))
)

(register-commands :cl-scripts/shell-aliases)
