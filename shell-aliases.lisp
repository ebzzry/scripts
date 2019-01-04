;; Slowly migrating my zsh aliases here...

(uiop:define-package :fare-scripts/shell-aliases
  (:use :cl :fare-utils :uiop
   :inferior-shell :cl-scripting :fare-scripts/commands
   :optima :optima.ppcre
   :cl-launch/dispatch)
  (:export
   ))

(in-package :fare-scripts/shell-aliases)

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

(defun snd-jack ()
  (run/i `(pasuspender -- jack_control start)))

(defun snd-jackd () ;; another way to start...
  (run/i `(pasuspender -- jackd "-R" "-P4" -dalsa -r44100 -p512 -n4 "-D" "-Chw:PCH" "-Phw:PCH")))

(defun snd-pulse ()
  (run/i `(jack_control exit) :on-error nil))

(defun snd-nojack ()
  (run/i `(killall jackd) :on-error nil))

(defun kill-chrome (&rest args)
  (inferior-shell:run `(killall ,@args chromium google-chrome chrome)
       :output :interactive :input :interactive :error-output nil :on-error nil))

(defun stop-chrome ()
  (kill-chrome "-STOP"))

(defun continue-chrome ()
  (kill-chrome "-CONT"))
);exporting-definitions

(register-commands :fare-scripts/shell-aliases)
