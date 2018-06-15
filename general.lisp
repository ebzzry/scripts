;;;; general.lisp

(uiop:define-package #:scripts/general
    (:use #:cl
          #:fare-utils
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:optima
          #:optima.ppcre
          #:cl-ppcre
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/utils
          #:scripts/intuos)
  (:export #:ascii-hex-table
           #:ascii-oct-table
           #:rot13
           #:battery
           #:trackpoint
           #:xxx
           #:psg
           #:psk
           #:psk!))

(in-package :scripts/general)

(defvar *num-mode* "[31m")
(defvar *colon-mode* "[34m")
(defvar *char-mode* "[0m[1m")
(defvar *normal-mode* "[0m")

(defun xdev-id (name type)
  (format nil "~A"
          (regex-replace
           (create-scanner ".*id=(.*?)	+.*")
           (first (remove-if (complement
                              #'(lambda (line)
                                  (and (search name line) (search (format nil "slave  ~A" type) line))))
                             (run-program '("xinput" "list") :output :lines))) "\\1")))

(defun xdev (name type command &rest args)
  (let ((id (xdev-id name type)))
    (when (not (string= id "NIL"))
      (run/i `(xinput ,command ,(parse-integer id) ,@args))
      (success))))

(defun xmap (keymap)
  (run/i `(setxkbmap dvorak))
  (run/i `(xset r rate 250))
  (run/i `(xmodmap ,(home (format nil "hejmo/ktp/xmodmap/.Xmodmap.~A" keymap))))
  (success))

(defun load-xmodmap (device)
  (if (remove-if (complement #'(lambda (line) (search device line)))
                 (run-program '("lsusb") :output :lines))
      (xmap "advantage.dvorak")
      (if (string-equal (hostname) "vulpo")
          (xmap "thinkpad.dvorak")
          (xmap "ceteraj.dvorak")))
  (success))

(defun load-keymap ()
  (load-xmodmap "Kinesis Advantage PRO MPC/USB Keyboard"))

(defun load-intuos ()
  (run/i `(intuos-bind)))

(defun load-resources ()
  (run `(xrdb ,(home ".Xresources")) :output :interactive :input :interactive :error-output nil :on-error nil)
  (success))

(defun load-hostname ()
  (let ((hostname (hostname))
        (xdev-args '("pointer" "set-button-map" "1" "2" "3" "5" "4")))
    (match hostname
      ((ppcre "vulpo")
       (scripts/touchpad:disable)
       (trackpoint "TPPS/2 IBM TrackPoint")
       (apply #'xdev (append '("Logitech USB Receiver") xdev-args)))
      ((ppcre "pando")
       (apply #'xdev (append '("Xornet gaming mouse") xdev-args)))
      (_ (success)))))

(exporting-definitions
  (defun ascii-hex-table ()
    (loop :for i :from 32 :to 255
          :do (format t "~A~X~A:~A~A~A~:[ ~;~%~]"
                      *num-mode* i
                      *colon-mode* *char-mode*
                      (char-display-char i)
                      *normal-mode*
                      (zerop (mod (1+ i) 16))))
    (success))

  (defun ascii-oct-table ()
    (loop :for i :from 32 :to 255
          :do (format t "~A~3O~A~A~A~:[ ~;~%~]"
                      *num-mode* i
                      *char-mode*
                      (char-display-char i)
                      *normal-mode*
                      (zerop (mod (1+ i) 16))))
    (success))

  (defun rot13 (&rest args)
    (run/i `(tr "[a-zA-Z]" "[n-za-mN-ZA-M]" ,@args))
    (success))

  (defun battery ()
    (format t "~A" (battery-status))
    (values))

  (defun trackpoint (arg)
    (let ((device arg))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation" 1))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Button" 2))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Timeout" 200))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Axes" 7 6 5 4))
      (success)))

  (defun xxx ()
    (load-keymap)
    (load-resources)
    (load-intuos)
    (load-hostname)
    (success))

  (defun psg (&rest args)
    (run/i `(pgrep "--list-full" "--list-name" "--full" "--ignore-case" ,@args))
    (success))

  (defun psk (&rest args)
    (let ((numbers (mapcar #'string-first (psg-lines (last args)))))
      (loop :for number :in numbers :do (run/i `(kill ,@(butlast args) ,number))))
    (success))

  (defun psk! (&rest args)
    (apply-args-1 'psk args :options '("-9"))))

(register-commands :scripts/general)
