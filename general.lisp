;;;; general.lisp

(uiop:define-package #:scripts/general
  (:use #:cl
        #:inferior-shell
        #:cl-scripting
        #:optima
        #:optima.ppcre
        #:scripts/common
        #:marie))

(in-package #:scripts/general)

(defvar *num-mode* "[31m")
(defvar *colon-mode* "[34m")
(defvar *char-mode* "[0m[1m")
(defvar *normal-mode* "[0m")

(defun xdev-id (name type)
  (fmt "~A"
       (cl-ppcre:regex-replace
        (cl-ppcre:create-scanner ".*id=(.*?)	+.*")
        (first (remove-if (complement
                           (λ (line)
                             (and (search name line) (search (fmt "slave  ~A" type) line))))
                          (uiop:run-program '("xinput" "list") :output :lines))) "\\1")))

(defun xdev (name type command &rest args)
  (let ((id (xdev-id name type)))
    (when (not (string= id "NIL"))
      (run/i `(xinput ,command ,(parse-integer id) ,@args))
      (success))))

(defun xmap (keymap)
  (run/i `("setxkbmap" "dvorak"))
  (run/i `("xset" "r" "rate" "250"))
  (run/i `("xmodmap" ,(home (fmt "hejmo/ktp/xmodmap/~A.xmap" keymap))))
  (success))

(defun trackpoint (arg)
  (let ((device arg))
    (run/i `("xinput" "set-prop" ,device "Evdev Wheel Emulation" 1))
    (run/i `("xinput" "set-prop" ,device "Evdev Wheel Emulation Button" 2))
    (run/i `("xinput" "set-prop" ,device "Evdev Wheel Emulation Timeout" 200))
    (run/i `("xinput" "set-prop" ,device "Evdev Wheel Emulation Axes" 6 7 4 5))
    (success)))

(defun load-keymap (&optional (device "Kinesis Advantage PRO MPC/USB Keyboard"))
  (if (remove-if (complement (λ (line) (search device line)))
                 (uiop:run-program '("lsusb") :output :lines))
      (xmap "advantage.dv")
      (if (string-equal (uiop:hostname) "vulpo")
          (xmap "thinkpad.dv")
          (xmap "aliaj.dv")))
  (success))

(defun load-xset ()
  (run/i `("xset" "-dpms"))
  (run/i `("xset" "m" "4" "2"))
  (run/i `("xset" "s" "off")))

(defun load-touchring ()
  (run/i `(touchring-bind))
  (dolist (cmd '(("2" "key +ctrl x -ctrl")
                 ("3" "key +ctrl c -ctrl")
                 ("8" "key +ctrl v -ctrl")
                 ("9" "key +ctrl a -ctrl")
                 ("10" "key +ctrl y -ctrl")
                 ("11" "key +ctrl z -ctrl")))
    (run/i (append (list "touchring-map" "Button") cmd))))

(defun load-resources ()
  (run `(xrdb ,(home ".Xresources"))
       :output :interactive
       :input :interactive
       :error-output nil
       :on-error nil)
  (success))

(defun load-hostname ()
  (let ((hostname (uiop:hostname))
        (xdev-args '("pointer" "set-button-map" "1" "2" "3" "5" "4")))
    (match hostname
      ((ppcre "vulpo")
       (scripts/touchpad:disable)
       (trackpoint "TPPS/2 IBM TrackPoint")
       (trackpoint "pointer:Lenovo ThinkPad Compact USB Keyboard with TrackPoint")
       (apply #'xdev (append '("Logitech USB Receiver") xdev-args)))
      ((ppcre "pando")
       (apply #'xdev (append '("Xornet gaming mouse") xdev-args)))
      (_ (success)))))

(defun load-pointer ()
  (run/i `("xsetroot" "-cursor_name" "left_ptr")))

(defun pgrep-lines (&rest args)
  (run/lines `(pgrep "--list-full" "--list-name" "--full" "--ignore-case" ,@args)))

(def ascii-hex-table ()
  (loop :for i :from 32 :to 255
        :do (format t "~A~X~A:~A~A~A~:[ ~;~%~]"
                    *num-mode* i
                    *colon-mode* *char-mode*
                    (char-display-char i)
                    *normal-mode*
                    (zerop (mod (1+ i) 16))))
  (success))

(def ascii-oct-table ()
  (loop :for i :from 32 :to 255
        :do (format t "~A~3O~A~A~A~:[ ~;~%~]"
                    *num-mode* i
                    *char-mode*
                    (char-display-char i)
                    *normal-mode*
                    (zerop (mod (1+ i) 16))))
  (success))

(def rot13 (&rest args)
  (run/i `(tr "[a-zA-Z]" "[n-za-mN-ZA-M]" ,@args))
  (success))

(def battery ()
  (format t "~A" (battery-status))
  (values))

(def (config-x x) ()
  (load-keymap)
  (load-xset)
  (load-resources)
  ;;(load-touchring)
  (load-hostname)
  (load-pointer)
  (success))

(def pg (&rest args)
  (run/i `(pgrep "--list-full" "--list-name" "--full" "--ignore-case" ,@args))
  (success))

(def pk (&rest args)
  (let ((numbers (mapcar #'string-first (pgrep-lines (last args)))))
    (loop :for number :in numbers
          :do (run/i `(kill ,@(butlast args) ,number))))
  (success))

(def pk! (&rest args)
  (apply-args-1 'pk args :options '("-9")))

(register-commands :scripts/general)
