;;;; touchring.lisp

(uiop:define-package #:scripts/touchring
  (:use #:cl
        #:inferior-shell
        #:cl-scripting
        #:marie))

(in-package #:scripts/touchring)

(defparameter *touchring-selector-key*
  "XF86Go"
  "The key to bind the selector key to.")

(defparameter *touchring-default-config*
  '((0 "button +4" "button +5")
    (1 "key PgUp" "key PgDn")
    (2 "key +ctrl - -ctrl" "key +ctrl +shift = -ctrl -shift")
    (3 "button +5" "button +4"))
  "The default configuration.")

(defparameter *touchring-default-config-file*
  (home ".touchring.lisp")
  "The location of the config file on the disk")

(defun touchring-config-file-exists-p ()
  "Return true if the touchring config file exists."
  (when (uiop:file-exists-p *touchring-default-config-file*)
    t))

(defun touchring-read-config-file ()
  "Read the configuration file."
  (uiop:read-file-forms *touchring-default-config-file*))

(defun touchring-read-config ()
  "Return the most proximate configuration."
  (if (touchring-config-file-exists-p)
      (touchring-read-config-file)
      *touchring-default-config*))

(defun touchring-config-value (index)
  "Return the operations associated with an index."
  (let ((config (touchring-read-config)))
    (rest (assoc index config))))

(defun touchring-led-file ()
  "Returns the device for controlling the LED states of the ring."
  (first (directory #P"/sys/bus/usb/devices/*/*/wacom_led/status_led0_select")))

(defun touchring-device-name (type)
  "Return the name of the touchring by type NAME."
  (let* ((lines (inferior-shell:run/lines `(xsetwacom list devices)))
         (device (concatenate 'string "type: " (string-upcase type)))
         (line (first (remove-if-not #'(lambda (line) (search device line :test #'string=))
                                     lines))))
    (cl-ppcre:regex-replace "(^.*Pad pad).*"  line "\\1")))

(defun touchring-pad-name ()
  "Return the pad name of touchring detected."
  (touchring-device-name "pad"))

(defun* (touchring-status t) ()
  "Return the current value of the LED file."
  (let ((value (uiop:read-file-form (touchring-led-file))))
    value))

(defun* (touchring-map t) (&rest args)
  "Bind a button using xsetwacom."
  (let ((name (touchring-pad-name)))
    (run/i `(xsetwacom "set" ,name ,@args))
    (success)))

(defun* (touchring-bind t) (&optional (key *touchring-selector-key*))
  "Bind the middle selector key to the default value."
  (let ((value (fmt "key ~A" key)))
    (touchring-map "Button" "1" value)))

(defun* (touchring-mode t) (value)
  "Use sudo to set the value of the LED file."
  (let ((command (fmt "echo ~A > ~A" value (touchring-led-file))))
    (sush command)))

(defun* (touchring-actions t) (action-1 action-2)
  "Bind actions to the ring."
  (touchring-map "AbsWheelUp" action-1)
  (touchring-map "AbsWheelDown" action-2))

(defun* (touchring-set t) (&optional mode)
  "Change the behavior of the ring depending on the current LED value."
  (when mode (touchring-mode mode))
  (apply #'touchring-actions (touchring-config-value (touchring-status)))
  (success))

(register-commands :scripts/touchring)
