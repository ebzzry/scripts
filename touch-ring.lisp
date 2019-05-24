;;;; touch-ring.lisp

(uiop:define-package #:scripts/touch-ring
    (:use #:cl
          #:fare-utils
          #:inferior-shell
          #:cl-scripting
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/unix
          #:scripts/utils)
  (:export #:touch-ring-status
           #:touch-ring-map
           #:touch-ring-bind
           #:touch-ring-mode
           #:touch-ring-actions
           #:touch-ring-set))

(in-package #:scripts/touch-ring)

(defparameter *touch-ring-selector-key*
  "XF86Go"
  "The key to bind the selector key to.")

(defparameter *touch-ring-default-config*
  '((0 "button +4" "button +5")
    (1 "key PgUp" "key PgDn")
    (2 "key +ctrl - -ctrl" "key +ctrl +shift = -ctrl -shift")
    (3 "button +5" "button +4"))
  "The default configuration.")

(defparameter *touch-ring-default-config-file*
  (mof:home ".touch-ring.lisp")
  "The location of the config file on the disk")

(defun touch-ring-config-file-exists-p ()
  "Return true if the touch-ring config file exists."
  (when (uiop:file-exists-p *touch-ring-default-config-file*)
    t))

(defun touch-ring-read-config-file ()
  "Read the configuration file."
  (uiop:read-file-forms *touch-ring-default-config-file*))

(defun touch-ring-read-config ()
  "Return the most proximate configuration."
  (if (touch-ring-config-file-exists-p)
      (touch-ring-read-config-file)
      *touch-ring-default-config*))

(defun touch-ring-config-value (index)
  "Return the operations associated with an index."
  (let ((config (touch-ring-read-config)))
    (rest (assoc index config))))

(defun touch-ring-led-file ()
  "Returns the device for controlling the LED states of the ring."
  (first (directory #P"/sys/bus/usb/devices/*/*/wacom_led/status_led0_select")))

(defun touch-ring-device-name (type)
  "Return the name of the touch-ring by type NAME."
  (let* ((lines (inferior-shell:run/lines `(xsetwacom list devices)))
         (device (concatenate 'string "type: " (string-upcase type)))
         (line (first (remove-if-not #'(lambda (line) (search device line :test #'string=))
                                    lines))))
    (cl-ppcre:regex-replace "(^.*Pad pad).*"  line "\\1")))

(defun touch-ring-pad-name ()
  "Return the pad name of touch-ring detected."
  (touch-ring-device-name "pad"))

(exporting-definitions
  (defun touch-ring-status ()
    "Return the current value of the LED file."
    (let ((value (uiop:read-file-form (touch-ring-led-file))))
      value))

  (defun touch-ring-map (&rest args)
    "Bind a button using xsetwacom."
    (let ((name (touch-ring-pad-name)))
      (run/i `(xsetwacom "set" ,name ,@args))
      (success)))

  (defun touch-ring-bind (&optional (key *touch-ring-selector-key*))
    "Bind the middle selector key to the default value."
    (let ((value (mof:fmt "key ~A" key)))
      (touch-ring-map "Button" "1" value)))

  (defun touch-ring-mode (value)
    "Use sudo to set the value of the LED file."
    (let ((command (mof:fmt "echo ~A > ~A" value (touch-ring-led-file))))
      (sush command)))

  (defun touch-ring-actions (action-1 action-2)
    "Bind actions to the ring."
    (touch-ring-map "AbsWheelUp" action-1)
    (touch-ring-map "AbsWheelDown" action-2))

  (defun touch-ring-set (&optional mode)
    "Change the behavior of the ring depending on the current LED value."
    (when mode (touch-ring-mode mode))
    (apply #'touch-ring-actions (touch-ring-config-value (touch-ring-status)))
    (success)))

(register-commands :scripts/touch-ring)
