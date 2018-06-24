;;;; tablet.lisp

(uiop:define-package #:scripts/tablet
    (:use #:cl
          #:fare-utils
          #:inferior-shell
          #:cl-scripting
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/unix
          #:scripts/utils)
  (:export #:intuos-ring-status
           #:intuos-status
           #:intuos-map
           #:intuos-bind
           #:intuos-mode
           #:intuos-actions
           #:intuos-ring))

(in-package #:scripts/tablet)

(defparameter *intuos-selector-key*
  "Pause"
  "The key to bind the selector key to.")

(defparameter *intuos-default-config*
  '((0 "button +4" "button +5")
    (1 "key PgUp" "key PgDn")
    (2 "key +ctrl - -ctrl" "key +ctrl +shift = -ctrl -shift")
    (3 "button +5" "button +4"))
  "The default configuration.")

(defparameter *intuos-default-config-file*
  (home ".intuos.lisp")
  "The location of the config file on the disk")

(defun intuos-config-file-exists-p ()
  "Return true if the intuos config file exists."
  (when (uiop:file-exists-p *intuos-default-config-file*)
    t))

(defun intuos-read-config-file ()
  "Read the configuration file."
  (uiop:read-file-forms *intuos-default-config-file*))

(defun intuos-read-config ()
  "Return the most proximate configuration."
  (if (intuos-config-file-exists-p)
      (intuos-read-config-file)
      *intuos-default-config*))

(defun intuos-config-value (index)
  "Return the operations associated with an index."
  (let ((config (intuos-read-config)))
    (rest (assoc index config))))

(defun intuos-led-file ()
  "Returns the device for controlling the LED states of the ring."
  (first (directory #P"/sys/bus/usb/devices/*/*/wacom_led/status_led0_select")))

(defun intuos-device-name (type)
  "Return the name of the tablet by type NAME."
  (let* ((lines (inferior-shell:run/lines `(xsetwacom list devices)))
         (device (concatenate 'string "type: " (string-upcase type)))
         (line (first (remove-if-not #'(lambda (line) (search device line :test #'string=))
                                    lines))))
    (cl-ppcre:regex-replace "(^.*Pad pad).*"  line "\\1")))

(defun intuos-pad-name ()
  "Return the pad name of tablet detected."
  (intuos-device-name "pad"))

(exporting-definitions
  (defun intuos-ring-status ()
    "Return the current value of the LED file."
    (let ((value (uiop:read-file-form (intuos-led-file))))
      value))

  (defun intuos-status ()
    (format t "~A~%" (intuos-ring-status))
    (success))

  (defun intuos-map (&rest args)
    "Bind a button using xsetwacom."
    (let ((name (intuos-pad-name)))
      (run/i `(xsetwacom "set" ,name ,@args))
      (success)))

  (defun intuos-bind (&optional (key *intuos-selector-key*))
    "Bind the middle selector key to the default value."
    (let ((value (mof:fmt "key ~A" key)))
      (intuos-map "Button" "1" value)))

  (defun intuos-mode (value)
    "Use sudo to set the value of the LED file."
    (let ((command (mof:fmt "echo ~A > ~A" value (intuos-led-file))))
      (sush command)))

  (defun intuos-actions (action-1 action-2)
    "Bind actions to the ring."
    (intuos-map "AbsWheelUp" action-1)
    (intuos-map "AbsWheelDown" action-2))

  (defun intuos-ring (&optional mode)
    "Change the behavior of the ring depending on the current LED value."
    (when mode (intuos-mode mode))
    (apply #'intuos-actions (intuos-config-value (intuos-ring-status)))
    (success)))

(register-commands :scripts/tablet)
