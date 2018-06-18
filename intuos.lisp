;;;; intuos.lisp

(uiop:define-package #:scripts/intuos
    (:use #:cl
          #:fare-utils
          #:inferior-shell
          #:cl-scripting
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/unix)
  (:export #:intuos-status
           #:intuos-map
           #:intuos-bind
           #:intuos-mode
           #:intuos-actions
           #:intuos-ring))

(in-package :scripts/intuos)

(defparameter *intuos-selector-key*
  "F10"
  "The key to bind the selector key to")

(defun intuos-led-file ()
  "Returns the device for controlling the LED states of the ring"
  (first (directory #P"/sys/bus/usb/devices/*/*/wacom_led/status_led0_select")))

(defun intuos-device-name (type)
  "Return the name of the tablet by type NAME"
  (let* ((lines (inferior-shell:run/lines `(xsetwacom list devices)))
         (device (concatenate 'string "type: " (string-upcase type)))
         (line (first (remove-if-not #'(lambda (line) (search device line :test #'string=))
                                    lines))))
    (cl-ppcre:regex-replace "(^.*Pad pad).*"  line "\\1")))

(defun intuos-pad-name ()
  "Return the pad name of tablet detected"
  (intuos-device-name "pad"))

(defun intuos-ring-status ()
  "Return the current value of the LED file"
  (let ((value (uiop:read-file-form (intuos-led-file))))
    value))

(exporting-definitions
  (defun intuos-status ()
    (format t "~A~%" (intuos-ring-status))
    (success))

  (defun intuos-map (&rest args)
    "Bind a button using xsetwacom"
    (let ((name (intuos-pad-name)))
      (run/i `(xsetwacom "set" ,name ,@args))
      (success)))

  (defun intuos-bind ()
    "Bind the middle selector key to the default value"
    (let ((key (format nil "key ~A" *intuos-selector-key*)))
      (intuos-map "Button" "1" key)))

  (defun intuos-mode (value)
    "Use sudo to set the value of the LED file"
    (let ((command (format nil "echo ~A > ~A" value (intuos-led-file))))
      (sush command)))

  (defun intuos-actions (action-1 action-2)
    "Bind actions to the ring"
    (intuos-map "AbsWheelUp" action-1)
    (intuos-map "AbsWheelDown" action-2))

  (defun intuos-ring (&optional mode)
    "Change the behavior of the ring depending on the current LED value"
    (let ((name (intuos-pad-name)))
      (when mode
        (intuos-mode mode))
      (ecase (intuos-ring-status)
        ;; scroll
        (0 (intuos-actions "button +4" "button +5"))

        ;; miscellany
        (1 (intuos-actions "key PgUp" "key PgDn"))

        ;; zoom
        (2 (intuos-actions "key +ctrl - -ctrl" "key +ctrl +shift = -ctrl -shift"))

        ;; brushes
        (3 (intuos-actions "key [" "key ]"))))
    (success)))

(register-commands :scripts/intuos)
