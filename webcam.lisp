;;;; webcam.lisp

(uiop:define-package #:scripts/webcam
  (:use #:cl
        #:inferior-shell
        #:cl-scripting
        #:scripts/unix
        #:marie))

(in-package #:scripts/webcam)

(defparameter +program+
  "v4l2-ctl"
  "The program name used to adjust webcam parameters.")

(defun webcam-devices ()
  "Return the list of v4l devices."
  (inferior-shell:run/lines `(,+program+ "--list-devices")))

(defparameter *webcam-regex*
  "^Logitech Webcam C930e"
  "The regular expression to use for matching the installed webcam.")

(defun webcam-device (regex)
  "Return the webcam device for REGEX."
  (let* ((devices (webcam-devices))
         (position (position-if (λ (entry)
                                  (cl-ppcre:scan regex entry))
                                devices)))
    (when position
      (string-trim '(#\Space #\Tab #\Newline)
                   (nth (1+ position) devices)))))

(defparameter *default-device*
  ;; (or (webcam-device *webcam-regex*) "/dev/video0")
  "/dev/video0"
  "The default webcam device.")

(defparameter *directory-wildcard*
  "/sys/bus/usb/devices/*/product"
  "The pathname wildcard for the USB devices.")

(defparameter +zoom-increments+
  10
  "The zoom increments.")

(defun default-device ()
  "Return the default webcam device."
  *default-device*)

(defun device-id (name)
  "Return the device ID of device with NAME."
  (let* ((files (directory *directory-wildcard*))
         (entry (first (remove-if-not (λ (device)
                                        (string-equal name (uiop:read-file-line device)))
                                      files)))
         (strings (remove-if #'empty-string-p
                             (cl-ppcre:split #\/
                                             (directory-namestring
                                              (uiop:native-namestring entry))))))
    (end strings)))

(defun run-command/i (device &rest args)
  (inferior-shell:run/i `(,+program+ "-d" ,device ,@args)))

(defun run-command/ss (device &rest args)
  (inferior-shell:run/ss `(,+program+ "-d" ,device ,@args)))

(defun current-zoom (&optional (device *default-device*))
  "Return the current zoom settings."
  (let* ((output (run-command/ss device "-C" "zoom_absolute"))
         (value (parse-integer (second (cl-ppcre:split #\space output)))))
    value))

(defun zoom-settings (&optional (device *default-device*))
  "Return the zoom settings from DEVICE."
  (uiop:split-string
   (first (remove-if-not (λ (line)
                           (search "zoom_absolute" line))
                         (inferior-shell:run/lines
                          `(,+program+ "-d" ,device "-l"))))
   :separator '(#\space)))

(defun zoom-value (type &optional (device *default-device*))
  "Return the current absolute zoom value for TYPE."
  (values
   (parse-integer
    (cl-ppcre:regex-replace
     (fmt "~A=(.*)" type)
     (first (remove-if-not (λ (text)
                             (search type text))
                           (zoom-settings device)))
     "\\1"))))

(defun get-default-zoom (&optional (device *default-device*))
  "Return the default zoom value."
  (zoom-value "default" device))

(defun get-minimum-zoom (&optional (device *default-device*))
  "Return the minimum zoom value for DEVICE."
  (zoom-value "min" device))

(defun get-maximum-zoom (&optional (device *default-device*))
  "Return the maximum zoom value for DEVICE."
  (zoom-value "max" device))

(defun get-integrated-camera-id ()
  "Return the ID of the integrated camera."
  (device-id "Integrated Camera"))

(defun get-usb-camera-id ()
  "Return the ID of the USB camera."
  (device-id "Logitech Webcam C930e"))

(defun enable-integrated-webcam ()
  "Enable the integrated webcam."
  (let* ((id (get-integrated-camera-id))
         (fmt (fmt "echo ~A > /sys/bus/usb/drivers/usb/bind" id)))
    (sush fmt)))

(defun disable-integrated-webcam ()
  "Disable the integrated webcam."
  (let* ((id (get-integrated-camera-id))
         (fmt (fmt "echo ~A > /sys/bus/usb/drivers/usb/unbind" id)))
    (sush fmt)))

(defun set-zoom (device value)
  "Set a specific zoom value."
  (run-command/i device "-c" (fmt "zoom_absolute=~A" value))
  (current-zoom device))

(defun reset-zoom (&optional (device *default-device*))
  "Set the zoom to the default vaule."
  (set-zoom device (get-default-zoom device))
  (current-zoom device))

(defun decrease-zoom (&optional (device *default-device*))
  "Decrease the zoom setting."
  (let* ((current (current-zoom device))
         (new (- current +zoom-increments+))
         (value (if (< new (get-minimum-zoom device))
                    (get-minimum-zoom device)
                    new)))
    (set-zoom device value)))

(defun increase-zoom (&optional (device *default-device*))
  "Decrease the zoom setting."
  (let* ((current (current-zoom device))
         (new (+ current +zoom-increments+))
         (value (if (> new (get-maximum-zoom device))
                    (get-maximum-zoom device)
                    new)))
    (set-zoom device value)))

(defun minimum-zoom (&optional (device *default-device*))
  "Set the zoom to the lowest setting."
  (set-zoom device (get-minimum-zoom device)))

(defun maximum-zoom (&optional (device *default-device*))
  "Set the zoom to the highest setting."
  (set-zoom device (get-maximum-zoom device)))

(def webcam (fn &rest args)
  "Apply matching FN to ARGS."
  (let ((symbol (intern (string-upcase fn) (find-package :scripts/webcam))))
    (when (fboundp symbol)
      (apply symbol args))))

(register-commands :scripts/webcam)
