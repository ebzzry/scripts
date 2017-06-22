(uiop:define-package :cl-scripts/utils
    (:use :cl
          :uiop
          :inferior-shell
          :cl-scripting
          :fare-utils
          :cl-launch/dispatch)
  (:export #:char-display-char
           #:battery-status
           #:wine
           #:home))

(in-package :cl-scripts/utils)

(defun char-display-char (c)
  (if (or (member c '(127 155))
          (< c 32)
          (<= 128 c 159))
      #\space
      (code-char c)))

(defun battery-status ()
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

(defun wine (path &rest args)
  (run/i `(wine ,path ,@args)))

(defun home (path)
  (subpathname (user-homedir-pathname) path))
