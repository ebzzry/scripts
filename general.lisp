(uiop:define-package
    :cl-scripts/general
    (:use
     :cl
     :fare-utils
     :uiop
     :inferior-shell
     :cl-scripting
     :optima
     :optima.ppcre
     :cl-ppcre
     :cl-launch/dispatch
     :cl-scripts/misc
     :cl-scripts/utils)
  (:export #:*char-mode*
           #:*colon-mode*
           #:*normal-mode*
           #:*num-mode*

           #:display-ascii-hex-table
           #:ascii
           #:display-ascii-oct-table
           #:rot13
           #:xrsync

           #:run-chrome
           #:chrome
           #:kill-chrome
           #:stop-chrome
           #:continue-chrome

           #:suma
           #:lisp-lisp
           #:battery
           #:trackpoint
           #:xdev-id
           #:xdev
           #:xmap
           #:xmr
           #:askpass
           #:xxx))

(in-package :cl-scripts/general)

(exporting-definitions
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

  (defun rot13 (&rest args)
    (run/i `(tr "[a-zA-Z]" "[n-za-mN-ZA-M]" ,@args))
    (success))

  (defun xrsync (&rest args)
    (run/i `(rsync "-rlptgoDHSx" ,@args))
    (success))

  (defun run-chrome (args)
    (run/i `(google-chrome-stable ,@args)))

  (defun chrome (&rest args)
    (run-chrome args))

  (defun kill-chrome (&rest args)
    (inferior-shell:run
     `(killall ,@args chromium-browser chromium google-chrome chrome)
     :output :interactive :input :interactive :error-output nil :on-error nil))

  (defun stop-chrome ()
    (kill-chrome "-STOP"))

  (defun continue-chrome ()
    (kill-chrome "-CONT"))

  (defun suma (&rest args)
    (run/nil `(wine ,(home ".wine/drive_c/Program Files/SumatraPDF/SumatraPDF.exe") ,@args) :on-error nil)
    (success))

  ;; Farenda,C<(Bo:
  ;; - akiri loko de programo
  (defun lisp-lisp (&rest args)
    (let* ((arguments (mapcar #'(lambda (s) (format nil "\'~A\'" s)) args))
           (list-args (append '("sbcl") arguments))
           (string-args (format nil "~{~a~^ ~}" list-args))
           (dir (home "hejmo/fkd/sxelo/lisp")))
      ;; (format t "~A~%" (uiop:argv0))
      ;; (format t "~A~%" (uiop:raw-command-line-arguments))
      (uiop:chdir dir)
      (run/i `(nix-shell --pure --command ,string-args))
      (success)))

  (defun battery ()
    (format t "~A" (battery-status))
    (values))

  (defun askpass ()
    (run/i `(git gui--askpass))
    (values))

  (defun trackpoint (arg)
    (let ((device arg))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation" 1))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Button" 2))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Timeout" 200))
      (run/i `(xinput set-prop ,device "Evdev Wheel Emulation Axes" 7 6 5 4))
      (success)))

  (defun xdev-id (name type)
    (format nil "~A"
            (cl-ppcre:regex-replace
             (cl-ppcre:create-scanner ".*id=(.*?)	+.*")
             (first (remove-if (complement
                                #'(lambda (line)
                                    (and (search name line) (search (format nil "slave  ~A" type) line))))
                               (uiop:run-program '("xinput" "list") :output :lines))) "\\1")))

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

  (defun xmr (device)
    (if (remove-if (complement #'(lambda (line) (search device line)))
                   (uiop:run-program '("lsusb") :output :lines))
        (xmap "kadv.dvorak")
        (if (string-equal (uiop:hostname) "vulpo")
            (xmap "tpad.dvorak")
            (xmap "mik.dvorak")))
    (success))

  (defun xxx ()
    (let ((hostname (uiop:hostname))
          (xdev-args '("pointer" "set-button-map" "1" "2" "3" "5" "4")))
      (xmr "Kinesis Advantage PRO MPC/USB Keyboard")
      (cond ((string= hostname "vulpo")
             (cl-scripts/touchpad:disable)
             (trackpoint "TPPS/2 IBM TrackPoint")
             (apply #'xdev (append '("Logitech USB Receiver") xdev-args)))
            ((string= hostname "pando")
             (apply #'xdev (append '("Xornet gaming mouse") xdev-args)))
            (t (success))))))

(register-commands :cl-scripts/general)
