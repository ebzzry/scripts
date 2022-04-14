;;; scripts.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(uiop:define-package :scripts-system
  (:use #:cl #:asdf))

(in-package #:scripts-system)

(defsystem :scripts
  :name "scripts"
  :version "1.3.0"
  :description "Common Lisp scripts"
  :license "CC0"
  :author "Rommel MARTINEZ <rommel.martinez@astn-group.com>"
  :class :package-inferred-system
  :depends-on (#:cl-ppcre
               #:local-time
               #:ironclad
               #:inferior-shell
               "scripts/common"
               "scripts/misc"
               "scripts/touchpad"
               "scripts/general"
               "scripts/webcam"
               "scripts/apps"
               "scripts/unix"
               "scripts/mksum"
               "scripts/touchring"
               "scripts/main"
               "scripts/driver"))
