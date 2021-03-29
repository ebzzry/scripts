#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :scripts-system
  (:use #:cl #:asdf))

(in-package #:scripts-system)

(defsystem :scripts
  :name "scripts"
  :version "1.2.0"
  :description "Common Lisp scripts"
  :license "CC0"
  :author "Rommel MARTINEZ <rom@mimix.io>"
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
