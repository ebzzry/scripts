#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :scripts-system
  (:use #:cl #:asdf))

(in-package #:scripts-system)

(defsystem :scripts
  :name "scripts"
  :version "0.0.1"
  :description "Common Lisp scripts"
  :license "CC0"
  :author "Rommel Martinez <ebzzry@ebzzry.io>"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
               (:version "inferior-shell" "2.0.3.3")
               (:version "fare-utils" "1.0.0.5")
               #:local-time
               #:ironclad
               #:net.didierverna.clon
               #:mof
               #:split-sequence
               "scripts/misc"
               "scripts/touchpad"
               "scripts/general"
               "scripts/apps"
               "scripts/unix"
               "scripts/mksum"
               "scripts/tablet"
               "scripts/webcam"))
