#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "scripts"
  :version "0.0.1"
  :description "Common Lisp scripts"
  :license "MIT"
  :author "Rommel Martinez"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
               (:version "inferior-shell" "2.0.3.3")
               (:version "fare-utils" "1.0.0.5")
               #:local-time
               #:ironclad
               #:net.didierverna.clon
               #:mof
               "scripts/misc"
               "scripts/touchpad"
               "scripts/general"
               "scripts/apps"
               "scripts/unix"
               "scripts/mksum"
               "scripts/intuos"))
