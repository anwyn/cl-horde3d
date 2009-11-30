 ;;; -*- lisp -*-

(defsystem :horde3d-examples
  :description "The Horde3d examples ported to common lisp." 
  :long-description "The Horde3d examples ported to common lisp.
Run (horde3d-examples:knight) and (horde3d-examples:chicago)." 
  :version "0.1"
  :author "Ole Arndt <ole@sugarshark.com>"
  :maintainer "Ole Arndt <ole@sugarshark.com>"
  :licence "LGPL"
  :depends-on (:horde3d :lispbuilder-sdl)
  :components ((:static-file "horde3d-examples.asd")
               (:module "examples"
                        :components ((:file "package")
                                     (:file "examples" :depends-on ("package"))
                                     (:file "knight" :depends-on ("examples"))
                                     (:file "chicago" :depends-on ("examples"))))))


