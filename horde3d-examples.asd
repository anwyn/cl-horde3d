 ;;; -*- lisp -*-

(defsystem :horde3d-examples
  :description "The Horde3d examples ported to Common Lisp."
  :long-description "The Horde3d examples ported to Common Lisp.
Run (horde3d-examples:knight) and (horde3d-examples:chicago)."
  :version "0.2"
  :author "Ole Arndt <anwyn@sugarshark.com>"
  :maintainer "Ole Arndt <anwyn@sugarshark.com>"
  :licence "EPL 1.0"
  :depends-on (:horde3d :lispbuilder-sdl)
  :components ((:static-file "horde3d-examples.asd")
               (:module "examples"
                        :components ((:file "package")
                                     (:file "examples" :depends-on ("package"))
                                     (:file "knight"   :depends-on ("examples"))
                                     (:file "chicago"  :depends-on ("examples"))))))


