 ;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:horde3d.system)
    (defpackage #:horde3d.system
      (:use :common-lisp :asdf))))

(in-package #:horde3d.system)

(defsystem :horde3d-examples
  :description "The Horde3d examples ported to lisp." 
  :long-description "The Horde3d examples ported to lisp." 
  :version "0.1"
  :author "Ole Arndt <ole@sugarshark.com>"
  :maintainer "Ole Arndt <ole@sugarshark.com>"
  :licence "LGPL"
  :depends-on (:horde3d :lispbuilder-sdl)
  :components ((:static-file "horde3d-examples.asd")
               (:module "examples"
                        :components ((:file "examples")
                                     (:file "knight" :depends-on ("examples"))))))

