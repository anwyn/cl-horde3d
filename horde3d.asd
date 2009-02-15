 ;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:horde3d.system)
    (defpackage #:horde3d.system
      (:use :common-lisp :asdf))))

(in-package #:horde3d.system)

(defsystem :horde3d 
  :description "CFFI bindings for the Horde3D rendering engine." 
  :long-description "CFFI bindings for the Horde3D rendering engine." 
  :version "0.1"
  :author "Ole Arndt <ole@sugarshark.com>"
  :maintainer "Ole Arndt <ole@sugarshark.com>"
  :licence "LGPL"
  :depends-on (:cffi)
  :in-order-to ((test-op (load-op :horde3d-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:run!) '#:it.bese.FiveAM) :horde3d))
  :components
  ((:doc-file "README")
   (:static-file "horde3d.asd")
   (:module "src"
            :components
            ((:file "bindings-package")
             (:file "libraries" :depends-on ("bindings-package"))
             (:file "bindings" :depends-on ("libraries"))
             ;; lispification
             (:file "packages" :depends-on ("bindings-package"))
             (:file "horde3d" :depends-on ("packages"))))))

(defsystem :horde3d-test
  :components ((:module "test"
                        :components ((:file "suite")
                                     (:file "horde3d" :depends-on ("suite")))))
  :depends-on (:horde3d :fiveam))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :horde3d))))
  (values nil))
