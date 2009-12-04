;;; -*- lisp -*-

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
                    (funcall (intern (string '#:horde3d-tests) '#:horde3d-test)))
  :components
  ((:doc-file "README.org")
   (:doc-file "README.txt")
   (:static-file "horde3d.asd")
   (:module "src"
            :components
            ((:file "bindings-package")
             (:file "libraries" :depends-on ("bindings-package"))
             (:file "types" :depends-on ("libraries"))
             (:file "terrain-bindings" :depends-on ("types"))
             (:file "sound-bindings" :depends-on ("types"))
             (:file "enums" :depends-on ("types" "terrain-bindings" "sound-bindings"))
             (:file "bindings" :depends-on ("enums"))
             ;; lispification
             (:file "package" :depends-on ("bindings-package"))
             (:file "horde3d" :depends-on ("package" "bindings"))
             (:file "terrain" :depends-on ("horde3d"))
             (:file "sound"   :depends-on ("horde3d"))
             ))))

(defsystem :horde3d-test
  :components ((:module "test"
                        :components ((:file "suite")
                                     (:file "horde3d" :depends-on ("suite")))))
  :depends-on (:horde3d :stefil))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :horde3d))))
  (values nil))
