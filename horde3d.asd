;;; -*- lisp -*-

;;; If Horde3d has been compiled with the sound or terrain extensions
;;; you can enable the respective bindings by pushing
;;; :horde3d-terrain-extension or :horde3d-terrain-extension to *features*
;;; before loading the system.
;;;
;; (push :horde3d-terrain-extension cl:*features*)
;; (push :horde3d-sound-extension cl:*features*)

(defsystem :horde3d
  :description "CFFI bindings for the Horde3D rendering engine."
  :long-description "CFFI bindings for the Horde3D rendering engine."
  :version "0.2"
  :author "Ole Arndt <anwyn@sugarshark.com>"
  :maintainer "Ole Arndt <anwyn@sugarshark.com>"
  :licence "EPL 1.0"
  :depends-on (:cffi)
  :in-order-to ((test-op (load-op :horde3d-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:horde3d-tests) '#:horde3d-test)))
  :components
  ((:doc-file "README.org")
   (:static-file "horde3d.asd")
   (:module "src"
    :components
    ((:file "bindings-package")
     (:file "libraries" :depends-on ("bindings-package"))
     (:file "types" :depends-on ("libraries"))
     #+horde3d-terrain-extension
     (:file "terrain-bindings" :depends-on ("types"))
     #+horde3d-sound-extension
     (:file "sound-bindings" :depends-on ("types"))
     (:file "enums" :depends-on ("types"
                                 #+horde3d-terrain-extension
                                 "terrain-bindings"
                                 #+horde3d-sound-extension
                                 "sound-bindings"))
     (:file "bindings" :depends-on ("enums"))
     ;; lispification
     (:file "package" :depends-on ("bindings-package"))
     (:file "horde3d" :depends-on ("package" "bindings"))
     #+horde3d-terrain-extension
     (:file "terrain" :depends-on ("horde3d"))
     #+horde3d-sound-extension
     (:file "sound"   :depends-on ("horde3d"))))))

(defsystem :horde3d-test
  :components ((:module "test"
                        :components ((:file "suite")
                                     (:file "horde3d" :depends-on ("suite")))))
  :depends-on (:horde3d :stefil))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :horde3d))))
  (values nil))
