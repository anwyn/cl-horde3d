;;; gen-binding.lisp --- generate the horde3d low level bindings
;;;                         _     _           _ _             
;;;   __ _  ___ _ __       | |__ (_)_ __   __| (_)_ __   __ _ 
;;;  / _` |/ _ \ '_ \ _____| '_ \| | '_ \ / _` | | '_ \ / _` |
;;; | (_| |  __/ | | |_____| |_) | | | | | (_| | | | | | (_| |
;;;  \__, |\___|_| |_|     |_.__/|_|_| |_|\__,_|_|_| |_|\__, |
;;;  |___/                                              |___/ 
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :verrazano))

(in-package :verrazano-user)

(defun generate-binding* (name headers &rest args
                          &key (working-directory (verrazano::system-relative-pathname
                                                   :horde3d "src/"))
                          (gccxml-flags "-I/usr/include")
                          &allow-other-keys)
  (format *debug-io* "~%~%; *** Processing binding ~S~%" name)
  (remove-from-plistf args :working-directory :gccxml-flags)
  (block try
    (handler-bind ((serious-condition
                    (lambda (error)
                      (warn "Failed to generated binding for ~S, error: ~A" name error)
                      (return-from try))))
      (let ((*print-right-margin* 100))
        (generate-binding (append
                           (list :cffi
                                 :package-name name
                                 :input-files headers
                                 :working-directory working-directory
                                 :gccxml-flags gccxml-flags)
                           args)
                          :keep-temporary-files nil))))
  (values))


(defun generate-opengl-binding ()
  (generate-binding*
   :opengl-cffi-bindings
   '("GL/gl.h"
     "GL/glu.h"
     "GL/glut.h")
   :standard-name-transformer-replacements
   ;; transform "3DFoo" patterns (in names like "gluBuild3DMipmaps" => glu-build-3d-mipmaps)
   `(,(cl-ppcre:create-scanner "([a-z][1-3]D)([A-Z])")
      ,(lambda (original start end match-start match-end reg-starts reg-ends)
               (declare (ignore start end match-start match-end))
               (flet ((match-group (position)
                        (subseq original (elt reg-starts position) (elt reg-ends position))))
                 (concatenate 'string
                              (string-downcase (match-group 0))
                              "-"
                              (string-downcase (match-group 1))))))))


(defun generate-openal-binding ()
  (generate-binding*
   :openal-cffi-bindings
   '("AL/al.h"
     "AL/alut.h"
     "AL/alc.h")
   #+win32 :gccxml-flags #+win32 "--gccxml-compiler bcc32 -IC:\\openal\\include\\"))

(defun generate-horde3d-binding ()
  (generate-binding*
   :horde3d-binding
   '("Horde3D.h"
     "Horde3DUtils.h")
   :gccxml-node-types-to-output '(gccxml:typedef)
   #+unix :gccxml-flags #+unix "-I/home/ole/src/graphics/Horde3D/Horde3D/Bindings/C++/"))

;;; gen-binding.lisp ends here
