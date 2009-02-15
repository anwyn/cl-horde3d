;;; examples.lisp --- Examples of the standard horde3d distribution ported to Lisp
;;;                                 _           
;;;   _____  ____ _ _ __ ___  _ __ | | ___  ___ 
;;;  / _ \ \/ / _` | '_ ` _ \| '_ \| |/ _ \/ __|
;;; |  __/>  < (_| | | | | | | |_) | |  __/\__ \
;;;  \___/_/\_\__,_|_| |_| |_| .__/|_|\___||___/
;;;                          |_|                
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :cl-user)

(defpackage #:horde3d-examples
  (:use #:cl #:horde3d))

(in-package :horde3d-examples)

(defclass example-application ()
  ((%camera-node        :accessor camera-node        :initarg :camera-node)
   (%viewer-position    :accessor viewer-position    :initarg :viewer-position
                        :initform (make-array 3 :initial-element 0.0))
   (%viewer-orientation :accessor viewer-orientation :initarg :viewer-orientation
                        :initform (make-array 3 :initial-element 0.0))

   (%velocity           :accessor velocity           :initarg :velocity :initform 10.0)
   (%keys               :accessor keys               :initarg :keys)

   (%anim-time          :accessor anim-time          :initarg :anim-time           :initform 0.0)
   (%anim-weight        :accessor anim-weight        :initarg :anim-weight         :initform 1.0)
   (%curr-fps           :accessor curr-fps           :initarg :curr-fps            :initform 30.0)

   (%logo-resource      :accessor logo-resource      :initarg :logo-resource)
   (%font-resource      :accessor font-resource      :initarg :font-resource)
   
   (%content-path       :accessor content-path       :initarg :content-path)
   (%debug-view         :accessor show-debug-view?   :initarg :show-debug-view     :initform nil)
   (%wire-frame         :accessor show-wire-frame?   :initarg :show-wire-frame     :initform nil)
   (%freeze             :accessor freeze?            :initarg :freeze              :initform nil)
   (%show-stats         :accessor show-stats?        :initarg :show-stats          :initform nil)))

(defgeneric app-init (app))
(defgeneric app-release (app))
(defgeneric app-resize (app width height))
(defgeneric app-main-loop (app fps))

(defun get-real-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun example-main (app &key (width 800) (height 600) (caption "Horde3D example"))
  (sdl:with-init ()
    (sdl:window width height
                :bpp 32
                :flags sdl-cffi::sdl-opengl
                :title-caption caption
                :icon-caption caption)
    (app-init app)
    (app-resize app width height)
    (let ((t0 (get-real-time))
          (fps 30.0)
          (frames 0))
      (sdl:with-events ()
        (:quit-event ()
                     (app-release app)
                     t)
        (:idle ()
               (when (>= (incf frames) 3)
                 (let ((time (get-real-time)))
                   (setf fps (/ frames (- time t0)))
                   (setf frames 0.0)
                   (setf t0 time)))
               ;; render
               (app-main-loop app fps)
               ;; Start processing buffered OpenGL routines.
               (sdl-cffi::sdl-gl-swap-buffers))))))


(defmethod app-init :before ((app example-application))
  (h3d:init)
  (h3d:set-resource-path :scene-graph "models")
  (h3d:set-resource-path :geometry "models") 
  (h3d:set-resource-path :animation "models") 
  (h3d:set-resource-path :material "materials") 
  (h3d:set-resource-path :code "shaders") 
  (h3d:set-resource-path :shader "shaders") 
  (h3d:set-resource-path :texture-2D "textures") 
  (h3d:set-resource-path :texture-cube "textures") 
  (h3d:set-resource-path :effect "effects") 
  (h3d:set-resource-path :pipeline "pipelines"))

(defmethod app-release :after ((app example-application))
  (h3d:release))

(defmethod app-release ((app example-application))
  )

;;; examples.lisp ends here
