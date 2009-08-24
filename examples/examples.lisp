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


(in-package :horde3d-examples)

(defclass example-application ()
  ((viewer-position    :accessor viewer-position    :initarg :viewer-position    :initform (make-array 3 :initial-element 0.0))
   (viewer-orientation :accessor viewer-orientation :initarg :viewer-orientation :initform (make-array 2 :initial-element 0.0))
   (velocity           :accessor velocity           :initarg :velocity           :initform 10.0)
   (keys               :accessor keys               :initarg :keys)

   (fullscreen         :accessor fullscreen?        :initarg :fullscreen         :initform nil)
   (width              :accessor width              :initarg :width)
   (height             :accessor height             :initarg :height)
   
   (hdr-pipeline       :accessor hdr-pipeline       :initarg :hdr-pipeline)
   (fwd-pipeline       :accessor fwd-pipeline       :initarg :fwd-pipeline)
   
   (camera-node        :accessor camera-node        :initarg :camera-node)


   (anim-time          :accessor anim-time          :initarg :anim-time          :initform 0.0)
   (anim-weight        :accessor anim-weight        :initarg :anim-weight        :initform 1.0)
   (curr-fps           :accessor curr-fps           :initarg :curr-fps           :initform 30.0)

   (logo-resource      :accessor logo-resource      :initarg :logo-resource)
   (font-resource      :accessor font-resource      :initarg :font-resource)
   (panel-resource     :accessor panel-resource     :initarg :panel-resource)
   
   (content-path       :accessor content-path       :initarg :content-path)
   (debug-view         :accessor show-debug-view?   :initarg :show-debug-view    :initform nil)
   (wire-frame         :accessor show-wire-frame?   :initarg :show-wire-frame    :initform nil)
   (freeze             :accessor freeze?            :initarg :freeze             :initform nil)
   (stat-mode          :accessor stat-mode          :initarg :stat-mode          :initform 0)))

(defgeneric app-init (app)
  (:method :before ((app example-application))
    (h3d:init)
    (h3d:set-options :load-textures 1
                     :tex-compression 0
                     :fast-animation 0
                     :max-anisotropy 4
                     :shadow-map-size 2048)
    (setf (hdr-pipeline app) (h3d:add-resource :pipeline "pipelines/hdr.pipeline.xml" 0)
          (fwd-pipeline app) (h3d:add-resource :pipeline "pipelines/forward.pipeline.xml" 0)
          (font-resource app) (h3d:add-resource :material "overlays/font.material.xml" 0)
          (panel-resource app) (h3d:add-resource :material "overlays/panel.material.xml" 0)
          (logo-resource app) (h3d:add-resource :material "overlays/logo.material.xml" 0))))

(defgeneric app-release (app)
  (:method ((app example-application))
    (declare (ignore app)))

  (:method :after ((app example-application))
           (declare (ignore app))
           (h3d:release)))

(defgeneric app-resize (app width height)
  (:documentation "Set window of app to new width and height.")

  (:method :after ((app example-application) width height)
           (setf (width app) width
                 (height app) height)))

(defgeneric app-key-press-event (app key)
  (:documentation "Key handler")
  
  (:method ((app example-application) key)
    (declare (ignore app key)))

  (:method ((app example-application) (key (eql :sdl-key-escape)))
    (sdl:push-quit-event))

  (:method ((app example-application) (key (eql :sdl-key-space)))
    (setf (freeze? app) (not (freeze? app))))

  (:method ((app example-application) (key (eql :sdl-key-f1)))
    (toggle-fullscreen app))
  
  (:method ((app example-application) (key (eql :sdl-key-f3)))
    (with-accessors ((cam camera-node)) app
      (if (eql (h3d:node-parameter cam :pipeline) (hdr-pipeline app))
          (setf (h3d:node-parameter cam :pipeline) (fwd-pipeline app))
          (setf (h3d:node-parameter cam :pipeline) (hdr-pipeline app)))))

  (:method ((app example-application) (key (eql :sdl-key-f7)))
    (setf (show-debug-view? app) (not (show-debug-view? app))))

  (:method ((app example-application) (key (eql :sdl-key-f8)))
    (setf (show-wire-frame? app) (not (show-wire-frame? app))))

  (:method ((app example-application) (key (eql :sdl-key-f9)))
    (when (> (incf (stat-mode app)) 2)
      (setf (stat-mode app) 0))))

(defgeneric app-key-release-event (app key)
  (:method ((app example-application) key)
    (declare (ignore app key))))

(defgeneric app-key-handler (app)
  (:method ((app example-application))
    (declare (ignore app))))


(defgeneric app-main-loop (app fps)
  (:method :before ((app example-application) fps)
           (assert (not (zerop fps)))
           (setf (curr-fps app) (coerce fps 'single-float))
           
           (h3d:set-option :debug-view-mode (if (show-debug-view? app) 1 0))
           (h3d:set-option :wireframe-mode (if (show-wire-frame? app) 1 0))
           (app-key-handler app))

  (:method :after ((app example-application) fps)
           (h3d:finalize-frame)
           (h3d:clear-overlays)
           (h3d:dump-messages)))

(defgeneric app-key-state-change (app key pressed?))

(defgeneric app-mouse-button-event (app x y button))


(defgeneric app-mouse-move-event (app x y)
  (:method ((app example-application) x y)
    (declare (ignore app x y)))

  (:method :before ((app example-application) x y)
           (decf (aref (viewer-orientation app) 1)
                 (coerce (* 30 (/ x 100)) 'single-float))
           (incf (aref (viewer-orientation app) 0)
                   (coerce (max -90 (min 90 (* 30 (/ y 100)))) 'single-float))))


(defgeneric toggle-fullscreen (app)
  (:method ((app example-application))
    (let ((width (width app))
          (height (height app)))
      (app-release app)
      (if (fullscreen? app)
          (sdl:resize-window width height :flags '(sdl:sdl-opengl sdl:sdl-resizable))
          (sdl:resize-window width height :flags '(sdl:sdl-opengl sdl:sdl-fullscreen)))
      (setf (fullscreen? app) (if (fullscreen? app) nil t))
      (app-init app)
      (app-resize app width height))))
    

(defun example-main-sdl (app &key (width 800) (height 600) (caption "Horde3D example"))
  (sdl:with-init ()
    (sdl:window width height
                :bpp 32
                :flags sdl:sdl-opengl
                :title-caption caption
                :icon-caption caption)
    (app-init app)
    (app-resize app width height)
    (setf (sdl:frame-rate) 0)
    (sdl:enable-unicode t)
    (let ((frames 0)
          (fps 100.0)
          (mx 0)
          (my 0)
          (m-init nil))
      (sdl:with-events ()
        (:quit-event () (app-release app) t)

        (:active-event (:gain gain)
                       (when (= gain 1)
                         (setf m-init nil)))

        (:mouse-motion-event (:x x :y y)
                             (when (null m-init)
                               (setf m-init t)
                               (setf mx x my y))
                             
                             (when (not (freeze? app))
                               (app-mouse-move-event app (- x mx) (- y my)))

                             (setf mx x my y))

        ;; Key pressed
        (:key-down-event (:key key) (app-key-press-event app key))

        ;; Key released
        (:key-up-event (:key key) (app-key-release-event app key))

        ;; Redraw display
	(:video-expose-event () (sdl:update-display))

        ;; Do work
        (:idle ()
               (when (>= (incf frames) 3)
                 (setf fps (max 100.0 (sdl:average-fps)))
                 (setf frames 0))
               (app-main-loop app fps)
               (sdl:update-display))))))


(defun get-real-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))


(defun example-main-glfw (app &key (width 800) (height 600) (caption "Horde3D example"))
  (let ((t0 (get-real-time))
        (fps 30.0)
        (frames 0))
    (glfw:do-window (:title caption :width width :height height
                            :redbits 8 :greenbits 8 :bluebits 8 :alphabits 8
                            :depthbits 24 :stencilbits 8)
        ((app-init app)
         (app-resize app width height)
         (push (lambda ()
                 (app-release app))
               glfw:*terminate-hooks*))
      ;; render
      (when (>= (incf frames) 3)
        (let* ((time (get-real-time))
               (dt (- time t0)))
          (when (/= dt 0.0)
            (setf fps (/ frames dt))
            (setf frames 0.0)
            (setf t0 time))))
      (app-main-loop app fps))))


;;; examples.lisp ends here
