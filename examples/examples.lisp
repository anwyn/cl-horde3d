;;; examples.lisp --- Examples of the standard horde3d distribution ported to Lisp
;;;                                 _
;;;   _____  ____ _ _ __ ___  _ __ | | ___  ___
;;;  / _ \ \/ / _` | '_ ` _ \| '_ \| |/ _ \/ __|
;;; |  __/>  < (_| | | | | | | |_) | |  __/\__ \
;;;  \___/_/\_\__,_|_| |_| |_| .__/|_|\___||___/
;;;                          |_|
;;;
;;; Copyright (C) 2009 Ole Arndt <anwyn@sugarshark.com>
;;;


(in-package :horde3d-examples)

(defparameter *horde3d-home-directory*
  (asdf:system-relative-pathname (asdf:find-system :horde3d-examples)
                                 (make-pathname :directory '(:relative "Horde3D"))))

(defclass example-application ()
  ((viewer-position    :accessor viewer-position    :initarg  :viewer-position    :initform (make-array 3 :initial-element 0.0 :element-type '(or null single-float)))
   (viewer-orientation :accessor viewer-orientation :initarg  :viewer-orientation :initform (make-array 2 :initial-element 0.0 :element-type '(or null single-float)))
   (velocity           :accessor velocity           :initform 10.0)
   (keys               :accessor keys               :initform (make-hash-table))
   (modifiers          :accessor modifiers          :initform nil)

   (fullscreen         :accessor fullscreen?        :initarg  :fullscreen         :initform nil)
   (width              :accessor width              :initarg  :width)
   (height             :accessor height             :initarg  :height)

   (hdr-pipeline       :accessor hdr-pipeline       :initarg  :hdr-pipeline)
   (fwd-pipeline       :accessor fwd-pipeline       :initarg  :fwd-pipeline)

   (camera-node        :accessor camera-node        :initarg  :camera-node)

   (anim-time          :accessor anim-time          :initarg  :anim-time          :initform 0.0)
   (anim-weight        :accessor anim-weight        :initarg  :anim-weight        :initform 1.0)
   (curr-fps           :accessor curr-fps           :initarg  :curr-fps           :initform 30.0)

   (logo-resource      :accessor logo-resource      :initarg  :logo-resource)
   (font-resource      :accessor font-resource      :initarg  :font-resource)
   (panel-resource     :accessor panel-resource     :initarg  :panel-resource)

   (content-path       :accessor content-path       :initarg  :content-path)
   (debug-view         :accessor show-debug-view?   :initarg  :show-debug-view    :initform nil)
   (wire-frame         :accessor show-wire-frame?   :initarg  :show-wire-frame    :initform nil)
   (freeze             :accessor freeze?            :initarg  :freeze             :initform nil)
   (stat-mode          :accessor stat-mode          :initarg  :stat-mode          :initform 0))
  (:documentation "Base class for Horde3D Examples. Inits/releases Horde and handles basic
                   keys and mouse movement."))

(defgeneric app-init (app)
  (:method :before ((app example-application))
    (h3d:init)
    (h3d:set-option :load-textures 1)
    (h3d:set-option :texture-compression 0)
    (h3d:set-option :fast-animation 0)
    (h3d:set-option :max-anisotropy 4)
    (h3d:set-option :shadow-map-size 2048)
    (setf (fwd-pipeline app) (h3d:add-resource :pipeline "pipelines/forward.pipeline.xml" 0)
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
          (height app) height))
  (:method ((app example-application) width height)
    (let ((cam (camera-node app)))
      (setf (h3d:node-parameter cam :camera-viewport-x) 0
            (h3d:node-parameter cam :camera-viewport-y) 0
            (h3d:node-parameter cam :camera-viewport-width) width
            (h3d:node-parameter cam :camera-viewport-height) height)

      (h3d:setup-camera-view cam 45.0 (/ width height) 0.1 1000.0)
      (h3d:resize-pipeline-buffers (hdr-pipeline app) width height)
      (h3d:resize-pipeline-buffers (fwd-pipeline app) width height))))


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
      (if (eql (h3d:node-parameter cam :camera-pipeline-resource) (fwd-pipeline app))
          (setf (h3d:node-parameter cam :camera-pipeline-resource) (hdr-pipeline app))
          (setf (h3d:node-parameter cam :camera-pipeline-resource) (fwd-pipeline app)))))

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

(defgeneric app-mouse-move-event (app x y)
  (:method ((app example-application) x y)
    (declare (ignore app x y)))

  (:method :before ((app example-application) x y)
           (decf (aref (viewer-orientation app) 1)
                 (coerce (* 30 (/ x 100)) 'single-float))
           (incf (aref (viewer-orientation app) 0)
                   (coerce (max -90 (min 90 (* 30 (/ y 100)))) 'single-float))))

(declaim (inline degtorad))
(defun degtorad (angle)
  (coerce (* angle (/ pi 180.0)) 'single-float))

(declaim (inline radtodeg))
(defun radtodeg (angle)
  (coerce (* angle (/ 180.0 pi)) 'single-float))

(defun handle-movement (app)
  (let ((curr-vel (/ (velocity app) (curr-fps app))))
    (declare (type single-float curr-vel))
    (with-accessors ((mods modifiers)
                     (keys keys)
                     (pos viewer-position)
                     (rot viewer-orientation)) app

      (when (member :sdl-key-mod-lshift mods)
        (setf curr-vel (* 5 curr-vel)))

      (let ((w (gethash :sdl-key-w keys))
            (s (gethash :sdl-key-s keys)))
        (when (or w s)
          (let ((rx (degtorad (aref rot 0)))
                (ry (degtorad (aref rot 1))))
            (declare (type single-float rx ry))
            (when w
              (decf (aref pos 0) (coerce (* curr-vel (sin ry) (cos (- rx))) 'single-float))
              (decf (aref pos 1) (coerce (* curr-vel (sin (- rx))) 'single-float))
              (decf (aref pos 2) (coerce (* curr-vel (cos ry) (cos (- rx))) 'single-float)))
            (when s
              (incf (aref pos 0) (coerce (* curr-vel (sin ry) (cos (- rx))) 'single-float))
              (incf (aref pos 1) (coerce (* curr-vel (sin (- rx))) 'single-float))
              (incf (aref pos 2) (coerce (* curr-vel (cos ry) (cos (- rx))) 'single-float))))))

      (let ((a (gethash :sdl-key-a keys))
            (d (gethash :sdl-key-d keys)))
        (when (or a d)
          (let ((ry-90 (degtorad (- (aref rot 1) 90.0f0)))
                (ry+90 (degtorad (+ (aref rot 1) 90.0f0))))
            (declare (type single-float ry-90 ry+90))
            (when a
              (incf (aref pos 0) (coerce (* curr-vel (sin ry-90)) 'single-float))
              (incf (aref pos 2) (coerce (* curr-vel (cos ry-90)) 'single-float)))
            (when (gethash :sdl-key-d keys)
              (incf (aref pos 0) (coerce (* curr-vel (sin ry+90)) 'single-float))
              (incf (aref pos 2) (coerce (* curr-vel (cos ry+90)) 'single-float)))))))))


(defgeneric app-main-loop (app)
  (:method :before ((app example-application))
           (h3d:set-option :debug-view-mode (if (show-debug-view? app) 1 0))
           (h3d:set-option :wireframe-mode (if (show-wire-frame? app) 1 0))
           (handle-movement app))

  (:method :after ((app example-application))
           (h3d:finalize-frame)
           (h3d:clear-overlays)
           (h3d:dump-messages)))


(defgeneric toggle-fullscreen (app)
  (:documentation "Switch from windowed mode to fullscreen an vice versa.")
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
  "Main example function. This function contains the game loop. Call it with an
instance of a class derived from example-application."
  (sdl:with-init ()
    (sdl:window width height
                :bpp 32
                :flags sdl:sdl-opengl
                :title-caption caption
                :icon-caption caption)

    (app-init app)
    (app-resize app width height)

    (setf (sdl:frame-rate) 0)
    (sdl:enable-unicode)

    (let ((frames 0)
          (fps 100.0)
          (mx 0)
          (my 0)
          (m-init nil))
      (sdl:with-events ()
        ;; Redraw display
        (:video-expose-event ()
                             (sdl:update-display))

        ;; leave example
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
        (:key-down-event (:key key :mod-key mod)
                         (setf (modifiers app) mod)
                         (setf (gethash key (keys app)) t)
                         (app-key-press-event app key))

        ;; Key released
        (:key-up-event (:key key :mod-key mod)
                       (setf (modifiers app) mod)
                       (setf (gethash key (keys app)) nil)
                       (app-key-release-event app key))

        ;; Do work
        (:idle ()
               (when (>= (incf frames) 3)
                 (setf fps (max 100.0 (sdl:average-fps)))
                 (setf frames 0))
               (setf (curr-fps app) (coerce fps 'single-float))
               (with-simple-restart
                   (skip-game-loop "Skip game loop for this frame")
                 (app-main-loop app))
               ;; #+horde3d-debug
               ;; (with-simple-restart
               ;;     (skip-swank-request "Skip swank evaluation")
               ;;   (let ((connection
               ;;          (or swank::*emacs-connection* (swank::default-connection))))
               ;;     (swank::handle-requests connection t)))
               (sdl:update-display))))))

;;; examples.lisp ends here
