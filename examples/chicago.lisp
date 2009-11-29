;;; chicago.lisp --- horde3d example
;;;       _     _                       
;;;   ___| |__ (_) ___ __ _  __ _  ___  
;;;  / __| '_ \| |/ __/ _` |/ _` |/ _ \ 
;;; | (__| | | | | (_| (_| | (_| | (_) |
;;;  \___|_| |_|_|\___\__,_|\__, |\___/ 
;;;                         |___/       
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :horde3d-examples)

(defclass particle ()
  ((px        :accessor px        :initarg :px)
   (pz        :accessor pz        :initarg :pz)
   (dx        :accessor dx        :initarg :dx)
   (dz        :accessor dz        :initarg :dz)
   (fx        :accessor fx        :initarg :fx)
   (fz        :accessor fz        :initarg :fz)
   (ox        :accessor ox        :initarg :ox :initform 0.0)
   (oz        :accessor oz        :initarg :oz :initform 0.0)
   (node      :accessor node      :initarg :node :initform nil)
   (anim-time :accessor anim-time :initarg :anim-time :initform 0.0)))

(defclass chicago-application (example-application)
  ((deferred-pipeline  :accessor deferred-pipeline  :initarg  :deferred-pipeline)
   (particles :accessor particles :initarg :particles
              :initform (make-array 100 :element-type '(or particle null)))))

(defun chicago ()
  (example-main-sdl
   (make-instance 'chicago-application
                  :viewer-position (make-array 3 :element-type 'single-float
                                               :initial-contents '(5.0 3.0 19.0))
                  :viewer-orientation (make-array 3 :element-type 'single-float
                                                  :initial-contents '(7.0 15.0 0.0))
                  :stat-mode 2
                  :content-path #p"/home/ole/src/graphics//Horde3D_SDK_1.0.0_Beta4/Horde3D/Binaries/Content/")
   :width 800
   :height 600
   :caption "Chicago - Horde3D Sample"))

(defun choose-destination (particle)
  (let ((ang (* 6.28 (/ (random 360.0) 360.0)))
        (rad (random 20.0)))
    (setf (dx particle) (* (sin ang) rad)
          (dz particle) (* (cos ang) rad))))

(defmethod app-init ((app chicago-application))
  
  ;; Add resources
  (let ((env-res (h3d:add-resource :scene-graph "models/platform/platform.scene.xml"))
        (skybox-res (h3d:add-resource :scene-graph "models/skybox/skybox.scene.xml"))
        (light-mat-res (h3d:add-resource :material "materials/light.material.xml"))
        (deferred-pipeline (h3d:add-resource :pipeline "pipelines/deferred.pipeline.xml"))
        (char-res (h3d:add-resource :scene-graph "models/man/man.scene.xml"))
        (char-walk-res (h3d:add-resource :animation "animations/man.anim")))

    (setf (hdr-pipeline app) deferred-pipeline)
    
    ;; Load resources
    (h3d:load-resources-from-disk (namestring (content-path app)))

    ;; add camera
    (setf (camera-node app)
          (h3d:add-camera-node h3d:+root-node+ "Camera" deferred-pipeline))

    (h3d:set-node-transform (h3d:add-nodes h3d:+root-node+ env-res)
                            0 0 0 0 0 0 0.23 0.23 0.23)
    (h3d:set-node-transform (h3d:add-nodes h3d:+root-node+ skybox-res)
                            0 0 0 0 0 0 210 50 201)

    ;; Add light source
    (let ((light (h3d:add-light-node h3d:+root-node+ "Light1" light-mat-res "LIGHTING" "SHADOWMAP")))
      (h3d:set-node-transform light 0 20 50 -30 0 0 1 1 1)
      (setf (h3d:node-parameter light :light-radius) 200
            (h3d:node-parameter light :light-fov) 90
            (h3d:node-parameter light :light-shadow-map-count) 3
            (h3d:node-parameter light :light-shadow-split-lambda) 0.9
            (h3d:node-parameter light :light-shadow-map-bias) 0.001
            (h3d:node-parameter light :light-color :component 0) 0.9
            (h3d:node-parameter light :light-color :component 1) 0.7
            (h3d:node-parameter light :light-color :component 2) 0.75))

    ;; initialize particles
    (dotimes (i (length (particles app)))
      (let ((p (make-instance 'particle
                              :node (h3d:add-nodes h3d:+root-node+ char-res)
                              :px (* 10.0 (sin (* 6.28 (float (/ i 100.0)))))
                              :pz (* 10.0 (cos (* 6.28 (float (/ i 100.0))))))))
        (h3d:setup-model-animation-stage (node p) 0 char-walk-res 0 "" nil)
        (choose-destination p)
        (h3d:set-node-transform (node p) (px p) 0.02 (pz p) 0 0 0 1 1 1)
        (setf (aref (particles app) i) p))))
  
  ;; Mark end of frame
  (h3d:finalize-frame))


(defmethod app-main-loop ((app chicago-application))
  (unless (freeze? app)
    (let ((inv-fps (/ 1.0 (curr-fps app))))
      (incf (anim-time app) inv-fps)
      (update-crowd app)))
  
  ;; Set camera parameters
    (with-accessors ((pos viewer-position)
                     (rot viewer-orientation)
                     (cam camera-node)
                     (font font-resource)
                     (panel panel-resource)) app

    (h3d:set-node-transform cam (aref pos 0) (aref pos 1) (aref pos 2)
                            (aref rot 0) (aref rot 1) 0 1 1 1 )

    (when (> (stat-mode app) 0)
      (h3d:show-frame-statistics font panel (stat-mode app))    

      ;; Display weight
      (h3d:show-text (format nil "Weight: ~a" (anim-weight app))
                     0.03 0.24 0.026 1 1 1 font 5))

    ;; Show logo
    (h3d:show-overlay 0.75 0.8 0 1 0.75 1 0 0
                      1 1 1 0 1 0.8 1 1
                      1 1 1 1 (logo-resource app) 7)
    
    ;; Render scene
    (h3d:render cam)))


(defun update-crowd (app)
  (let ((d1 0.25) (d2 2.0) (d3 4.5)
	(f1 3.0) (f2 1.0) (f3 0.1))
    (loop for p across (particles app) do
          (with-accessors ((px px) (pz pz)
                           (dx dx) (dz dz)
                           (fx fx) (fz fz)
                           (ox ox) (oz oz)) p
            ;; reset force
            (setf fx 0.0 fz 0.0)
            (let ((dist (sqrt (+ (* (- dx px) (- dx px))
                                 (* (- dz pz) (- dz pz))))))
              (cond
                ((> dist 3.0)
                 ;; destination not reached, walk towards destination
                 (incf fx (* 0.035 (/ (- dx px) dist)))
                 (incf fz (* 0.035 (/ (- dz pz) dist)))
                 (loop for p2 across (particles app)
                       when (not (eq p p2)) do
                       (let* ((dist2 (sqrt (+ (* (- px (px p2)) (- px (px p2)))
                                              (* (- pz (pz p2)) (- pz (pz p2))))))
                              (strength (cond
                                          ((and (<= dist2 d3) (> dist2 d2))
                                           (let* ((m (/ (- f3 0) (- d2 d3)))
                                                  (n (- 0 (* m d3))))
                                             (+ (* m dist2)) n))
                                          ((and (<= dist2 d2) (> dist2 d1))
                                           (let* ((m (/ (- f2 f3) (- d1 d2)))
                                                  (n (- f3 (* m d2))))
                                             (+ (* m dist2)) n))
                                          ((<= dist2 d1)
                                           (let* ((m (/ (- f1 f2) (- 0 d1)))
                                                  (n (- f2 (* m d1))))
                                             (+ (* m dist2)) n))
                                          (t
                                           0.0))))
                         (incf fx (* strength (/ (- px (px p2)) dist2)))
                         (incf fz (* strength (/ (- pz (pz p2)) dist2))))))
                (t
                 ;; near destination, choose a new one
                 (choose-destination p)))

              ;; make force framerate independant
              (setf fx (* fx (/ 30.0 (curr-fps app)))
                    fz (* fz (/ 30.0 (curr-fps app))))

              ;; set new position
              (incf px fx)
              (incf pz fz)

              ;; calculate orientation
              (setf ox (/ (+ ox fx) 2))
              (setf oz (/ (+ oz fz) 2))

              ;; update character position
              (let ((ry (degtorad (if (/= oz 0.0) (atan ox oz) 0.0))))
                (h3d:set-node-transform (node p) px 0.02 pz 0 ry 0 1 1 1))

              ;; update character animation
              (let ((vel (sqrt (+ (* fx fx) (* fz fz)))))
                (incf (anim-time p) (* vel 35.0))
                (h3d:set-model-animation-parameters (node p) 0 (anim-time p) 1.0)))))))



                     

;;; chicago.lisp ends here
