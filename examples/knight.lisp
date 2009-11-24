;;; knight.lisp --- The knight example from the horde3d distribution
;;;  _          _       _     _   
;;; | | ___ __ (_) __ _| |__ | |_ 
;;; | |/ / '_ \| |/ _` | '_ \| __|
;;; |   <| | | | | (_| | | | | |_ 
;;; |_|\_\_| |_|_|\__, |_| |_|\__|
;;;               |___/           
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :horde3d-examples)

(defclass knight-application (example-application)
  ((%particle-sys-node :accessor particle-sys-node :initarg :particle-sys-node)
   (%knight-node       :accessor knight-node       :initarg :knight-node)))

(defun knight ()
  (example-main-sdl
   (make-instance 'knight-application
                  :viewer-position (make-array 3 :element-type 'single-float
                                               :initial-contents '(5.0 3.0 19.0))
                  :viewer-orientation (make-array 3 :element-type 'single-float
                                                  :initial-contents '(7.0 15.0 0.0))
                  :stat-mode 2
                  :anim-weight 0.0
                  :content-path #p"/home/ole/src/graphics//Horde3D_SDK_1.0.0_Beta4/Horde3D/Binaries/Content/")
   :width 1280
   :height 800
   :caption "Knight - Horde3D Sample"))


(defmethod app-init ((app knight-application))
  
  ;; Add resources
  (let ((env-res (h3d:add-resource :scene-graph "models/sphere/sphere.scene.xml" 0))
        (knight-res (h3d:add-resource :scene-graph "models/knight/knight.scene.xml" 0))
        (knight-anim-res-1 (h3d:add-resource :animation "animations/knight_order.anim" 0))
        (knight-anim-res-2 (h3d:add-resource :animation "animations/knight_attack.anim" 0))
        (particle-sys-res (h3d:add-resource :scene-graph "particles/particleSys1/particleSys1.scene.xml" 0)))

    ;; Load resources
    (h3d:load-resources-from-disk (namestring (content-path app)))

    ;; add camera
    (setf (camera-node app) (h3d:add-camera-node h3d:+root-node+ "Camera" (hdr-pipeline app)))

    (let ((env (h3d:add-nodes h3d:+root-node+ env-res))
          (knight (h3d:add-nodes h3d:+root-node+ knight-res)))
      (setf (knight-node app) knight)
      
      (h3d:set-node-transform env 0 -20 0 0 0 0 20 20 20)
      (h3d:set-node-transform knight 0 0 0 0 180 0 0.1 0.1 0.1)
      
      (h3d:setup-model-animation-stage knight 0 knight-anim-res-1 0 "" nil)
      (h3d:setup-model-animation-stage knight 1 knight-anim-res-2 0 "" nil)
      
      ;; add particles to hand
      (h3d:find-nodes knight "Bip01_R_Hand" :joint)
      (let ((particle-sys-node (h3d:add-nodes (h3d:get-node-find-result 0) particle-sys-res)))
        (setf (particle-sys-node app) particle-sys-node)
        (h3d:set-node-transform particle-sys-node 0 40 0 90 0 0 1 1 1))))

  ;; Add light source
  (let ((light (h3d:add-light-node h3d:+root-node+ "Light1" 0 "LIGHTING" "SHADOWMAP")))
    (h3d:set-node-transform light 0 15 10 -60 0 0 1 1 1)
    (setf (h3d:node-parameter light :light-radius) 30
          (h3d:node-parameter light :light-fov) 90
          (h3d:node-parameter light :light-shadow-map-count) 1
          (h3d:node-parameter light :light-shadow-map-bias) 0.01
          (h3d:node-parameter light :light-color :component 0) 1.0
          (h3d:node-parameter light :light-color :component 1) 0.8
          (h3d:node-parameter light :light-color :component 2) 0.7))

  ;; Customize post processing effects
  (let ((mat-res (h3d:find-resource :material "pipelines/postHDR.material.xml")))
    (h3d:set-material-uniform mat-res "hdrParams" 2.5 0.5 0.08 0))

  ;; Mark end of frame
  (h3d:finalize-frame))

(defmethod app-resize ((app knight-application) width height)
  (h3d:setup-viewport 0 0 width height t)
  (h3d:setup-camera-view (camera-node app) 45.0
                         (/ width height)
                         0.1 1000.0))


(defmethod app-main-loop ((app knight-application))
  (when (gethash :sdl-key-1 (keys app))
    (incf (anim-weight app) (/ 2 (curr-fps app)))
    (when (> (anim-weight app) 1.0)
      (setf (anim-weight app) 1.0)))

  (when (gethash :sdl-key-2 (keys app))
    (decf (anim-weight app) (/ 2 (curr-fps app)))
    (when (< (anim-weight app) 0.0)
      (setf (anim-weight app) 0.0)))
  
  (unless (freeze? app)
    (let ((inv-fps (/ 1.0 (curr-fps app))))
      (incf (anim-time app) inv-fps)
      (h3d:set-model-animation-parameters (knight-node app) 0
                                          (* 24.0 (anim-time app))
                                          (anim-weight app))
      (h3d:set-model-animation-parameters (knight-node app) 1
                                          (* 24.0 (anim-time app))
                                          (- 1.0 (anim-weight app)))

      ;; candidate for a donodes macro
      (dotimes (i (h3d:find-nodes (particle-sys-node app) "" :emitter))
        (h3d:advance-emitter-time (h3d:get-node-find-result i) inv-fps))))
  
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


;;; knight.lisp ends here
