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
  (example-main
   (make-instance 'knight-application
                  :viewer-position (make-array 3 :element-type 'single-float
                                               :initial-contents '(5.0 3.0 19.0))
                  :viewer-orientation (make-array 3 :element-type 'single-float
                                                  :initial-contents '(7.0 15.0 0.0))
                  :show-stats t
                  :content-path #p"/home/ole/src/graphics/Horde3D/Horde3D/Binaries/Content/")
   :caption "Knight - Horde3D Sample"))


(defmethod app-init ((app knight-application))
  (h3d:set-options :load-textures 1.0
                   :tex-compression 0.0
                   :fast-animation 0.0
                   :anisotropy-factor 8.0
                   :shadow-map-size 2048.0)
  ;; Add resources
  (let ((hdr-pipe-res (h3d:add-resource :pipeline "hdr.pipeline.xml" 0))
        (fwd-pipe-res (h3d:add-resource :pipeline "forward.pipeline.xml" 0))
        (env-res (h3d:add-resource :scene-graph "sphere.scene.xml" 0))
        (knight-res (h3d:add-resource :scene-graph "knight.scene.xml" 0))
        (knight-anim-res-1 (h3d:add-resource :animation "knight_order.anim" 0))
        (knight-anim-res-2 (h3d:add-resource :animation "knight_attack.anim" 0))
        (particle-sys-res (h3d:add-resource :scene-graph "particleSys1.scene.xml" 0)))

    (setf (font-resource app) (h3d:add-resource :material "font.material.xml" 0)
          (logo-resource app) (h3d:add-resource :material "logo.material.xml" 0))

    ;; Load resources
    (h3d:load-resources-from-disk (namestring (content-path app)))

    ;; add camera
    (setf (camera-node app) (h3d:add-camera-node h3d:+root-node+ "Camera" fwd-pipe-res))

    (let ((env (h3d:add-nodes h3d:+root-node+ env-res))
          (knight (h3d:add-nodes h3d:+root-node+ knight-res)))
      (setf (knight-node app) knight)
      
      (h3d:set-node-transform env 0.0 -20.0 0.0 0.0 0.0 0.0 20.0 20.0 20.0)
      (h3d:set-node-transform knight 0.0 0.0 0.0 0.0 180.0 0.0 0.1 0.1 0.1)
      ;; (h3d:setup-model-anim-stage knight 0 knight-anim-res-1 "" nil)
      (h3d:setup-model-anim-stage knight 0 knight-anim-res-2 "" nil)

      ;; add particles to hand
      (h3d:find-nodes knight "Bip01_R_Hand" :joint)
      (let ((particle-sys-node (h3d:add-nodes (h3d:get-node-find-result 0) particle-sys-res)))
        (setf (particle-sys-node app) particle-sys-node)
        (h3d:set-node-transform particle-sys-node 0.0 40.0 0.0 90.0 0.0 0.0 1.0 1.0 1.0))))

    ;; Add light source
    (let ((light (h3d:add-light-node h3d:+root-node+ "Light1" 0 "LIGHTING" "SHADOWMAP")))
      (h3d:set-node-transform light 0.0 15.0 10.0 -60.0 0.0 0.0 1.0 1.0 1.0)
      (h3d:set-node-paramf light :radius 30.0)
      (h3d:set-node-paramf light :fov 90.0)
      (h3d:set-node-parami light :shadow-map-count 1)
      (h3d:set-node-paramf light :shadow-map-bias 0.01)
      (h3d:set-node-paramf light :col-r 1.0)
      (h3d:set-node-paramf light :col-g 0.8)
      (h3d:set-node-paramf light :col-b 0.7))

    ;; Customize post processing effects
    (let ((mat-res (h3d:find-resource :material "postHDR.material.xml")))
      (h3d:set-material-uniform mat-res "hdrParams" 2.5 0.5 0.08 0.0)))

(defmethod app-resize ((app knight-application) width height)
  (h3d:resize 0 0 width height)
  (h3d:setup-camera-view (camera-node app) 45.0
                         (coerce (/ width height) 'single-float)
                         0.1 1000.0))


(defmethod app-main-loop ((app knight-application) fps)
  (setf (curr-fps app) (coerce fps 'single-float))
  
  (h3d:set-option :debug-view-mode (if (show-debug-view? app) 1.0 0.0))
  (h3d:set-option :wireframe-mode (if (show-wire-frame? app) 1.0 0.0))
  
  (unless (freeze? app)
    (incf (anim-time app) (/ 1.0 fps))

    (h3d:set-model-anim-params (knight-node app) 0
                               (* 24.0 (anim-time app))
                               (anim-weight app))
    (h3d:set-model-anim-params (knight-node app) 1
                               (* 24.0 (anim-time app))
                               (- 1.0 (anim-weight app)))

    ;; candidate for a donodes macro
    (dotimes (i (h3d:find-nodes (particle-sys-node app) "" :emitter))
      (h3d:advance-emitter-time (h3d:get-node-find-result i)
                                (coerce (/ 1.0 (curr-fps app)) 'single-float))))
  
  ;; Set camera parameters
  
  (with-accessors ((pos viewer-position)
                   (rot viewer-orientation)
                   (cam camera-node)
                   (font font-resource)) app
    (h3d:set-node-transform cam (aref pos 0) (aref pos 1) (aref pos 2)
                            (aref rot 0) (aref rot 1) 0.0 1.0 1.0 1.0 )
    
    (when (show-stats? app)
      (h3d:show-frame-stats font (curr-fps app))

      ;; Display weight
      (h3d:show-text (format nil "Weight: ~a" (anim-weight app))
                     0.0 0.78 0.03 0 font))

    ;; Show logo
    (h3d:show-overlay 0.75 0.0 0.0 0.0 1.0 0.0 1.0 0.0
                      1.0 0.2 1.0 1.0 0.75 0.2 0.0 1.0 7 (logo-resource app))
    
    ;; Render scene
    (h3d:render cam))

  ;; Remove all overlays
  (h3d:clear-overlays)

  ;; Write all mesages to log file
  (h3d:dump-messages))


;;; knight.lisp ends here
