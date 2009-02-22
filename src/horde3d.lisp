;;; horde3d.lisp --- lispification for the horde3d wrapper
;;;  _                   _      _____     _ 
;;; | |__   ___  _ __ __| | ___|___ /  __| |
;;; | '_ \ / _ \| '__/ _` |/ _ \ |_ \ / _` |
;;; | | | | (_) | | | (_| |  __/___) | (_| |
;;; |_| |_|\___/|_|  \__,_|\___|____/ \__,_|
                                        
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :horde3d)

(defmacro import-export (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; Unintern first to avoid conflicts.
     (dolist (sym ',symbols)
       (let ((s (find-symbol (symbol-name sym))))
         (when s (unintern s))))
     ;; Import and re-export.
     (import ',symbols)
     (export ',symbols)))

(defmacro with-unique-names (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                          (stem (if (every #'alpha-char-p symbol-name)
                                    symbol-name
                                    (concatenate 'string symbol-name "-"))))
                     `(,symbol (gensym ,stem))))
                 symbols)
     ,@body))

(defmacro define-get-function (name (&rest head-args) &body clauses)
  (with-unique-names (return-type return-count p)
    `(defun ,name (,@head-args ,return-type &optional (,return-count 1))
       (with-foreign-object (,p ,return-type ,return-count)
         (ecase ,return-type
           ,@(loop for (func . types) in clauses
                collect `((,@types) (,func ,@head-args ,p))))
         (if (= ,return-count 1)
             (mem-aref ,p ,return-type 0)
             (loop for i from 0 to (1- ,return-count)
                collect (mem-aref ,p ,return-type i)))))))

;;;; Typedefs and constants

(import-export %h3d:+root-node+)

;;;; Enumerations

;;;; Basic functions

(import-export %h3d:get-version-string
               %h3d:check-extension
               %h3d:init
               %h3d:release
               %h3d:resize
               %h3d:render
               %h3d:clear)

(defun get-message ()
  (with-foreign-objects ((level :int)
                         (time :float))
    (values (%h3d:get-message level time)
            (mem-ref level :int)
            (mem-ref time :float))))

(import-export %h3d:get-option
               %h3d:set-option)

(defun set-options (&rest plist
                    &key max-log-level
                    max-num-messages
                    trilinear-filtering
                    anisotropy-factor
                    tex-compression
                    load-textures
                    fast-animation
                    shadow-map-size
                    sample-count
                    wireframe-mode
                    debug-view-mode)
  (loop for (key . rest) on plist by #'cddr
     do (set-option key (car rest))))

(import-export %h3d:get-stat
               %h3d:show-overlay
               %h3d:clear-overlays)

;;;; General resource management functions

(import-export %h3d:get-resource-type
               %h3d:get-resource-name
               %h3d:find-resource
               %h3d:add-resource
               %h3d:clone-resource
               %h3d:remove-resource
               %h3d:is-resource-loaded
               %h3d:load-resource
               %h3d:unload-resource)

(defun resource-param-to-type (param)
  (ecase param
    (:vertex-count :int) 
    (:index-count :int)
    (:vertex-data :data)
    (:index-data :data)

    (:frame-count :int) 

    (:class :string) 
    (:link :int)
    (:shader :int)
    (:tex-unit-0 :int)
    (:tex-unit-1 :int)
    (:tex-unit-2 :int)
    (:tex-unit-3 :int)
    (:tex-unit-4 :int)
    (:tex-unit-5 :int)
    (:tex-unit-6 :int)
    (:tex-unit-7 :int)
    (:tex-unit-8 :int)
    (:tex-unit-9 :int)
    (:tex-unit-10 :int)
    (:tex-unit-11 :int)

    (:pixel-data :data)
    (:width :int)
    (:height :int)
    (:comps :int)
    (:hdr :int)
    
    (:life-min :float)
    (:life-max :float)
    (:move-vel-min :float)
    (:move-vel-max :float)
    (:move-vel-end-rate :float)
    (:rot-vel-min :float)
    (:rot-vel-max :float)
    (:rot-vel-end-rate :float)
    (:size-min :float)
    (:size-max :float)
    (:size-end-rate :float)
    (:col-r-min :float)
    (:col-r-max :float)
    (:col-r-end-rate :float)
    (:col-g-min :float)
    (:col-g-max :float)
    (:col-g-end-rate :float)
    (:col-b-min :float)
    (:col-b-max :float)
    (:col-b-end-rate :float)
    (:col-a-min :float)
    (:col-a-max :float)
    (:col-a-end-rate :float)))

(defun resource-parameter (handle param)
  (let ((return-type (resource-param-to-type param)))
    (ecase return-type
      (:int
       (%h3d:get-resource-parami handle param))
      (:float
       (%h3d:get-resource-paramf handle param))
      (:string
       (%h3d:get-resource-paramstr handle param))
      (:data
       (%h3d:get-resource-data handle param)))))

(defun update-resource-data (handle param data &optional size)
  (cond
    ((pointerp data)
     (%h3d:update-resource-data handle param data size))
    ((stringp data)
     (with-foreign-string (str data)
       (%h3d:update-resource-data handle param str (length data))))
    ((vectorp data)
     (with-pointer-to-vector-data (array data)
       (%h3d:update-resource-data handle param array (length data))))
    (t
     (error "unknown data format"))))

(defun set-resource-parameter (handle param value &optional size)
  (let ((type (resource-param-to-type param)))
    (ecase type
      (:int
       (%h3d:set-resource-parami handle param value))
      (:float
       (%h3d:set-resource-paramf handle param value))
      (:string
       (%h3d:set-resource-paramstr handle param value))
      (:data
       (update-resource-data handle param value size)))
    value))

(defsetf resource-parameter (handle param &optional size) (store)
  `(set-resource-parameter ,handle ,param ,store ,size))

(import-export %h3d:query-unloaded-resource
               %h3d:release-unused-resources)

;;;; Specific resource management functions

(import-export %h3d:create-texture-2d
               %h3d:set-shader-preambles
               %h3d:set-material-uniform
               %h3d:set-pipeline-stage-activation)

(defun get-pipeline-render-target-data
    (pipeline-res target-name buf-index &optional data-buffer buffer-size)
  (with-foreign-objects ((w :int)
                         (h :int)
                         (count :int))
    (if (and (pointerp data-buffer) (not (null buffer-size)))
        (%h3d:get-pipeline-render-target-data pipeline-res target-name
                                              buf-index w h count data-buffer buffer-size)
        (%h3d:get-pipeline-render-target-data pipeline-res target-name
                                              buf-index w h count (null-pointer) 0))
    (values (mem-ref w :int) (mem-ref h :int) (mem-ref count :int))))

;;;; General scene graph functions

(import-export %h3d:get-node-type
               %h3d:get-node-parent
               %h3d:get-node-child
               %h3d:add-nodes
               %h3d:remove-node
               %h3d:set-node-activation
               %h3d:check-node-transform-flag)

(defun get-node-transform (handle)
  (with-foreign-objects ((tx :float) (ty :float) (tz :float)
                         (rx :float) (ry :float) (rz :float)
                         (sx :float) (sy :float) (sz :float))
    (%h3d:get-node-transform handle tx ty tz rx ry rz sx sy sz)
    (values (mem-ref tx :float) (mem-ref ty :float) (mem-ref tz :float)
            (mem-ref rx :float) (mem-ref ry :float) (mem-ref rz :float)
            (mem-ref sx :float) (mem-ref sy :float) (mem-ref sz :float))))

(import-export %h3d:set-node-transform
               %h3d:get-node-transform-matrices
               %h3d:set-node-transform-matrix)

(defun node-param-to-type (param)
  (ecase param
    (:name :string)
    (:attachment-string :string)
    (:min-dist :float)
    (:max-dist :float)
    (:geometry '%h3d::resource-handle)
    (:software-skinning :int)
    (:mesh-material '%h3d::resource-handle)
    (:batch-start :int)
    (:batch-count :int)
    (:vert-r-start :int)
    (:vert-r-end :int)
    (:joint-index :int)
    (:light-material '%h3d::resource-handle)
    (:radius :float)
    (:fov :float)
    (:col-r :float)
    (:col-g :float)
    (:col-b :float)
    (:shadow-map-count :int)
    (:shadow-split-lambda :float)
    (:shadow-map-bias :float)
    (:pipeline '%h3d::resource-handle)
    (:output-tex '%h3d::resource-handle)
    (:output-buffer-index :int)
    (:left-plane :float)
    (:right-plane :float)
    (:bottom-plane :float)
    (:top-plane :float)
    (:near-plane :float)
    (:far-plane :float)
    (:orthographic :int)
    (:occlusion-culling :int)
    (:emitter-material '%h3d::resource-handle)
    (:effect-res '%h3d::resource-handle)
    (:max-count :int)
    (:respawn-count :int)
    (:delay :float)
    (:emission-rate :float)
    (:spread-angle :float)
    (:force-X :float)
    (:force-Y :float)
    (:force-Z :float)
    (:height-map-res '%h3d::resource-handle)
    (:terrain-material '%h3d::resource-handle)
    (:mesh-quality :float)
    (:skirt-height :float)
    (:block-size :int)))

(defun node-parameter (handle param)
  (let ((type (node-param-to-type param)))
    (ecase type
      ((:int '%h3d::resource-handle)
       (%h3d:get-node-parami handle param))
      (:float
       (%h3d:get-node-paramf handle param))
      (:string
       (%h3d:get-node-paramstr handle param)))))

(defun set-node-parameter (handle param value)
  (let ((type (node-param-to-type param)))
    (ecase type
      ((:int '%h3d::resource-handle)
       (%h3d:set-node-parami handle param value))
      (:float
       (%h3d:set-node-paramf handle param value))
      (:string
       (%h3d:set-node-paramstr handle param value)))
    value))

(defsetf node-parameter (handle param) (store)
  `(set-node-parameter ,handle ,param ,store))

(defun get-node-aabb (handle)
  (with-foreign-objects ((minx :float) (miny :float) (minz :float)
                         (maxx :float) (maxy :float) (maxz :float))
    (%h3d:get-node-aabb handle minx miny minz maxy maxy maxz)
    (values (mem-ref minx :float) (mem-ref miny :float) (mem-ref minz :float)
            (mem-ref maxx :float) (mem-ref maxy :float) (mem-ref maxz :float))))

(import-export %h3d:find-nodes
               %h3d:get-node-find-result
               %h3d:cast-ray)

(define-condition no-such-node-result (error)
  ((index :initarg :index :reader index)))

(defun get-cast-ray-result (index)
  (with-pointer-to-vector-data (intersection (make-array 3 :element-type 'single-float))
    (with-foreign-objects ((node :int)
                           (distance :float))
      (unless (%h3d:get-cast-ray-result index node distance intersection)
        (error 'no-such-node-result :index index))
      (values (mem-ref node :int) (mem-ref distance :float)
              intersection))))

;;;; Group-specific scene graph functions 

(import-export %h3d:add-group-node)

;;;; Model-specific scene graph functions 

(import-export %h3d:add-group-node
               %h3d:add-model-node
               %h3d:setup-model-anim-stage
               %h3d:set-model-anim-params
               %h3d:set-model-morpher)

;;;; Mesh-specific scene graph functions 

(import-export %h3d:add-mesh-node)

;;;; Joint-specific scene graph functions

(import-export %h3d:add-joint-node)

;;;; Light-specific scene graph functions 

(import-export %h3d:add-light-node
               %h3d:set-light-contexts)

;;;; Camera-specific scene graph functions

(import-export %h3d:add-camera-node
               %h3d:setup-camera-view)

(defun calc-camera-projection-matrix (camera-node &optional matrix)
  (with-pointer-to-vector-data (mat (or matrix (make-array 16 :element-type 'single-float)))
    (%h3d:calc-camera-projection-matrix camera-node mat)
    mat))

;;;; Emitter-specific scene graph functions

(import-export %h3d:add-emitter-node
               %h3d:advance-emitter-time
               %h3d:has-emitter-finished)

;;;; Util Library

(import-export %h3d:dump-messages
               %h3d:init-open-gl
               %h3d:release-open-gl
               %h3d:swap-buffers)

;;;; Resource management

(import-export %h3d:get-resource-path
               %h3d:set-resource-path
               %h3d:load-resources-from-disk)

(defun set-resource-paths (&rest plist
                           &key scene-graph
                           geometry
                           animation
                           material
                           code
                           shader
                           texture-2d
                           texture-cube
                           effect
                           pipeline)
  (loop for (key . rest) on plist by #'cddr
     do (%h3d:set-resource-path key (car rest))))

(defun create-tga-image (pixels width height bpp)
  (with-foreign-objects ((out-data :pointer)
                         (out-size :int))
    (%h3d:create-tga-image pixels width height bpp out-data out-size)
    (values (mem-ref out-data :pointer) (mem-ref out-size :int))))

;;;; Scene graph

(defun pick-ray (camera-node nwx nwy)
  (with-foreign-objects ((ox :float) (oy :float) (oz :float)
                         (dx :float) (dy :float) (dz :float))
    (%h3d:pick-ray camera-node nwx nwy ox oy oz dx dy dz)
    (values (mem-ref ox :float) (mem-ref oy :float) (mem-ref oz :float)
            (mem-ref dx :float) (mem-ref dy :float) (mem-ref dz :float))))

(import-export %h3d:pick-node)

;;;; Overlays

(import-export %h3d:show-text
               %h3d:show-frame-stats)

;;;; Terrain Extension

(import-export %h3d:add-terrain-node
               %h3d:create-geometry-resource)

;;;; Sound Extension

;;; horde3d.lisp ends here
