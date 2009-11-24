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

(import-export resource
               node
               %h3d:+root-node+
               no-such-enum-type)

;;;; Enumerations

;;;; Basic functions

(import-export %h3d:get-version-string
               %h3d:check-extension
               %h3d:get-error
               %h3d:init
               %h3d:release
               %h3d:setup-viewport
               %h3d:render
               %h3d:finalize-frame
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
                    max-anisotropy
                    texture-compression
                    load-textures
                    fast-animation
                    shadow-map-size
                    sample-count
                    wireframe-mode
                    debug-view-mode
                    dump-failed-shaders)
  (declare (ignore max-log-level
                    max-num-messages
                    trilinear-filtering
                    max-anisotropy
                    texture-compression
                    load-textures
                    fast-animation
                    shadow-map-size
                    sample-count
                    wireframe-mode
                    debug-view-mode
                    dump-failed-shaders))
  (loop for (key . rest) on plist by #'cddr
     do (set-option key (car rest))))

(import-export %h3d:get-statistics
               %h3d:show-overlay
               %h3d:clear-overlays)

;;;; General resource management functions

(import-export %h3d:get-resource-type
               %h3d:get-resource-name
               %h3d:get-next-resource
               %h3d:find-resource
               %h3d:add-resource
               %h3d:clone-resource
               %h3d:remove-resource
               %h3d:resource-loaded-p
               %h3d:load-resource
               %h3d:unload-resource
               %h3d:get-resource-element-count
               %h3d:find-resource-element)


(defun resource-parameter
    (resource element element-index parameter &key (component 0)) 
  (let ((return-type (%h3d:enum-type parameter)))
    (ecase return-type
      ((:int :resource)
       (%h3d:get-resource-parameter-i resource element element-index parameter))
      (:float
       (%h3d:get-resource-parameter-f resource element element-index parameter component))
      (:string
       (%h3d:get-resource-parameter-str resource element element-index parameter)))))

(define-compiler-macro resource-parameter
    (&whole form resource element element-index parameter &key (component 0))
  (if (keywordp parameter)
      (let ((return-type (%h3d:enum-type parameter)))
        (ecase return-type
          ((:int :resource)
           `(%h3d:get-resource-parameter-i ,resource ,element ,element-index ,parameter))
          (:float
           `(%h3d:get-resource-parameter-f ,resource ,element ,element-index ,parameter
                                           ,component))
          (:string
           `(%h3d:get-resource-parameter-str ,resource ,element ,element-index ,parameter))))
      form))

#+(or)
(defun update-resource-data (handle param data &optional size)
  (cond ((pointerp data)
         (%h3d:update-resource-data handle param data size))
        ((stringp data)
         (with-foreign-string (str data)
           (%h3d:update-resource-data handle param str (or size (length data)))))
        ((vectorp data)
         (with-pointer-to-vector-data (array data)
           (%h3d:update-resource-data handle param array (or size (length data)))))
        (t
         (error "unknown data format"))))
#+(or)
(define-compiler-macro update-resource-data (&whole form handle param data &optional size)
  (if (constantp data)
      (cond ((pointerp data)
             `(%h3d:update-resource-data ,handle ,param ,data ,size))
            ((stringp data)
             (with-unique-names (str)
               `(with-foreign-string (,str ,data)
                  (%h3d:update-resource-data ,handle ,param ,str (or ,size (cl:length ,data))))))
            ((vectorp data)
             (with-unique-names (array)
               `(with-pointer-to-vector-data (,array ,data)
                  (%h3d:update-resource-data ,handle ,param ,array (or ,size (cl:length ,data))))))
            (t
             (error "unknown data format")))
      form))

(defun set-resource-parameter (resource element element-index parameter value &key (component 0))
  (let ((type (%h3d:enum-type parameter)))
    (ecase type
      ((:int :resource)
       (%h3d:set-resource-parameter-i resource element element-index parameter value))
      (:float
       (%h3d:set-resource-parameter-f resource element element-index parameter component value))
      (:string
       (%h3d:set-resource-parameter-str resource element element-index parameter value)))
    value))

(define-compiler-macro set-resource-parameter
    (&whole form resource element element-index parameter value &key (component 0))
  (if (keywordp parameter)
      (with-unique-names (val)
        (let ((type (%h3d:enum-type parameter)))
          (when (null type)
            (error 'no-such-enum-type :enum parameter))
          (ecase type
            ((:int :resource)
             `(let ((,val ,value))
                (%h3d:set-resource-parameter-i ,resource ,element ,element-index
                                               ,parameter ,val) ,val))
            (:float
             `(let ((,val ,value))
                (%h3d:set-resource-parameter-f ,resource ,element ,element-index
                                               ,parameter ,component ,val) ,val))
            (:string
             `(let ((,val ,value))
                (%h3d:set-resource-parameter-str ,resource ,element ,element-index
                                                 ,parameter ,val) ,val)))))
      form))

(defsetf resource-parameter (resource element element-index parameter &key (component 0))
    (store)
  `(set-resource-parameter ,resource ,element ,element-index
                           ,parameter ,component ,store))

(import-export %h3d:map-resource-stream
               %h3d:unmap-resource-stream
               %h3d:query-unloaded-resource
               %h3d:release-unused-resources)

;;;; Specific resource management functions

(import-export %h3d:create-texture
               %h3d:set-shader-preambles
               %h3d:set-material-uniform)

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

(defun node-parameter (node parameter &key (component 0))
  (let ((type (%h3d:enum-type parameter)))
    (ecase type
      ((:int :resource)
       (%h3d:get-node-parameter-i node parameter))
      (:float
       (%h3d:get-node-parameter-f node parameter component))
      (:string
       (%h3d:get-node-parameter-str node parameter)))))

(define-compiler-macro node-parameter (&whole form node parameter &key (component 0))
  (if (keywordp parameter)
      (let ((type (%h3d:enum-type parameter)))
        (ecase type
          ((:int :resource)
           `(%h3d:get-node-parameter-i ,node ,parameter))
          (:float
           `(%h3d:get-node-parameter-f ,node ,parameter ,component))
          (:string
           `(%h3d:get-node-parameter-str ,node ,parameter))))
      form))

(defun set-node-parameter (node parameter value &key (component 0))
  (let ((type (%h3d:enum-type parameter)))
    (ecase type
      ((:int :resource)
       (%h3d:set-node-parameter-i node parameter value))
      (:float
       (%h3d:set-node-parameter-f node parameter component value))
      (:string
       (%h3d:set-node-parameter-str node parameter value)))
    value))

(define-compiler-macro set-node-parameter (&whole form node parameter value &key (component 0))
  (if (keywordp parameter)
      (with-unique-names (val)
        (let ((type (%h3d:enum-type parameter)))
          (ecase type
            ((:int :resource)
             `(let ((,val ,value))
                (%h3d:set-node-parameter-i ,node ,parameter ,val) ,val))
            (:float
             `(let ((,val ,value))
                (%h3d:set-node-parameter-f ,node ,parameter ,component ,val) ,val))
            (:string
             `(let ((,val ,value))
                (%h3d:set-node-parameter-str ,node ,parameter ,val) ,val)))))
      form))

(defsetf node-parameter (node param &key (component 0)) (store)
  `(set-node-parameter ,node ,param ,store :component ,component))

(defun get-node-aabb (node)
  (with-foreign-objects ((minx :float) (miny :float) (minz :float)
                         (maxx :float) (maxy :float) (maxz :float))
    (%h3d:get-node-aabb node minx miny minz maxy maxy maxz)
    (values (mem-ref minx :float) (mem-ref miny :float) (mem-ref minz :float)
            (mem-ref maxx :float) (mem-ref maxy :float) (mem-ref maxz :float))))

(import-export %h3d:find-nodes
               %h3d:get-node-find-result
               %h3d:cast-ray)

(define-condition no-such-node-result (error)
  ((index :initarg :index :reader index)))

(defun get-cast-ray-result (index &optional intersection-point)
  (let ((point (or intersection-point (make-array 3 :element-type 'single-float))))
    (with-pointer-to-vector-data (intersection point)
      (with-foreign-objects ((node :int)
                             (distance :float))
        (unless (%h3d:get-cast-ray-result index node distance intersection)
          (error 'no-such-node-result :index index))
        (values (mem-ref node :int) (mem-ref distance :float)
                intersection)))))

;;; Lispification

(defmacro do-ray-cast ((node origin direction &key (max-results 0))
                       (rnode distance intersection) &body body)
  (with-unique-names (index orig dir point)
    `(let ((,orig ,origin)
           (,dir ,direction)
           (,point (make-array 3 :element-type 'single-float)))
       (dotimes (,index (cast-ray ,node
                                  (aref orig 0) (aref orig 1) (aref orig 2)
                                  (aref dir 0) (aref dir 1) (aref dir 2)
                                  ,max-results))
         (multiple-value-bind (,rnode ,distance ,intersection )
             (get-cast-ray-result ,index)
           ,@body)))))


(import-export %h3d:check-node-visibility)

;;;; Group-specific scene graph functions 

(import-export %h3d:add-group-node)

;;;; Model-specific scene graph functions 

(import-export %h3d:add-model-node
               %h3d:setup-model-animation-stage
               %h3d:set-model-animation-parameters
               %h3d:set-model-morpher)

;;;; Mesh-specific scene graph functions 

(import-export %h3d:add-mesh-node)

;;;; Joint-specific scene graph functions

(import-export %h3d:add-joint-node)

;;;; Light-specific scene graph functions 

(import-export %h3d:add-light-node)

;;;; Camera-specific scene graph functions

(import-export %h3d:add-camera-node
               %h3d:setup-camera-view)

(defun get-camera-projection-matrix (camera-node &optional matrix)
  (with-pointer-to-vector-data (mat (or matrix (make-array 16 :element-type 'single-float)))
    (%h3d:get-camera-projection-matrix camera-node mat)
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
  (declare (ignore scene-graph
                   geometry
                   animation
                   material
                   code
                   shader
                   texture-2d
                   texture-cube
                   effect
                   pipeline))
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
               %h3d:show-frame-statistics)

;;; horde3d.lisp ends here
