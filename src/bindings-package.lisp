;;; bindings-package.lisp --- package definition and exported symbols
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :cl-user)

(defpackage #:horde3d-cffi
  (:nicknames #:%h3d)
  (:use #:common-lisp #:cffi)
  (:shadow #:float #:boolean #:string)
  (:export

   ;; defines
   #:enum-type
   #:notice-enum-type
   #:deftypedenum
   #:defh3fun

   
   ;; conditions
   #:no-such-enum-type
   
   ;; constants
   #:+root-node+

   ;; types
   #:resource
   #:node
   #:boolean
   #:float
   #:string
   #:int
   #:void
   
   ;; enums
   #:option
   #:statistics
   #:resource-type
   #:resource-flags
   #:resource-format
   #:resource-parameter
   #:resource-element
   #:node-type
   #:node-parameter
   
   
   ;; basic functions
   #:get-version-string
   #:check-extension
   #:get-error
   #:init
   #:release
   #:setup-viewport
   #:render
   #:finalize-frame
   #:clear

   ;; General functions
   #:get-message
   #:get-option
   #:set-option
   #:get-statistics

   ;; Overlays
   #:show-overlay
   #:clear-overlays

   ;; Resource management
   #:get-resource-type
   #:get-resource-name
   #:get-next-resource
   #:find-resource
   #:add-resource
   #:clone-resource
   #:remove-resource
   #:resource-loaded-p
   #:load-resource
   #:unload-resource
   #:get-resource-element-count
   #:find-resource-element
   
   #:get-resource-parameter-i
   #:set-resource-parameter-i
   #:get-resource-parameter-f
   #:set-resource-parameter-f
   #:get-resource-parameter-str
   #:set-resource-parameter-str
   #:map-resource-stream
   #:unmap-resource-stream
   
   #:query-unloaded-resource
   #:release-unused-resources

   ;; Specific resource management functions
   #:create-texture
   #:set-shader-preambles
   #:set-material-uniform
   #:get-pipeline-render-target-data

   ;;  General scene graph functions
   #:get-node-type
   #:get-node-parent
   #:set-node-parent
   #:get-node-child
   #:add-nodes
   #:remove-node
   #:set-node-activation
   #:check-node-transform-flag
   #:get-node-transform
   #:set-node-transform
   #:get-node-transform-matrices
   #:set-node-transform-matrix
   #:get-node-parameter-i
   #:set-node-parameter-i
   #:get-node-parameter-f
   #:set-node-parameter-f
   #:get-node-parameter-str
   #:set-node-parameter-str
   #:get-node-aabb
   #:find-nodes
   #:get-node-find-result
   #:cast-ray
   #:get-cast-ray-result
   #:check-node-visibility
           
   ;; Group-specific scene graph functions
   #:add-group-node

   ;; Model-specific scene graph functions
   #:add-model-node
   #:setup-model-animation-stage
   #:set-model-animation-parameters
   #:set-model-morpher

   ;; Mesh-specific scene graph functions
   #:add-mesh-node

   ;; Joint-specific scene graph functions
   #:add-joint-node

   ;; Light-specific scene graph functions
   #:add-light-node

   ;; Camera-specific scene graph functions
   #:add-camera-node
   #:setup-camera-view
   #:get-camera-projection-matrix

   ;; Emitter-specific scene graph functions
   #:add-emitter-node
   #:advance-emitter-time
   #:has-emitter-finished

   ;; Horde utils

   #:free-mem
   #:dump-messages
   #:init-open-gl
   #:release-open-gl
   #:swap-buffers
   #:get-resource-path
   #:set-resource-path
   #:load-resources-from-disk
   #:create-tga-image
   #:pick-ray
   #:pick-node
   #:show-text
   #:show-frame-statistics))


;;; binding-package.lisp ends here
