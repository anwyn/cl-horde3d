;;; bindings-package.lisp --- package definition and exported symbols
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :cl-user)

(defpackage #:horde3d-cffi-binding
  (:nicknames #:%h3d)
  (:use #:common-lisp #:cffi)
  (:shadow #:float #:boolean #:string)
  (:export #:+root-node+
           #:resource-handle
           #:node-handle
           #:get-version-string
           #:check-extension
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
           #:get-stat

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
           #:is-resource-loaded
           #:load-resource
           #:unload-resource
           #:get-resource-parami
           #:set-resource-parami
           #:get-resource-paramf
           #:set-resource-paramf
           #:get-resource-paramstr
           #:set-resource-paramstr
           #:get-resource-data
           #:update-resource-data
           #:query-unloaded-resource
           #:release-unused-resources
           #:create-texture-2d
           #:set-shader-preambles
           #:set-material-uniform
           #:set-material-sampler 
           #:set-pipeline-stage-activation
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
           #:get-node-paramf
           #:set-node-paramf
           #:get-node-parami
           #:set-node-parami
           #:get-node-paramstr
           #:set-node-paramstr
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
           #:setup-model-anim-stage
           #:set-model-anim-params
           #:set-model-morpher
           #:add-mesh-node
           #:add-joint-node
           #:add-light-node
           #:set-light-contexts
           #:add-camera-node
           #:setup-camera-view
           #:get-camera-projection-matrix
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
           #:show-frame-stats

           ;; Terrain
           #:add-terrain-node
           #:create-geometry-resource))


;;; binding-package.lisp ends here
