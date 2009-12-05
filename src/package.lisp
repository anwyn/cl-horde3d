;;; packages.lisp --- package definition and exported symbols
;;;                   _                         
;;;  _ __   __ _  ___| | ____ _  __ _  ___  ___ 
;;; | '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
;;; | |_) | (_| | (__|   < (_| | (_| |  __/\__ \
;;; | .__/ \__,_|\___|_|\_\__,_|\__, |\___||___/
;;; |_|                         |___/           
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :cl-user)

(defpackage #:horde3d
  (:nicknames #:h3d)
  (:use #:common-lisp #:cffi)
  (:export
   #:resource
   #:node
   #:+root-node+
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
   #:get-stat

   ;; Overlays
   #:show-overlay
   #:clear-overlays

   ;; Resource management
   #:get-resource-type
   #:get-resource-name
   #:find-resource
   #:add-resource
   #:clone-resource
   #:remove-resource
   #:is-resource-loaded
   #:load-resource
   #:unload-resource
   #:resource-parameter

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
   #:node-parameter
   #:get-node-aabb
   #:find-nodes
   #:get-node-find-result
   #:cast-ray
   #:get-cast-ray-result
   #:check-node-visibility
           
   ;; Group-specific scene graph functions
   #:add-group-node
   #:add-model-node
   #:setup-model-animation-stage
   #:set-model-animation-parameters
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
   #:emitter-finished-p

   ;; Horde3d utilities
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

   ;; lispification
   #:set-resource-paths
   #:set-options
   #:add-resource))


;;; packages.lisp ends here
