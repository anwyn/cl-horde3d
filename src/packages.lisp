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
  (:export #:+root-node+
           #:get-version-string
           #:check-extension
           #:init
           #:release
           #:resize
           #:render
           #:clear
           #:get-message
           #:get-option
           #:set-option
           #:get-stat
           #:show-overlay
           #:clear-overlays
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
           #:update-resource-data
           #:query-unloaded-resource
           #:release-unused-resources
           #:create-texture-2d
           #:set-shader-preambles
           #:set-material-uniform
           #:set-pipeline-stage-activation
           #:get-pipeline-render-target-data
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
           #:add-group-node
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
           #:calc-camera-projection-matrix
           #:add-emitter-node
           #:advance-emitter-time
           #:has-emitter-finished
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
           #:add-terrain-node
           #:create-geometry-resource

           ;; lispification
           #:set-resource-paths
           #:set-options))


;;; packages.lisp ends here
