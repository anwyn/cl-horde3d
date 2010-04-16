;;; terrain-bindings.lisp --- bindings to the horde3d terrain extension
;;;  _                      _             _     _           _ _
;;; | |_ ___ _ __ _ __ __ _(_)_ __       | |__ (_)_ __   __| (_)_ __   __ _ ___
;;; | __/ _ \ '__| '__/ _` | | '_ \ _____| '_ \| | '_ \ / _` | | '_ \ / _` / __|
;;; | ||  __/ |  | | | (_| | | | | |_____| |_) | | | | | (_| | | | | | (_| \__ \
;;;  \__\___|_|  |_|  \__,_|_|_| |_|     |_.__/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;;                                                                   |___/
;;;
;;; Copyright (C) 2009 Ole Arndt <anwyn@sugarshark.com>
;;;

(cl:defpackage #:horde3d-terrain-cffi
  (:nicknames #:%h3d-terrain)
  (:use #:cffi #:%h3d)
  (:import-from #:common-lisp
                #:in-package
                #:eval-when
                #:push
                #:*features*)
  (:export
   #:add-terrain-node
   #:create-geometry-resource))

(in-package :horde3d-terrain-cffi)

(defcenum terrain-node-type
  "The available scene node types.

 :terrain         - Terrain node
"
  (:terrain 100))

(deftypedenum terrain-node-parameters
    "The available Terrain node parameters:

 :terrain-height-map-resource - Height map texture; must be square and a power of two [type: resource, write-only]
 :terrain-material-resource   - Material resource used for rendering the terrain [type: resource]
 :terrain-mesh-quality        - Constant controlling the overall resolution of the terrain mesh (default: 50.0) [type: float]
 :terrain-skirt-height        - Height of the skirts used to hide cracks (default: 0.1) [type: float]
 :terrain-block-size          - Size of a terrain block that is drawn in a single render call; must be 2^n+1 (default: 17) [type: int]
"
  (:terrain-height-map-resource 10000) :resource
  :terrain-material-resource :resource
  :terrain-mesh-quality :float
  :terrain-skirt-height :float
  :terrain-block-size :int)

;;;; Group: Terrain Extension

;; NodeHandle addTerrainNode( NodeHandle parent, const char *name, ResHandle heightMapRes, ResHandle materialRes );
(defh3fun ("h3dAddTerrainNode" add-terrain-node) node
  "Adds a Terrain node to the scene.

This function creates a new Terrain node and attaches it to the specified parent node.

Parameters:
        parent     - handle to parent node to which the new node will be attached
        name       - name of the node
        height-map - handle to a Texture2D resource that contains the terrain height information (must be square and POT)
        material   - handle to the Material resource used for rendering the terrain

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node) (name string) (height-map resource) (material resource))

;; ResHandle createGeometryResource( NodeHandle node, const char *resName, float meshQuality );
(defh3fun ("h3dCreateGeometryResource" create-geometry-resource) resource
  "Creates a Geometry resource from a specified Terrain node.

This function creates a new Geometry resource that contains the vertex data of the specified Terrain node.
To reduce the amount of data, it is possible to specify a quality value which controls the overall resolution
of the terrain mesh. The algorithm will automatically create a higher resoultion in regions where the
geometrical complexity is higher and optimize the vertex count for flat regions.

Parameters:
        node            - handle to terrain node that will be accessed
        resName         - name of the Geometry resource that shall be created
        meshQuality - constant controlling the overall mesh resolution

Returns:
         handle to the created Geometry resource or 0 in case of failure
"
  (node node) (res-name string) (mesh-quality float))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push :horde3d-terrain-extension *features*))

;;; terrain-bindings.lisp ends here
