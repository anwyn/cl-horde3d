;;; bindings.lisp --- bindings for the core of the Horde3D engine
;;;  _     _           _ _
;;; | |__ (_)_ __   __| (_)_ __   __ _ ___
;;; | '_ \| | '_ \ / _` | | '_ \ / _` / __|
;;; | |_) | | | | | (_| | | | | | (_| \__ \
;;; |_.__/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;;                              |___/
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :horde3d)

;;;; Group: Typedefs and constants

(defctype resource-handle :int
  "Handle to a resource (int).")

(defctype node-handle :int
  "Handle to a scene node (int).")

;;;; Constants: Predefined constants

(defconstant +root-node+ 
  "Scene root node handle.")

;;;; Group: Enumerations

(defcenum engine-options
  "The available engine option parameters.

 :max-log-level       - Defines the maximum log level; only messages which are smaller or equal to this value
                        (hence more important) are published in the message queue. (Default: 4)
 :max-num-messages    - Defines the maximum number of messages that can be stored in the message queue (Default: 512)
 :trilinear-filtering - Enables or disables trilinear filtering for textures; only affects textures
                        that are loaded after setting the option. (Values: 0, 1; Default: 1)
 :anisotropy-factor   - Sets the quality for anisotropic filtering; only affects textures that
                        are loaded after setting the option. (Values: 1, 2, 4, 8; Default: 1)
 :tex-compression     - Enables or disables texture compression; only affects textures that are
                        loaded after setting the option. (Values: 0, 1; Default: 0)
 :load-textures       - Enables or disables loading of texture images; option can be used to
                        minimize loading times for testing. (Values: 0, 1; Default: 1)
 :fast-animation      - Disables or enables inter-frame interpolation for animations. (Values: 0, 1; Default: 1)
 :shadowmap-size      - Sets the size of the shadow map buffer (Values: 128, 256, 512, 1024, 2048; Default: 1024)
 :sample-count        - Maximum number of samples used for multisampled render targets; only affects pipelines
                        that are loaded after setting the option. (Values: 0, 2, 4, 8, 16; Default: 0)
 :wireframe-mode      - Enables or disables wireframe rendering
 :debug-view-mode     - Enables or disables debug view where geometry is rendered in wireframe without shaders and
                        lights are visualized using their screen space bounding box. (Values: 0, 1; Default: 0)
"
  (:max-log-level 1)
  :max-num-messages
  :trilinear-filtering
  :anisotropy-factor
  :tex-compression
  :load-textures
  :fast-animation
  :shadow-map-size
  :sample-count
  :wireframe-mode
  :debug-view-mode)

(defcenum engine-stats
  "The available engine statistic parameters.

 :tri-count        - Number of triangles that were pushed to the renderer
 :batch-count      - Number of batches (draw calls)
 :light-pass-count - Number of lighting passes
"
  (:tri-count 100)
  :batch-count
  :light-pass-count)

(defcenum resource-type
  "Enum: resource-type - The available resource types.

 :undefined    - An undefined resource, returned by getResourceType in case of error
 :scene-graph  - Scene graph subtree stored in XML format
 :geometry     - Geometrical data containing bones, vertices and triangles
 :animation    - Animation data
 :material     - Material script
 :code         - Text block containing shader source code
 :shader       - Shader program
 :texture-2d   - Two-dimensional texture map
 :texture-cube - Cube map texture
 :effect       - Particle configuration
 :pipeline     - Rendering pipeline
"
  (:undefined  0)
  :scene-graph
  :geometry
  :animation
  :material
  :code
  :shader
  :texture-2d
  :texture-cube
  :effect
  :pipeline)

(defbitfield resource-flags
  "Enum: resource-flags - The available flags used when adding a resource.

   NoQuery                 - Excludes resource from being listed by queryUnloadedResource function.
   NoTexPOTConversion      - Disables texture conversion to power-of-two dimensions on hardware without NPOT-support.
   NoTexCompression        - Disables texture compression for Texture2D or TextureCube resource.
   NoTexMipmaps            - Disables generation of mipmaps for textures.
   NoTexFiltering          - Disables bilinear filtering for textures.
   NoTexRepeat             - Disables tiling (repeat mode) for textures and enables clamping instead.
"
  (:no-query #x0001)
  :no-tex-potconversion
  :no-tex-compression
  :no-tex-mipmaps
  :no-tex-filtering
  (:no-tex-repeat #x0003))

(defcenum resource-parameter
    "
The available Geometry resource parameters:

   :vertex-count - Number of vertices; valid for getResourceParami
   :index-count  - Number of triangle indices; valid for getResourceParami
   :vertex-data  - Vertex positon data (pointer to float); valid for getResourceData
   :index-data   - Triangle indices (pointer to uint); valid for getResourceData

 The available Animation resource parameters:

   :frame-count   - Number of animation frames; valid for getResourceParami

 The available Material resource parameters:

   :class           - Hierarchical class name (Default: empty string);
                             valid for get-/setResourceParamstr
   :link        - Link to other material resource;
                             valid for get-/setResourceParami
   :shader      - Shader resource used for rendering;
                             valid for get-/setResourceParami
   :tex-unit-0       - Texture resource for the first unit;
                             valid for get-/setResourceParami
   :tex-unit-1       - Texture resource for the second unit;
                             valid for get-/setResourceParami
   :tex-unit-2       - Texture resource for the third unit;
                             valid for get-/setResourceParami
   :tex-unit-3       - Texture resource for the fourth unit;
                             valid for get-/setResourceParami
   :tex-unit-4       - Texture resource for the fifth unit;
                             valid for get-/setResourceParami
   :tex-unit-5       - Texture resource for the sixth unit;
                             valid for get-/setResourceParami
   :tex-unit-6       - Texture resource for the seventh unit;
                             valid for get-/setResourceParami
   :tex-unit-7       - Texture resource for the eighth unit;
                             valid for get-/setResourceParami
   :tex-unit-8       - Texture resource for the ninth unit;
                             valid for get-/setResourceParami
   :tex-unit-9       - Texture resource for the tenth unit;
                             valid for get-/setResourceParami
   :tex-unit-10      - Texture resource for the eleventh unit;
                             valid for get-/setResourceParami
   :tex-unit-11      - Texture resource for the twelfth unit;
                                       valid for get-/setResourceParami

   The available Texture2D and TextureCube resource parameters.

   :pixel-data - Image pixel data (pointer to unsigned char); valid for updateResourceData for Texture2D
   :width     - Image width in pixels; valid for getResourceParami
   :height    - Image height in pixels; valid for getResourceParami
   :comps     - Number of channels in image (e.g. an RGBA image has 4 channels); valid for getResourceParami
   :hdr        - Flag indicating whether the texture is a HDR image (returns 1) or a usual 8 bit per channel image (returns 0);
                 valid for getResourceParami

   The available Effect resource parameters.

   :life-min          - Minimum value for selecting random life time; valid for get-/setResourceParamf
   :life-max              - Maximum value for selecting random life time; valid for get-/setResourceParamf
   :move-vel-min       - Minimum value for selecting random initial value of velocity defining
                                          how many units per second particle is moving; valid for get-/setResourceParamf
   :move-vel-max       - Maximum value for selecting random initial value of velocity defining
                                          how many units per second particle is moving; valid for get-/setResourceParamf
   :move-vel-end-rate   - Percentage of the initial translation velocity value when particle is dying;
                                          valid for get-/setResourceParamf
   :rot-vel-min        - Minimum value for selecting random initial value of velocity defining
                                          how many degrees per second particle is rotating; valid for get-/setResourceParamf
   :rot-vel-max        - Maximum value for selecting random initial value of velocity defining
                                          how many degrees per second particle is rotating; valid for get-/setResourceParamf
   :rot-vel-end-rate    - Percentage of the initial rotation velocity value when particle is dying;
                                          valid for get-/setResourceParamf
   :size-vel-min       - Minimum value for selecting random initial size value;
                      valid for get-/setResourceParamf
   :size-vel-max       - Maximum value for selecting random initial size value;
                                          valid for get-/setResourceParamf
   :size-vel-end-rate   - Percentage of the initial size value when particle is dying;
                                          valid for get-/setResourceParamf
   :col-r-min        - Minimum value for selecting random initial red color value;
                                          valid for get-/setResourceParamf
   :col-r-max        - Maximum value for selecting random initial red color value;
                                          valid for get-/setResourceParamf
   :col-r-end-rate    - Percentage of the initial red value when particle is dying;
                                          valid for get-/setResourceParamf
   :col-g-min        - Minimum value for selecting random initial green color value;
                                          valid for get-/setResourceParamf
   :col-g-max        - Maximum value for selecting random initial green color value;
                                          valid for get-/setResourceParamf
   :col-g-end-rate    - Percentage of the initial green value when particle is dying;
                                          valid for get-/setResourceParamf
   :col-b-min        - Minimum value for selecting random initial blue color value;
                                          valid for get-/setResourceParamf
   :col-b-max        - Maximum value for selecting random initial blue color value;
                                          valid for get-/setResourceParamf
   :col-b-end-rate    - Percentage of the initial blue value when particle is dying;
                                          valid for get-/setResourceParamf
   :col-a-min        - Minimum value for selecting random initial alpha color value;
                                          valid for get-/setResourceParamf
   :col-a-max        - Maximum value for selecting random initial alpha color value;
                                          valid for get-/setResourceParamf
   :col-a-end-rate    - Percentage of the initial alpha value when particle is dying;
                                          valid for get-/setResourceParamf

"
  (:vertex-count 200)
  :index-count
  :vertex-data
  :index-data

  (:frame-count 300)

  (:class 400)
  :link
  :shader
  :tex-unit-0
  :tex-unit-1
  :tex-unit-2
  :tex-unit-3
  :tex-unit-4
  :tex-unit-5
  :tex-unit-6
  :tex-unit-7
  :tex-unit-8
  :tex-unit-9
  :tex-unit-10
  :tex-unit-11

  (:pixel-data 700)
  :width
  :height
  :comps
  :hdr
  
  (:life-min 900)
  :life-max
  :move-vel-min
  :move-vel-max
  :move-vel-end-rate
  :rot-vel-min
  :rot-vel-max
  :rot-vel-end-rate
  :size-min
  :size-max
  :size-end-rate
  :col-r-min
  :col-r-max
  :col-r-end-rate
  :col-g-min
  :col-g-max
  :col-g-end-rate
  :col-b-min
  :col-b-max
  :col-b-end-rate
  :col-a-min
  :col-a-max
  :col-a-end-rate)

(defcenum node-type
  "Enum: node-type: The available scene node types.

   :undefined       - An undefined node type, returned by getNodeType in case of error
   :group           - Group of different scene nodes
   :model           - 3D model with optional skeleton
   :mesh            - Subgroup of a model with triangles of one material
   :joint           - Joint for skeletal animation
   :light           - Light source
   :camera          - Camera giving view on scene
   :emitter         - Particle system emitter
   :terrain         - Terrain node
"
  (:undefined 0)
  :group
  :model
  :mesh
  :joint
  :light
  :camera
  :emitter
  (:terrain 100))


(defcenum node-parameter
  "
The available scene node parameters:

   :name                 - Name of the scene node [type: string]
   :attachment-string    - Optional application-specific meta data for a node
                           encapsulated in an 'Attachment' XML string [type: string]

The available Group node parameters:

   :min-dist             - Minimal distance from the viewer for the node to be visible
                           (default: 0.0); used for level of detail [type: float]
   :max-dist             - Maximal distance from the viewer for the node to be visible
                           (default: infinite); used for level of detail [type: float]

The available Model node parameters:

   :geometry             - Geometry resource used for the model [type: ResHandle]
   :software-skinning    - Enables or disables software skinning (default: 0) [type: int]

   The available Mesh node parameters:

   :mesh-material        - Material resource used for the mesh [type: ResHandle]
   :batch-start          - First triangle index of mesh in Geometry resource of parent Model node [type: int, read-only]
   :batch-count          - Number of triangle indices used for drawing mesh [type: int, read-only]
   :vert-r-start         - First vertex in Geometry resource of parent Model node [type: int, read-only]
   :vert-r-end           - Last vertex in Geometry resource of parent Model node [type: int, read-only]

The available Joint node parameters:

   :joint-index          - Index of joint in Geometry resource of parent Model node [type: int, read-only]

The available Light node parameters:

   :light-material       - Material resource used for the light [type: ResHandle]
   :radius               - Radius of influence (default: 100.0) [type: float]
   :fov                  - Field of view (FOV) angle (default: 90.0) [type: float]
   :col-r                - Red component of light diffuse color (default: 1.0) [type: float]
   :col-g                - Green component of light diffuse color (default: 1.0) [type: float]
   :col-b                - Blue component of light diffuse color (default: 1.0) [type: float]
   :shadow-map-count     - Number of shadow maps used for light source (values: 0, 1, 2, 3, 4; default: 0) [type: int]
   :shadow-split-lambda  - Constant determining segmentation of view frustum for Parallel Split Shadow Maps
                                             (default: 0.5) [type: float]
   :shadow-map-bias      - Bias value for shadow mapping to reduce shadow acne (default: 0.005) [type: float]

The available Camera node parameters:

   :pipeline             - Pipeline resource used for rendering [type: ResHandle]
   :output-tex           - Texture2D resource used as output buffer (can be 0 to use main framebuffer) (default: 0) [type: ResHandle]
   :output-buffer-index  - Index of the output buffer for stereo rendering (values: 0 for left eye, 1 for right eye) (default: 0) [type: int]
   :left-plane           - Coordinate of left plane relative to near plane center (default: -0.055228457) [type: float]
   :right-plane          - Coordinate of right plane relative to near plane center (default: 0.055228457) [type: float]
   :bottom-plane         - Coordinate of bottom plane relative to near plane center (default: -0.041421354f) [type: float]
   :top-plane            - Coordinate of top plane relative to near plane center (default: 0.041421354f) [type: float]
   :near-plane           - Distance of near clipping plane (default: 0.1) [type: float]
   :far-plane            - Distance of far clipping plane (default: 1000) [type: float]
   :orthographic         - Flag for setting up an orthographic frustum instead of a perspective one (default: 0) [type:int]
   :occlusion-culling    - Flag for enabling occlusion culling (default: 0) [type:int]

The available Emitter node parameters:

   :emitter-material     - Material resource used for rendering [type: ResHandle]
   :effect-res           - Effect resource which configures particle properties [type: ResHandle]
   :max-count            - Maximal number of particles living at the same time [type: int]
   :respawn-count        - Number of times a single particle is recreated after dying (-1 for infinite) [type: int]
   :delay                - Time in seconds before emitter begins creating particles (default: 0.0) [type: float]
   :emission-rate        - Maximal number of particles to be created per second (default: 0.0) [type: float]
   :spread-angle         - Angle of cone for random emission direction (default: 0.0) [type: float]
   :force-X              - X-component of force vector applied to particles (default: 0.0) [type: float]
   :force-Y              - Y-component of force vector applied to particles (default: 0.0) [type: float]
   :force-Z              - Z-component of force vector applied to particles (default: 0.0) [type: float]

The available Terrain node parameters:
		
   :height-map-res       - Height map texture; must be square and a power of two [type: ResHandle, write-only]
   :material-res         - Material resource used for rendering the terrain [type: ResHandle]
   :mesh-quality         - Constant controlling the overall resolution of the terrain mesh (default: 50.0) [type: float]
   :skirt-height         - Height of the skirts used to hide cracks (default: 0.1) [type: float]
   :block-size           - Size of a terrain block that is drawn in a single render call; must be 2^n+1 (default: 17) [type: int]

"
  ;; scene-node-params
  (:name 1)
  :attachment-string

  ;; group-node-params
  (:min-dist 100)
  :max-dist

  ;; model-node-params
  (:geometry 200)
  :software-skinning

  ;; mesh-node-params
  (:mesh-material 300)
  :batch-start
  :batch-count
  :vert-r-start
  :vert-r-end

  ;; joint-node-params
  (:joint-index 400)
  
  ;; light-node-params
  (:light-material 500)
  :radius
  :fov
  :col-r
  :col-g
  :col-b
  :shadow-map-count
  :shadow-split-lambda
  :shadow-map-bias

  ;; camera-node-params
  (:pipeline 600)
  :output-tex
  :output-buffer-index
  :left-plane
  :right-plane
  :bottom-plane
  :top-plane
  :near-plane
  :far-plane
  :orthographic
  :occlusion-culling
  
  ;; emitter-node-params
  (:emitter-material 700)
  :effect-res
  :max-count
  :respawn-count
  :delay
  :emission-rate
  :spread-angle
  :force-X
  :force-Y
  :force-Z
  ;; terrain node params

  (:height-map-res 10000)
  :terrain-material
  :mesh-quality
  :skirt-height
  :block-size)


;;; Helper macro to define a horde3d API function and declare it inline.
(defmacro defh3fun ((cname lname) result-type &body body)
  `(progn
     (declaim (inline ,lname))
     (defcfun (,cname ,lname :library horde3d) ,result-type ,@body)))

;;;; Group: Basic functions

(defh3fun ("getVersionString" get-version-string) :string
  "Returns the engine version string.

This function returns a pointer to a string containing the current version of
Horde3D.

Parameters:
      none

Returns:
      pointer to the version string
")

(defh3fun ("checkExtension" check-extension) :boolean
  "Checks if an extension is part of the engine library.

This function checks if a specified extension is contained in the DLL/shared
object of the engine.

Parameters:
    extensionName   - name of the extension

Returns:
    true if extension is implemented, otherwise false
"
  (extension-name :string))

;; bool init();
(defh3fun ("init" init) :void
  "Initializes the engine.

This function initializes the graphics engine and makes it ready for use. It has
to be the first call to the engine except for getVersionString. In order to
successfully initialize the engine the calling application must provide a valid
OpenGL context. The function can be called several times on different rendering
contexts in order to initialize them.

Parameters:
        none

Returns:
        true in case of success, otherwise false
")

;; void release();
(defh3fun ("release" release) :void
  "Releases the engine.

This function releases the engine and frees all objects and associated
memory. It should be called when the application is destroyed.

Parameters:
        none

Returns:
        nothing
")


;; void resize( int x, int y, int width, int height );
(defh3fun ("resize" resize) :void
  "Resizes the viewport.

This function sets the dimensions of the rendering viewport. It has to be called
after initialization and whenever the viewport size changes.

Parameters:
        x               - the x-position of the viewport in the rendering context
        y               - the y-position of the viewport in the rendering context
        width   - the width of the viewport
        height  - the height of the viewport

Returns:
        nothing
"
  (x :int)
  (y :int)
  (width :int)
  (heigth :int))



;; bool render( NodeHandle cameraNode );
(defh3fun ("render" render) :boolean
  "Main rendering function.

This is the main function of the engine. It executes all the rendering,
animation and other tasks. The function can be called several times per frame,
for example in order to write to different output buffers.

Parameters:
        cameraNode      - camera node used for rendering scene

Returns:
        true in case of success, otherwise false
"
  (camera-node node-handle))


;; void clear();
(defh3fun ("clear" clear) :void
  "Removes all resources and scene nodes.

This function removes all nodes from the scene graph except the root node and
releases all resources.

Warning: All resource and node IDs are invalid after calling this function*

Parameters:
        none

Returns:
        nothing
")



;; const char *getMessage( int *level, float *time );
(defh3fun ("getMessage" get-message) :string
  "Gets the next message from the message queue.

This function returns the next message string from the message queue and writes
additional information o the specified variables. If no message is left over in
the queue an empty string is returned.

Parameters:
        level   - pointer to variable for storing message level indicating importance (can be NULL)
        time    - pointer to variable for storing time when message was added (can be NULL)

Returns:
        message string or empty string if no message is in queue
"
  (level (:pointer :int))
  (time (:pointer :float)))


;; float getOption( EngineOptions::List param );
(defh3fun ("getOption" get-option) :float
  "Gets an option parameter of the engine.

This function gets a specified option parameter and returns its value.

Parameters:
        param   - option parameter

Returns:
        current value of the specified option parameter
"
  (param engine-options))


;; bool setOption( EngineOptions::List param, float value );
(defh3fun ("setOption" set-option) :boolean
  "Sets an option parameter for the engine.

This function sets a specified option parameter to a specified value.

Parameters:
        param   - option parameter
        value   - value of the option parameter

Returns:
        true if the option could be set to the specified value, otherwise false
"
  (param engine-options)
  (value :float))



;; float getStat( EngineStats::List param, bool reset );
(defh3fun ("getStat" get-stat) :float
  "Gets a statistic value of the engine.

This function returns the value of the specified statistic. The reset flag makes
it possible to reset the statistic value after reading.

Parameters:
        param   - statistic parameter
        reset   - flag specifying whether statistic value should be reset

Returns:
        current value of the specified statistic parameter
"
  (param engine-stats)
  (reset :boolean))


;; void showOverlay( float x_ll, float y_ll, float u_ll, float v_ll,
;;                                                float x_lr, float y_lr, float u_lr, float v_lr,
;;                                                float x_ur, float y_ur, float u_ur, float v_ur,
;;                                                float x_ul, float y_ul, float u_ul, float v_ul,
;;                                                int layer, ResHandle materialRes );
(defh3fun ("showOverlay" show-overlay) :void
  "Shows an overlay on the screen.

This function displays an overlay with a specified material at a specified
position on the screen.  n overlay is a 2D image that can be used to render 2D
GUI elements. The coordinate system sed has its origin (0, 0) at the lower left
corner of the screen and its maximum (1, 1) at he upper right corner. Texture
coordinates are using the same system, where the coordinates 0, 0) correspond to
the lower left corner of the image.  verlays can have different layers which
describe the order in which they are drawn. Overlays with maller layer numbers
are drawn before overlays with higher layer numbers.  ote that the overlays have
to be removed manually using the function clearOverlays.

Parameters:
        x_ll, y_ll, u_ll, v_ll  - position and texture coordinates of the lower left corner
        x_lr, y_lr, u_lr, v_lr  - position and texture coordinates of the lower right corner
        x_ur, y_ur, u_ur, v_ur  - position and texture coordinates of the upper right corner
        x_ul, y_ul, u_ul, v_ul  - position and texture coordinates of the upper left corner
        layer                                   - layer index of the overlay (Values: from 0 to 7)
        materialRes                             - material resource used for rendering

Returns:
        nothing
"
  (x-ll :float) (y-ll :float) (u-ll :float)
  (v-ll :float) (x-lr :float) (y-lr :float)
  (u-lr :float) (v-lr :float) (x-ur :float)
  (y-ur :float) (u-ur :float) (v-ur :float)
  (x-ul :float) (y-ul :float) (u-ul :float)
  (v-ul :float) (layer :int)
  (material-res resource-handle))


;; void clearOverlays();
(defh3fun ("clearOverlays" clear-overlays) :void
  "Removes all overlays.

This function removes all overlays that were added using showOverlay.

Parameters:
        none

Returns:
        nothing
")


;;;;  Group: General resource management functions


;; int getResourceType( ResHandle res );
(defh3fun ("getResourceType" get-resource-type) :int
  "Returns the type of a resource.

This function returns the type of a specified resource. If the resource handle
is invalid, he function returns the resource type 'Unknown'.

Parameters:
        res     - handle to the resource

Returns:
        type of the resource
"
  (res resource-handle))


;; const char *getResourceName( ResHandle res );
(defh3fun ("getResourceName" get-resource-name) :string
  "Returns the name of a resource.

This function returns a pointer to the name of a specified resource. If the
resource handle is invalid, the function returns an empty string.

Important Note: The pointer is const and allows only read access to the data. Do never try to modify the
ata of the pointer since that can corrupt the engine's internal states!*

Parameters:
        res     - handle to the resource

Returns:
        name of the resource or empty string in case of failure
"
  (res resource-handle))


;; ResHandle findResource( int type, const char *name );
(defh3fun ("findResource" find-resource) resource-handle
  "Finds a resource and returns its handle.

This function searches the resource of the specified type and name and returns
its handle. If he resource is not available in the resource manager a zero
handle is returned.

Parameters:
        type    - type of the resource
        name    - name of the resource

Returns:
        handle to the resource or 0 if not found
"
  (type resource-type)
  (name :string))


;; ResHandle addResource( int type, const char *name, int flags );
(defh3fun ("addResource" add-resource) resource-handle
  "Adds a resource.

This function tries to add a resource of a specified type and name to the
 resource manager. If resource of the same type and name is already existing,
 the handle to the existing resource is returned nstead of creating a new one
 and the user reference count of the resource is increased.

*Note: The name string may not contain a colon character (:)*

Parameters:
        type    - type of the resource
        name    - name of the resource
        flags   - flags used for creating the resource

Returns:
        handle to the resource to be added or 0 in case of failure
"
  (type resource-type)
  (name :string)
  (flags resource-flags))


;; ResHandle cloneResource( ResHandle sourceRes, const char *name );
(defh3fun ("cloneResource" clone-resource) resource-handle
  "Duplicates a resource.

This function duplicates a specified resource. In the cloning process a new
resource with the specified name is added to the resource manager and filled
with the data of the specified source resource. If the specified name for the
new resource is already in use, the function fails and returns 0. If the name
string is empty, a unique name for the resource is generated automatically.

*Note: The name string may not contain a colon character (:)*

Parameters:
        sourceRes       - handle to resource to be cloned
        name            - name of new resource (can be empty for auto-naming)

Returns:
        handle to the cloned resource or 0 in case of failure
"
  (source-res resource-handle)
  (name :string))


;; int removeResource( ResHandle res );
(defh3fun ("removeResource" remove-resource) :int
  "Removes a resource.

This function decreases the user reference count of a specified resource. When
the user reference count is zero and there are no internal references, the
resource can be released and removed using the API fuction
releaseUnusedResources.

Parameters:
        res     - handle to the resource to be removed

Returns:
        the number of references that the application is still holding after removal or -1 in case of an error
"
  (res resource-handle))


;; bool isResourceLoaded( ResHandle res );
(defh3fun ("isResourceLoaded" is-resource-loaded) :boolean
  "Checks if a resource is loaded.

This function checks if the specified resource has been successfully loaded.

Parameters:
        res             - handle to the resource to be checked

Returns:
        true if resource is loaded, otherwise or in case of failure false
"
  (res resource-handle))


;; bool loadResource( ResHandle res, const char *data, int size );
(defh3fun ("loadResource" load-resource) :boolean
  "Loads a resource.

This function loads data for a resource that was previously added to the
resource manager.  f data is a NULL-pointer the resource manager is told that
the resource doesn't have any data
e.g. the corresponding file was not found. In this case, the resource remains in the unloaded state
but is no more returned when querying unloaded resources. When the specified resource is already loaded,
the function returns false.

Important Note: XML-data must be NULL-terminated*

Parameters:
        res             - handle to the resource for which data will be loaded
        data    - pointer to the data to be loaded
        size    - size of the data block

Returns:
        true in case of success, otherwise false
"
  (res resource-handle)
  (data :pointer)
  (size :int))


;; bool unloadResource( ResHandle res );
(defh3fun ("unloadResource" unload-resource) :boolean
  "Unloads a resource.

This function unloads a previously loaded resource and restores the default
values t had before loading. The state is set back to unloaded which makes it
possible to load he resource again.

Parameters:
        res     - handle to resource to be unloaded

Returns:
        true in case of success, otherwise false
"
  (res resource-handle))



;; int getResourceParami( ResHandle res, int param );
(defh3fun ("getResourceParami" get-resource-parami) :int
  "Gets a property of a resource.

This function returns a specified property of the specified resource.  The
property must be of the type int.

Parameters:
        res     - handle to the resource to be accessed
        param   - parameter to be accessed

Returns:
        value of the parameter
"
  (res resource-handle) (param resource-parameter))


;; bool setResourceParami( ResHandle res, int param, int value );
(defh3fun ("setResourceParami" set-resource-parami) :boolean
  "Sets a property of a resource.

This function sets a specified property of the specified resource to a specified
value.  The property must be of the type int.

Parameters:
        res     - handle to the resource to be accessed
        param   - parameter to be modified
        value   - new value for the specified parameter

Returns:
         true in case of success otherwise false
"
  (res resource-handle) (param resource-parameter) (value :int))


;; float getResourceParamf( ResHandle res, int param );
(defh3fun ("getResourceParamf" get-resource-paramf) :float
  "Gets a property of a resource.

This function returns a specified property of the specified resource.  The
property must be of the type float.

Parameters:
        res             - handle to the resource to be accessed
        param   - parameter to be accessed

Returns:
        value of the parameter
"
  (res resource-handle) (param resource-parameter))



;; bool setResourceParamf( ResHandle res, int param, float value );
(defh3fun ("setResourceParamf" set-resource-paramf) :boolean
  "Sets a property of a resource.

This function sets a specified property of the specified resource to a specified
value.  The property must be of the type float.

Parameters:
        node    - handle to the node to be modified
        param   - parameter to be modified
        value   - new value for the specified parameter

Returns:
         true in case of success otherwise false
"
  (res resource-handle) (param resource-parameter) (value :float))


;; const char *getResourceParamstr( ResHandle res, int param );
(defh3fun ("getResourceParamstr" get-resource-paramstr) :string
  "Gets a property of a resource.

This function returns a specified property of the specified resource.  The
property must be of the type string (const char *).

*Important Note: The pointer is const and allows only read access to the data. Do never try to modify the
ata of the pointer since that can corrupt the engine's internal states!*

Parameters:
        res             - handle to the resource to be accessed
        param   - parameter to be accessed

Returns:
        value of the property or empty string if no such property exists
"
  (res resource-handle) (param resource-parameter))


;; bool setResourceParamstr( ResHandle res, int param, const char *value );
(defh3fun ("setResourceParamstr" set-resource-paramstr) :boolean
  "Sets a property of a resource.

This function sets a specified property of the specified resource to a specified
value.  The property must be of the type string (const char *).

Parameters:
        node    - handle to the node to be modified
        param   - parameter to be modified
        value   - new value for the specified parameter

Returns:
         true in case of success otherwise false
"
  (res resource-handle) (param resource-parameter) (value :string))


;; const void *getResourceData( ResHandle res, int param );
(defh3fun ("getResourceData" get-resource-data) :string
  "Gives access to resource data.

This function returns a pointer to the specified data of a specified
resource. For information on the ormat (uint, float, ..) of the pointer see the
ResourceData description.

Important Note: The pointer is const and allows only read access to the data. Do never try to modify the
ata of the pointer since that can corrupt the engine's internal states!*

Parameters:
        res             - handle to the resource to be accessed
        param   - parameter indicating data of the resource that will be accessed

Returns:
        pointer to the specified resource data if it is available, otherwise NULL-pointer
"
  (res resource-handle) (param resource-parameter))


;; bool updateResourceData( ResHandle res, int param, const void *data, int size );
(defh3fun ("updateResourceData" update-resource-data) :boolean
  "Updates the data of a resource.

 This function updates the content of a resource that was successfully loaded
 before. The new data must have exactly the same data layout as the data that
 was loaded.

 Notes on available ResourceData parameters:
  - Texture2DResData::PixelData
         Sets the image data of a Texture2D resource. The data pointer must
         point to a memory block that contains the pixels of the image. Each
         pixel needs to have 32 bit color data in BGRA format and the dimensions
         of the image (width, height) must be exactly the same as the dimensions
         of the image that was originally loaded for the resource. The first
         element in the data array corresponds to the lower left corner of the
         image and subsequent elements progress from left to right in the image.

  Parameters:
          res     - handle to the resource for which the data is modified
          param   - data structure which will be updated
          data    - pointer to the new data
          size    - size of the new data block

  Returns:
          true in case of success, otherwise false
"
  (res resource-handle) (param resource-parameter) (data :pointer) (size :int))


;; ResHandle queryUnloadedResource( int index );
(defh3fun ("queryUnloadedResource" query-unloaded-resource) resource-handle
  "Returns handle to an unloaded resource.

This function looks for a resource that is not yet loaded and returns its
handle.  If there are no unloaded resources or the zero based index specified is
greater than the number of the currently unloaded resources, 0 is returned.

Parameters:
        index    - index of unloaded resource within the internal list of unloaded resources (starting with 0)

Returns:
        handle to an unloaded resource or 0
"
  (index :int))


;; void releaseUnusedResources();
(defh3fun ("releaseUnusedResources" release-unused-resources) :void
  "Frees resources that are no longer used.

This function releases resources that are no longer used. Unused resources were
either told to be released by the user calling removeResource or are no more
referenced by any other engine objects.

Parameters:
        none

Returns:
        nothing
")

;;;;  Group: Specific resource management functions 

;; ResHandle createTexture2D( const char *name, int flags, int width, int height, bool renderable );
(defh3fun ("createTexture2D" create-texture-2d) resource-handle
  "Adds a Texture2D resource.

This function tries to create and add a Texture2D resource with the specified
name to the resource manager. If a Texture2D resource with the same name is
already existing, the function fails. The texture is initialized with the
specified dimensions and the resource is declared as loaded. This function is
especially useful to create dynamic textures (e.g. for displaying videos) or
output buffers for render-to-texture.

*Note: The name string may not contain a colon character (:)*

Parameters:
        name            - name of the resource
        flags           - flags used for creating the resource
        width           - width of the texture image
        height          - height of the texture image
        renderable      - flag indicating whether the texture can be used as an output buffer for a Camera node

Returns:
        handle to the created resource or 0 in case of failure
"
  (name :string)
  (flags resource-flags)
  (width :int)
  (height :int)
  (renderable :boolean))


;; void setShaderPreambles( const char *vertPreamble, const char *fragPreamble );
(defh3fun ("setShaderPreambles" set-shader-preambles) :void
  "Sets preambles of all Shader resources.

This function defines a header that is inserted at the beginning of all
shaders. The preamble is used when a shader is compiled, so changing it will not
affect any shaders that are already compiled. The preamble is useful for setting
platform-specific defines that can be employed for creating several shader code
paths, e.g. for supporting different hardware capabilities.

Parameters:
        vertPreamble    - preamble text of vertex shaders (default: empty string)
        fragPreamble    - preamble text of fragment shaders (default: empty string)

Returns:
        nothing
"
  (vert-preamble :string)
  (frag-preamble :string))


;; bool setMaterialUniform( ResHandle materialRes, const char *name, float a, float b, float c, float d );
(defh3fun ("setMaterialUniform" set-material-uniform) :boolean
  "Sets a shader uniform of a Material resource.

This function sets the specified shader uniform of the specified material to the
specified values.

Parameters:
        materialRes     - handle to the Material resource to be accessed
        name            - name of the uniform as defined in Material resource
        a, b, c, d      - values of the four components

Returns:
        true in case of success, otherwise false
"
  (material-res resource-handle) (name :string) (a :float) (b :float) (c :float) (d :float))


;; bool setPipelineStageActivation( ResHandle pipelineRes, const char *stageName, bool enabled );
(defh3fun ("setPipelineStageActivation" set-pipeline-stage-activation) :boolean
  "Sets the activation state of a pipeline stage.

This function enables or disables a specified stage of the specified pipeline
resource.

Parameters:
        pipelineRes     - handle to the Pipeline resource to be accessed
        stageName       - name of the stage to be modified
        enabled         - flag indicating whether the stage shall be enabled or disabled

Returns:
        true in case of success, otherwise false
"
  (pipeline-res resource-handle)
  (stage-name :string)
  (enabled :boolean))


;; bool getPipelineRenderTargetData( ResHandle pipelineRes, const char *targetName,
;;                                            int bufIndex, int *width, int *height, int *compCount,
;;                                                                                float *dataBuffer, int bufferSize );
(defh3fun ("getPipelineRenderTargetData" get-pipeline-render-target-data) :boolean
  "Reads the pixel data of a pipeline render target buffer.

This function reads the pixels of the specified buffer of the specified render
target from the specified pipeline resource and stores it in the specified float
array. To calculate the size required for the array this function can be called
with a NULL pointer for dataBuffer and pointers to variables where the width,
height and number of (color) components (e.g. 4 for RGBA or 1 for depth) will be
stored.  The function is not intended to be used for real-time scene rendering
but rather as a tool for debugging.  For more information about the render
buffers please refer to the Pipeline documentation.

Parameters:
        pipelineRes     - handle to pipeline resource
        targetName      - unique name of render target to access
        bufIndex        - index of buffer to be accessed
        width           - pointer to variable where the width of the buffer will be stored (can be NULL)
        height          - pointer to variable where the height of the buffer will be stored (can be NULL)
        compCount       - pointer to variable where the number of components will be stored (can be NULL)
        dataBuffer      - pointer to float array where the pixel data will be stored (can be NULL)
        bufferSize      - size of dataBuffer array in bytes

Returns:
        true in case of success, otherwise false
"
  (pipeline-res resource-handle)
  (target-name :string)
  (buf-index :int)
  (width (:pointer :int))
  (height (:pointer :int))
  (comp-count (:pointer :int))
  (data-buffer (:pointer :float))
  (buffer-size :int))


;;;; Group: General scene graph functions


;; int getNodeType( NodeHandle node );
(defh3fun ("getNodeType" get-node-type) node-type
  "Returns the type of a scene node.

This function returns the type of a specified scene node. If the node handle is
invalid, he function returns the node type 'Unknown'.

Parameters:
        node    - handle to the scene node

Returns:
        type of the scene node
"
  (node node-handle))


;; NodeHandle getNodeParent( NodeHandle node );
(defh3fun ("getNodeParent" get-node-parent) node-handle
  "Returns the parent of a scene node.

This function returns the handle to the parent node of a specified scene
node. If the specified node handle is invalid or the root node, 0 is returned.

Parameters:
        node    - handle to the scene node

Returns:
        handle to parent node or 0 in case of failure
"
  (node node-handle))


;; bool setNodeParent( NodeHandle node, NodeHandle parent );
(defh3fun ("setNodeParent" set-node-parent) :boolean
  "Relocates a node in the scene graph.

This function relocates a scene node. It detaches the node from its current
parent and attaches it to the specified new parent node. If the attachment to
the new parent is not possible, the function returns false. Relocation is not
possible for the RootNode.

Parameters:
        node    - handle to the scene node to be relocated
        parent  - handle to the new parent node

Returns:
        true in case of success, otherwise false
"
  (node node-handle) (parent node-handle))


;; NodeHandle getNodeChild( NodeHandle node, int index );
(defh3fun ("getNodeChild" get-node-child) node-handle
  "Returns the handle to a child node.

This function looks for the n-th (index) child node of a specified node and
returns its handle. If the child doesn't exist, the function returns 0.

Parameters:
        node    - handle to the parent node
        index   - index of the child node

Returns:
        handle to the child node or 0 if child doesn't exist
"
  (node node-handle) (index :int))


;; NodeHandle addNodes( NodeHandle parent, ResHandle sceneGraphRes );
(defh3fun ("addNodes" add-nodes) node-handle
  "Adds nodes from a SceneGraph resource to the scene.

This function creates several new nodes as described in a SceneGraph resource
and ttaches them to a specified parent node. If an invalid scenegraph resource
is specified or the scenegraph resource is unloaded, the function returns 0.

Parameters:
        parent                  - handle to parent node to which the root of the new nodes will be attached
        sceneGraphRes   - handle to loaded SceneGraph resource

Returns:
        handle to the root of the created nodes or 0 in case of failure
"
  (parent node-handle) (scene-graph-res resource-handle))


;; bool removeNode( NodeHandle node );
(defh3fun ("removeNode" remove-node) :boolean
  "Removes a node from the scene.

This function removes the specified node and all of it's children from the
scene.

Parameters:
        node    - handle to the node to be removed

Returns:
        true in case of success otherwise false
"
  (node node-handle))


;; bool setNodeActivation( NodeHandle node, bool active );
(defh3fun ("setNodeActivation" set-node-activation) :boolean
  "Sets the activation (visibility) state of a node.

This function sets the activation state of the specified node to active or
inactive. Inactive odes with all their children are excluded from rendering.

Parameters:
        node    - handle to the node to be modified
        active  - boolean value indicating whether node shall be active or inactive

Returns:
        true in case of success otherwise false
"
  (node node-handle) (active :boolean))


;; bool checkNodeTransformFlag( NodeHandle node, bool reset );
(defh3fun ("checkNodeTransformFlag" check-node-transform-flag) :boolean
  "Checks if a scene node has been transformed by the engine.

This function checks if a scene node has been transformed by the engine since
the last time the transformation flag was reset. Therefore, it stores a flag
that is set to true when a setTransformation function is called explicitely by
the application or when the node transformation has been updated by the
animation system. The function also makes it possible to reset the
transformation flag.

Parameters:
        node    - handle to the node to be accessed
        reset   - flag indicating whether transformation flag shall be reset

Returns:
        true if node has been transformed, otherwise false
"
  (node node-handle) (reset :boolean))


;; bool getNodeTransform( NodeHandle node, float *tx, float *ty, float *tz, float *rx, float *ry, float *rz, float *sx, float *sy, float *sz );
(defh3fun ("getNodeTransform" get-node-transform) :boolean
  "Gets the relative transformation of a node.

This function gets the translation, rotation and scale of a specified scene node
object. The coordinates are in local space and contain the transformation of the
node relative to its parent.

Parameters:
        node            - handle to the node which will be accessed
        tx, ty, tz      - pointers to variables where translation of the node will be stored (can be NULL)
        rx, ry, rz      - pointers to variables where rotation of the node in Euler angles
                                  will be stored (can be NULL)
        sx, sy, sz      - pointers to variables where scale of the node will be stored (can be NULL)

Returns:
        true in case of success otherwise false
"
  (node node-handle)
  (tx (:pointer :float)) (ty (:pointer :float)) (tz (:pointer :float))
  (rx (:pointer :float)) (ry (:pointer :float)) (rz (:pointer :float))
  (sx (:pointer :float)) (sy (:pointer :float)) (sz (:pointer :float)))


;; bool setNodeTransform( NodeHandle node, float tx, float ty, float tz, float rx, float ry, float rz, float sx, float sy, float sz );
(defh3fun ("setNodeTransform" set-node-transform) :boolean
  "Sets the relative transformation of a node.

This function sets the relative translation, rotation and scale of a specified
scene node object.  The coordinates are in local space and contain the
transformation of the node relative to its parent.

Parameters:
        node            - handle to the node which will be modified
        tx, ty, tz      - translation of the node
        rx, ry, rz      - rotation of the node in Euler angles
        sx, sy, sz      - scale of the node

Returns:
        true in case of success otherwise false
"
  (node node-handle)
  (tx :float) (ty :float) (tz :float)
  (rx :float) (ry :float) (rz :float)
  (sx :float) (sy :float) (sz :float))


;; bool getNodeTransformMatrices( NodeHandle node, const float **relMat, const float **absMat );
(defh3fun ("getNodeTransformMatrices" get-node-transform-matrices) :boolean
  "Returns the transformation matrices of a node.

This function stores a pointer to the relative and absolute transformation
matrices f the specified node in the specified pointer varaibles.

Parameters:
        node    - handle to the scene node to be accessed
        relMat  - pointer to a variable where the address of the relative transformation matrix will be stored
         (can be NULL if matrix is not required)
        absMat  - pointer to a variable where the address of the absolute transformation matrix will be stored
         (can be NULL if matrix is not required)


Returns:
        true in case of success otherwise false
"
  (node node-handle)
  (rel-mat (:pointer :float))
  (abs-mat (:pointer :float)))


;; bool setNodeTransformMatrix( NodeHandle node, const float *mat4x4 );
(defh3fun ("setNodeTransformMatrix" set-node-transform-matrix) :boolean
  "Sets the relative transformation matrix of a node.

This function sets the relative transformation matrix of the specified scene
node. It is basically the ame as setNodeTransform but takes directly a matrix
instead of individual transformation parameters.

Parameters:
        node    - handle to the node which will be modified
        mat4x4  - pointer to a 4x4 matrix in column major order

Returns:
        true in case of success otherwise false
"
  (node node-handle)
  (mat-4x4 :pointer))


;; float getNodeParamf( NodeHandle node, int param );
(defh3fun ("getNodeParamf" get-node-paramf) :float
  "Gets a property of a scene node.

This function returns a specified property of the specified node.  The property
must be of the type float.

Parameters:
        node    - handle to the node to be accessed
        param   - parameter to be accessed

Returns:
         value of the parameter
"
  (node node-handle) (param node-parameter))


;; bool setNodeParamf( NodeHandle node, int param, float value );
(defh3fun ("setNodeParamf" set-node-paramf) :boolean
  "Sets a property of a scene node.

This function sets a specified property of the specified node to a specified
value.  The property must be of the type float.

Parameters:
        node    - handle to the node to be modified
        param   - parameter to be modified
        value   - new value for the specified parameter

Returns:
         true in case of success otherwise false
"
  (node node-handle) (param node-parameter) (value :float))


;; int getNodeParami( NodeHandle node, int param );
(defh3fun ("getNodeParami" get-node-parami) :int
  "Gets a property of a scene node.

This function returns a specified property of the specified node.  The property
must be of the type int or ResHandle.

Parameters:
        node    - handle to the node to be accessed
        param   - parameter to be accessed

Returns:
         value of the parameter
"
  (node node-handle) (param node-parameter))


;; bool setNodeParami( NodeHandle node, int param, int value );
(defh3fun ("setNodeParami" set-node-parami) :boolean
  "Sets a property of a scene node.

This function sets a specified property of the specified node to a specified
value.  The property must be of the type int or ResHandle.

Parameters:
        node    - handle to the node to be modified
        param   - parameter to be modified
        value   - new value for the specified parameter

Returns:
         true in case of success otherwise false
"
  (node node-handle) (param node-parameter) (value :int))


;; const char *getNodeParamstr( NodeHandle node, int param );
(defh3fun ("getNodeParamstr" get-node-paramstr) :string
  "Gets a property of a scene node.

This function returns a specified property of the specified node.  The property
must be of the type string (const char *).

*Important Note: The pointer is const and allows only read access to the data. Do never try to modify the
ata of the pointer since that can corrupt the engine's internal states!*

Parameters:
        node    - handle to the node to be accessed
        param   - parameter to be accessed

Returns:
         value of the property or empty string if no such property exists
"
  (node node-handle) (param node-parameter))


;; bool setNodeParamstr( NodeHandle node, int param, const char *value );
(defh3fun ("setNodeParamstr" set-node-paramstr) :boolean
  "Sets a property of a scene node.

This function sets a specified property of the specified node to a specified
value.  The property must be of the type string (const char *).

Parameters:
        node    - handle to the node to be modified
        param   - parameter to be modified
        value   - new value for the specified parameter

Returns:
         true in case of success otherwise false
"
  (node node-handle) (param node-parameter) (value :string))



;; bool getNodeAABB( NodeHandle node, float *minX, float *minY, float *minZ, float *maxX, float *maxY, float *maxZ );
(defh3fun ("getNodeAABB" get-node-aabb) :boolean
  "Gets the bounding box of a scene node.

This function stores the world coordinates of the axis aligned bounding box of a
specified node in the specified variables. The bounding box is represented using
the minimum and maximum coordinates on all three axes.

Parameters:
        node                            - handle to the node which will be accessed
        minX, minY, minZ        - pointers to variables where minimum coordinates will be stored
        maxX, maxY, maxZ        - pointers to variables where maximum coordinates will be stored

Returns:
        true in case of success otherwise false
"
  (node node-handle)
  (min-x (:pointer :float)) (min-y (:pointer :float)) (min-z (:pointer :float))
  (max-x (:pointer :float)) (max-y (:pointer :float)) (max-z (:pointer :float)))


;; int findNodes( NodeHandle startNode, const char *name, int type );
(defh3fun ("findNodes" find-nodes) :int
  "Finds scene nodes with the specified properties.

This function loops recursively over all children of startNode and adds them to
an internal list of results if they match the specified name and type. The
result list is cleared each time this function is called. The function returns
the number of nodes which were found and added to the list.

Parameters:
        startNode       - handle to the node where the search begins
        name            - name of nodes to be searched (empty string for all nodes)
        type            - type of nodes to be searched (SceneNodeTypes::Undefined for all types)

Returns:
        number of search results
"
  (start-node node-handle) (name :string) (type node-type))


;; NodeHandle getNodeFindResult( int index );
(defh3fun ("getNodeFindResult" get-node-find-result) node-handle
  "Gets a result from the findNodes query.

This function returns the n-th (index) result of a previous findNodes query. The
result is the handle to a scene node with the poperties specified at the
findNodes query. If the index doesn't exist in the result list the function
returns 0.

Parameters:
        index   - index of search result

Returns:
        handle to scene node from findNodes query or 0 if result doesn't exist
"
  (index :int))


;; int castRay( NodeHandle node, float ox, float oy, float oz, float dx, float dy, float dz, int numNearest );
(defh3fun ("castRay" cast-ray) :int
  "Performs a recursive ray collision query.

This function checks recursively if the specified ray intersects the specified
node or one of its children.  The function finds intersections relative to the
ray origin and returns the number of intersecting scene odes. The ray is a line
segment and is specified by a starting point (the origin) and a finite direction
vector which also defines its length. Currently this function is limited to
returning intersections with Meshes.

Parameters:
        node            - node at which intersection check is beginning
        ox, oy, oz      - ray origin
        dx, dy, dz      - ray direction vector also specifying ray length
        numNearest      - maximum number of intersection points to be stored (0 for all)

Returns:
        number of intersections
"
  (node node-handle)
  (ox :float) (oy :float) (oz :float)
  (dx :float) (dy :float) (dz :float)
  (num-nearest :int))



;; bool getCastRayResult( int index, NodeHandle *node, float *distance, float *intersection );
(defh3fun ("getCastRayResult" get-cast-ray-result) :boolean
  "Returns a result of a previous castRay query.

This functions is used to access the results of a previous castRay query. The
index is used to access a specific result. The intersection data is copied to
the specified variables.

Parameters:
        index                   - index of result to be accessed (range: 0 to number of results returned by castRay)
        node                    - handle of intersected node
        distance                - distance from ray origin to intersection point
        intersection    - coordinates of intersection point (float[3] array)

Returns:
        true if index was valid and data could be copied, otherwise false
"
  (index :int)
  (node (:pointer node-handle))
  (distance (:pointer :float))
  (intersection (:pointer :float)))


;;;; Group: Group-specific scene graph functions 


;; NodeHandle addGroupNode( NodeHandle parent, const char *name );
(defh3fun ("addGroupNode" add-group-node) node-handle
  "Adds a Group node to the scene.

This function creates a new Group node and attaches it to the specified parent
node.

Parameters:
        parent  - handle to parent node to which the new node will be attached
        name    - name of the node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node-handle) (name :string))


;;;; Group: Model-specific scene graph functions 


;; NodeHandle addModelNode( NodeHandle parent, const char *name, ResHandle geometryRes );
(defh3fun ("addModelNode" add-model-node) node-handle
  "Adds a Model node to the scene.

This function creates a new Model node and attaches it to the specified parent
node.

Parameters:
        parent          - handle to parent node to which the new node will be attached
        name            - name of the node
        geometryRes     - Geometry resource used by Model node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node-handle) (name :string)
  (geometry-res resource-handle))


;; bool setupModelAnimStage( NodeHandle modelNode, int stage, ResHandle animationRes, const char *startNode, bool additive );
(defh3fun ("setupModelAnimStage" setup-model-anim-stage) :boolean
  "Configures an animation stage of a Model node.

This function is used to setup the specified animation stage (channel) of the specified Model node.

The function is used for animation blending. There is a fixed number of
stages (by default 16) on which different animations can be played. The start
node determines the first node (Joint or Mesh) to which the animation is
recursively applied. If the start node is an empty string, the animation affects
all animatable nodes (Joints and Meshes) of the model. If a NULL-handle is used
for animationRes, the stage is cleared and the previous animation is removed.

A simple way to do animation mixing is using additive animations. If a stage is
configured to be additive the engine calculates the difference between the
current frame and the first frame in the animation and adds this delta to the
current transformation of the joints or meshes.

Parameters:
        modelNode               - handle to the Model node to be modified
        stage                   - index of the animation stage to be configured
        animationRes    - handle to Animation resource (can be 0)
        startNode               - name of first node to which animation shall be applied (or empty string)
        additive                - flag indicating whether stage is additive

Returns:
         true in case of success, otherwise false
"
  (model-node node-handle)
  (stage :int)
  (animation-res resource-handle)
  (start-node :string)
  (additive :boolean))


;; bool setModelAnimParams( NodeHandle modelNode, int stage, float time, float weight );
(defh3fun ("setModelAnimParams" set-model-anim-params) :boolean
  "Sets the parameters of an animation stage in a Model node.

This function sets the current animation time and weight for a specified stage
of the specified model.  The time corresponds to the frames of the animation and
the animation is looped if the time is higher than the maximum number of frames
in the Animation resource. The weight is used for animation blending and
determines how much influence the stage has compared to the other active
stages. When the sum of the weights of all stages is more than one, the
animations on the lower stages get priority.

Parameters:
        modelNode       - handle to the Model node to be modified
        stage           - index of the animation stage to be modified
        time            - new animation time/frame
        weight          - new blend weight

Returns:
         true in case of success, otherwise false
"
  (model-node node-handle) (stage :int) (time :float) (weight :float))


;; bool setModelMorpher( NodeHandle modelNode, const char *target, float weight );
(defh3fun ("setModelMorpher" set-model-morpher) :boolean
  "Sets the weight of a morph target.

This function sets the weight of a specified morph target. If the target
parameter is an empty string the weight of all morph targets in the specified
Model node is modified.  If the specified morph target is not found the function
returns false.

Parameters:
        modelNode       - handle to the Model node to be modified
        target          - name of morph target
        weight          - new weight for morph target

Returns:
         true in case of success, otherwise false
"
  (model-node node-handle) (target :string) (weight :float))


;;;; Group: Mesh-specific scene graph functions 


;; NodeHandle addMeshNode( NodeHandle parent, const char *name, ResHandle materialRes, int batchStart, int batchCount, int vertRStart, int vertREnd );
(defh3fun ("addMeshNode" add-mesh-node) node-handle
  "Adds a Mesh node to the scene.

This function creates a new Mesh node and attaches it to the specified parent
node.

Parameters:
        parent          - handle to parent node to which the new node will be attached
        name            - name of the node
        materialRes     - material resource used by Mesh node
        batchStart      - first triangle index of mesh in Geometry resource of parent Model node
        batchCount      - number of triangle indices used for drawing mesh
        vertRStart      - first vertex in Geometry resource of parent Model node
        vertREnd        - last vertex in Geometry resource of parent Model node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node-handle) (name :string)
  (material-res resource-handle)
  (batch-start :int) (batch-count :int)
  (vert-r-start :int) (vert-r-end :int))

;;;; Group: Joint-specific scene graph functions


;; NodeHandle addJointNode( NodeHandle parent, const char *name, int jointIndex );
(defh3fun ("addJointNode" add-joint-node) node-handle
  "Adds a Joint node to the scene.

This function creates a new Joint node and attaches it to the specified parent
node.

Parameters:
        parent          - handle to parent node to which the new node will be attached
        name            - name of the node
        jointIndex      - index of joint in Geometry resource of parent Model node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node-handle) (name :string) (joint-index :int))


;;;; Group: Light-specific scene graph functions 


;; NodeHandle addLightNode( NodeHandle parent, const char *name, ResHandle materialRes, const char *lightingContext, const char *shadowContext );
(defh3fun ("addLightNode" add-light-node) node-handle
  "Adds a Light node to the scene.

This function creates a new Light node and attaches it to the specified parent
node.  he direction vector of the untransformed light node is pointing along the
the negative -axis. The specified material resource can define uniforms and
projective textures.  urthermore it can contain a shader for doing lighting
calculations if deferred shading s used. If no material is required the
parameter can be zero. The context names efine which shader contexts are used
when rendering shadow maps or doing light calculations for orward rendering
configurations.

Parameters:
        parent                  - handle to parent node to which the new node will be attached
        name                    - name of the node
        materialRes             - material resource for light configuration or 0 if not used
        lightingContext - name of the shader context used for doing light calculations
        shadowContext   - name of the shader context used for doing shadow map rendering

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node-handle) (name :string)
  (material-res resource-handle)
  (lighting-context :string)
  (shadow-context :string))


;; bool setLightContexts( NodeHandle lightNode, const char *lightingContext, const char *shadowContext );
(defh3fun ("setLightContexts" set-light-contexts) :boolean
  "Sets the shader contexts used by a light source.

This function sets the lighting and shadow shader contexts of the specified
light source. The contexts define which shader code is used when doing lighting
calculations or rendering the shadow map.

Parameters:
        lightNode               - handle to the Light node to be modified
        lightingContext - name of the shader context used for performing lighting calculations
        shadowContext   - name of the shader context used for rendering shadow maps

Returns:
         true in case of success otherwise false
"
  (light-node node-handle)
  (lighting-context :string)
  (shadow-context :string))

;;;; Group: Camera-specific scene graph functions


;; NodeHandle addCameraNode( NodeHandle parent, const char *name, ResHandle pipelineRes );
(defh3fun ("addCameraNode" add-camera-node) node-handle
  "Adds a Camera node to the scene.

This function creates a new Camera node and attaches it to the specified parent
node.

Parameters:
        parent          - handle to parent node to which the new node will be attached
        name            - name of the node
        pipelineRes     - pipeline resource used for rendering

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node-handle) (name :string) (pipeline-res resource-handle))


;; bool setupCameraView( NodeHandle cameraNode, float fov, float aspect, float nearDist, float farDist );
(defh3fun ("setupCameraView" setup-camera-view) :boolean
  "Sets the planes of a camera viewing frustum.

This function calculates the view frustum planes of the specified camera node
using the specified view arameters.

Parameters:
        cameraNode      - handle to the Camera node which will be modified
        fov                     - field of view (FOV) angle
        aspect          - aspect ratio
        nearDist        - distance of near clipping plane
        farDist         - distance of far clipping plane

Returns:
         true in case of success otherwise false
"
  (camera-node node-handle) (fov :float)
  (aspect :float) (near-dist :float)
  (far-dist :float))


;; bool calcCameraProjectionMatrix( NodeHandle cameraNode, float *projMat );
(defh3fun ("calcCameraProjectionMatrix" calc-camera-projection-matrix) :boolean
  "Calculates the camera projection matrix.

This function calculates the camera projection matrix used for bringing the
geometry to screen space and copies it to the specified array.

Parameters:
        cameraNode      - handle to Camera node
        projMat         - pointer to float array with 16 elements

Returns:
         true in case of success otherwise false
"
  (camera-node node-handle)
  (proj-mat (:pointer :float)))


;;;; Group: Emitter-specific scene graph functions


;; NodeHandle addEmitterNode( NodeHandle parent, const char *name, ResHandle materialRes, ResHandle effectRes, int maxParticleCount, int respawnCount );
(defh3fun ("addEmitterNode" add-emitter-node) node-handle
  "Adds a Emitter node to the scene.

This function creates a new Emitter node and attaches it to the specified parent
node.

Parameters:
        parent           - handle to parent node to which the new node will be attached
        name             - name of the node
        materialRes      - handle to Material resource used for rendering
        effectRes        - handle to Effect resource used for configuring particle properties
        maxParticleCount - maximal number of particles living at the same time
        respawnCount     - number of times a single particle is recreated after dying (-1 for infinite)


Returns:
         handle to the created node or 0 in case of failure
"
  (parent node-handle)
  (name :string)
  (material-res resource-handle)
  (effect-res resource-handle)
  (max-particle-count :int)
  (respawn-count :int))


;; bool advanceEmitterTime( NodeHandle emitterNode, float timeDelta );
(defh3fun ("advanceEmitterTime" advance-emitter-time) :boolean
  "Advances the time value of an Emitter node.

This function advances the simulation time of a particle system and continues
the particle simulation with timeDelta being the time elapsed since the last
call of this function. The specified node must be an Emitter node.

Parameters:
        emitterNode     - handle to the Emitter node which will be modified
        timeDelta       - time delta in seconds

Returns:
         true in case of success otherwise false
"
  (emitter-node node-handle)
  (time-delta :float))


;; bool hasEmitterFinished( NodeHandle emitterNode );
(defh3fun ("hasEmitterFinished" has-emitter-finished) :boolean
  "Checks if an Emitter node is still alive.

This function checks if a particle system is still active and has living
particles or will spawn new particles. The specified node must be an Emitter
node. The function can be used to check when a not infinitely running emitter
for an effect like an explosion can be removed from the scene.

Parameters:
        emitterNode     - handle to the Emitter node which is checked

Returns:
         true if Emitter will no more emit any particles, otherwise or in case of failure false
"
  (emitter-node node-handle))


;;; Helper macro to define a horde3d utils API function and declare it inline.
(defmacro defh3ufun ((cname lname) result-type &body body)
  `(progn
     (declaim (inline ,lname))
     (defcfun (,cname ,lname :library horde3d-utils) ,result-type ,@body)))

;; bool dumpMessages();
(defh3ufun ("dumpMessages" dump-messages) :boolean
  "Writes all messages in the queue to a log file.

This utility function pops all messages from the message queue and writes them
to a HTML formated log file 'EngineLog.html'.

Parameters:
        none

Returns:
        true in case of success, otherwise false
")


;;     /*    Group: OpenGL-related functions */

;; bool initOpenGL( int hDC );
(defh3ufun ("initOpenGL" init-open-gl) :boolean
  "Initializes OpenGL.

This utility function initializes an OpenGL rendering context in a specified
window component.  *Currently this function is only available on Windows
platforms.*

Parameters:
        hDC    - handle to device context for which OpenGL context shall be created

Returns:
        true in case of success, otherwise false
"
  (h-dc :int))


;; void releaseOpenGL();
(defh3ufun ("releaseOpenGL" release-open-gl) :void
  "Releases OpenGL.

This utility function destroys the previously created OpenGL rendering context.
*Currently this function is only available on Windows platforms.*

Parameters:
        none

Returns:
        nothing
")


;; void swapBuffers();
(defh3ufun ("swapBuffers" swap-buffers) :void
  "Displays the rendered image on the screen.

This utility function displays the image rendered to the previously initialized
OpenGL context on the screen by copying it from the backbuffer to the
frontbuffer.  *Currently this function is only available on Windows platforms.*

Parameters:
        none

Returns:
        nothing
")


;;;; Group: Resource management


;; const char *getResourcePath( int type );
(defh3ufun ("getResourcePath" get-resource-path) :string
  "Returns  the search path of a resource type.

This function returns the search path of a specified resource type.

Parameters:
        type    - type of resource

Returns:
        pointer to the search path string
"
  (type :int))


;; void setResourcePath( int type, const char *path );
(defh3ufun ("setResourcePath" set-resource-path) :void
  "Sets the search path for a resource type.

This function sets the search path for a specified resource type.

Parameters:
        type    - type of resource
        path    - path where the resources can be found ((back-)slashes at end are removed)

Returns:
        nothing
"
  (type :int) (path :string))


;; bool loadResourcesFromDisk( const char *contentDir );
(defh3ufun ("loadResourcesFromDisk" load-resources-from-disk) :boolean
  "Loads previously added resources from a data drive.

This utility function loads previously added and still unloaded resources from
the specified directories on a data drive. Several search paths can be specified
using the pipe character (|) as separator. All resource names are directly
converted to filenames and the function tries to find them in the specified
directories using the given order of the search paths.

Parameters:
        contentDir    - directories where data is located on the drive ((back-)slashes at end are removed)

Returns:
        false if at least one resource could not be loaded, otherwise true
"
  (content-dir :string))


;; bool createTGAImage( const unsigned char *pixels, int width, int height, int bpp, char **outData, int *outSize );
(defh3ufun ("createTGAImage" create-tga-image) :boolean
  "Creates a TGA image in memory.

This utility function allocates memory at the pointer outData and creates a TGA
image from the specified pixel data. The dimensions of the image have to be
specified as well as the bit depth.  The created TGA-image-data can be used as
Texture2D or TexureCube resource in the engine.  *Note: The memory allocated by
this routine has to freed manually using the freeMem function.*

Parameters:
        pixels  - pointer to pixel source data in BGR(A) format from which TGA-image is constructed;
                  memory layout: pixel with position (x, y) in image (origin of image is upper left
                                 corner) has memory location (y * width + x) * (bpp / 8) in pixels-array  
        width   - width of source image
        height  - height of source image
        bpp     - color bit depth of source data (valid values: 24, 32)
        outData - address of a pointer to which the address of the created memory block is written
        outSize - variable to which to size of the created memory block is written

Returns:
        false if at least one resource could not be loaded, otherwise true
"
  (pixels (:pointer :uchar))
  (width :int) (height :int)
  (bpp :int)
  (out-data :pointer)
  (out-size (:pointer :int)))


;;;; Group: Scene graph

;; void pickRay(NodeHandle cameraNode, float nwx, float nwy, float *ox, float *oy, float *oz, float *dx, float *dy, float *dz );
(defh3ufun ("pickRay" pick-ray) :void
  "*      Calculates the ray originating at the specified camera and window coordinates

This utility function takes normalized window coordinates (ranging from 0 to 1
with the origin being the bottom left corner of the window) and returns ray
origin and direction for the given camera. The function is especially useful for
selecting objects by clicking on them.

Parameters:
        cameraNode    - camera used for picking
        nwx, nwy    - normalized window coordinates
        ox, oy, oz  - calculated ray origin
        dx, dy, dz  - calculated ray direction

Returns:
        nothing
"
  (camera-node node-handle)
  (nwx :float) (nwy :float)
  (ox (:pointer :float)) (oy (:pointer :float)) (oz (:pointer :float))
  (dx (:pointer :float)) (dy (:pointer :float)) (dz (:pointer :float)))


;; NodeHandle pickNode(NodeHandle cameraNode, float nwx, float nwy );
(defh3ufun ("pickNode" pick-node) node-handle
  "Returns the scene node which is at the specified window coordinates.

This utility function takes normalized window coordinates (ranging from 0 to 1 with the
origin being the bottom left corner of the window) and returns the scene node which is
visible at that location. The function is especially useful for selecting objects by clicking
on them. Currently picking is only working for Meshes.

Parameters:
        cameraNode - camera used for picking
        nwx, nwy   - normalized window coordinates

Returns:
        handle of picked node or 0 if no node was hit
"
  (camera-node node-handle) (nwx :float) (nwy :float))

;;;; Group: Overlays

;; void showText( const char *text, float x, float y, float size, int layer, ResHandle fontMaterialRes );
(defh3ufun ("showText" show-text) :void
  "Shows text on the screen using a font texture.

This utility function uses overlays to display a text string at a specified position on the screen.
The font texture of the specified font material has to be a regular 16x16 grid containing all
ASCII characters in row-major order. The layer corresponds to the layer parameter of overlays.

Parameters:
        text            - text string to be displayed
        x, y            - position of the lower left corner of the first character;
                                          for more details on coordinate system see overlay documentation
        size            - size (scale) factor of the font
        layer           - layer index of the font overlays
        fontMaterialRes - font material resource used for rendering

Returns:
        nothing
"
  (text :string) (x :float) (y :float) (size :float)
  (layer :int) (font-material-res resource-handle))


;; void showFrameStats( ResHandle fontMaterialRes, float curFPS );
(defh3ufun ("showFrameStats" show-frame-stats) :void
  "Shows frame statistics on the screen.

This utility function displays statistics for the current frame in the upper left corner of
the screen. Since the statistic counters are reset after the call, it should be called exactly
once per frame to obtain correct values.

Parameters:
        fontMaterialRes - font material resource used for drawing text
        curFPS          - frames per second with which application is currently running

Returns:
        nothing
"
  (font-material-res resource-handle)
  (cur-fps :float))

;;;; Group: Terrain Extension

;; NodeHandle addTerrainNode( NodeHandle parent, const char *name, ResHandle heightMapRes, ResHandle materialRes );
(defh3fun ("addTerrainNode" add-terrain-node) node-handle
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
  (parent node-handle) (name :string) (height-map resource-handle) (material resource-handle))

;; ResHandle createGeometryResource( NodeHandle node, const char *resName, float meshQuality );
(defh3fun ("createGeometryResource" create-geometry-resource) resource-handle
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
  (node node-handle) (res-name :string) (mesh-quality :float))


;;; core-bindings.lisp ends here
