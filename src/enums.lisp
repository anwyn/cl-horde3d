;;; enums.lisp --- horde3d enums

;;;   ___ _ __  _   _ _ __ ___  ___
;;;  / _ \ '_ \| | | | '_ ` _ \/ __|
;;; |  __/ | | | |_| | | | | | \__ \
;;;  \___|_| |_|\__,_|_| |_| |_|___/

;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :horde3d-cffi)

;;;; ---------------------------------------------------------------------------
;;;; * Group: Enumerations


(defcenum option
  "The available engine option parameters.

 :max-log-level          - Defines the maximum log level; only messages which are smaller or equal to this value
                           (hence more important) are published in the message queue. (Default: 4)
 :max-num-messages       - Defines the maximum number of messages that can be stored in the message queue (Default: 512)
 :trilinear-filtering    - Enables or disables trilinear filtering for textures; only affects textures
                           that are loaded after setting the option. (Values: 0, 1; Default: 1)
 :max-anisotropy         - Sets the quality for anisotropic filtering; only affects textures that
                           are loaded after setting the option. (Values: 1, 2, 4, 8; Default: 1)
 :texture-compression    - Enables or disables texture compression; only affects textures that are
                           loaded after setting the option. (Values: 0, 1; Default: 0)
 :load-textures          - Enables or disables loading of texture images; option can be used to
                           minimize loading times for testing. (Values: 0, 1; Default: 1)
 :fast-animation         - Disables or enables inter-frame interpolation for animations. (Values: 0, 1; Default: 1)
 :shadowmap-size         - Sets the size of the shadow map buffer (Values: 128, 256, 512, 1024, 2048; Default: 1024)
 :sample-count           - Maximum number of samples used for multisampled render targets; only affects pipelines
                           that are loaded after setting the option. (Values: 0, 2, 4, 8, 16; Default: 0)
 :wireframe-mode         - Enables or disables wireframe rendering
 :debug-view-mode        - Enables or disables debug view where geometry is rendered in wireframe without shaders and
                           lights are visualized using their screen space bounding box. (Values: 0, 1; Default: 0)
 :dump-failed-shaders    - Enables or disables storing of shader code that failed to compile in a text file; this can be
                           useful in combination with the line numbers given back by the shader compiler.
                           (Values: 0, 1; Default: 0)
 :gather-time-statistics - Enables or disables gathering of time stats that are useful for profiling (Values: 0, 1; Default: 1)

"
  (:max-log-level 1)
  :max-num-messages
  :trilinear-filtering
  :max-anisotropy
  :texture-compression
  :load-textures
  :fast-animation
  :shadow-map-size
  :sample-count
  :wireframe-mode
  :debug-view-mode
  :dump-failed-shaders
  :gather-time-statistics)

(defcenum statistics
  "The available engine statistic parameters.

 :triangle-count   - Number of triangles that were pushed to the renderer
 :batch-count      - Number of batches (draw calls)
 :light-pass-count - Number of lighting passes
 :frame-time       - Time in ms between two finalizeFrame calls
 :custom-time      - Value of custom timer (useful for profiling engine functions)
 :texture-vmem     - Estimated amount of video memory used by textures (in Mb)
 :geometry-vmem    - Estimated amount of video memory used by geometry (in Mb)
"
  (:triangle-count 100)
  :batch-count
  :light-pass-count
  :frame-time
  :custom-time
  :texture-vmem
  :geometry-vmem)

;;;; ---------------------------------------------------------------------------
;;;; Resource types

(defcenum resource-types
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
  :texture
  :particle-effect
  :pipeline)

(defctype resource-type
    (multi-enum :enums (resource-types
                        #+horde3d-terrain-extension terrain-resource-types
                        #+horde3d-sound-extension sound-resource-types))
  "All resource types.")

;;;; ----------------------------------------------------------------------------
;;;; Resource flags & formats

(defbitfield resource-flags
  "Enum: resource-flags - The available flags used when adding a resource.

 :no-query                - Excludes resource from being listed by queryUnloadedResource function.
 :no-texture-compression  - Disables texture compression for Texture2D or TextureCube resource.
 :no-texture-mipmaps      - Disables generation of mipmaps for textures.
 :texture-cubemap         - Sets Texture resource to be a cubemap.
 :texture-dynamic         - Enables more efficient updates of Texture resource streams.
 :texture-renderable      - Makes Texture resource usable as render target.
"
  (:no-query #x0001)
  :no-texture-compression
  :no-texture-mipmaps
  :texture-cubemap
  :texture-dynamic
  :texture-renderable)

(defcenum resource-format
    "The available resource stream formats.

 :unknown          - Unknown format
 :texture-bgra8    - 8-bit BGRA texture
 :texture-dxt1     - DXT1 compressed texture
 :texture-dxt3     - DXT3 compressed texture
 :texture-dxt5     - DXT5 compressed texture
 :texture-rgba16f  - Half float RGBA texture
 :texture-rgba32f  - Float RGBA texture
"
  :unknown
  :texture-bgra8
  :texture-dxt1
  :texture-dxt3
  :texture-dxt5
  :texture-rgba16f
  :texture-rgba32f)

;;;; ----------------------------------------------------------------------------
;;;;  Resource parameters

;;; Geometry resources

(defcenum geometry-resource-elements
  "The available Geometry resource accessors.

 :geometry-element                - Base element
"
  (:geometry-element 200))

(deftypedenum geometry-resource-parameters
  "The available Geometry resource accessors.

 :geometry-index-count            - Number of indices [read-only]
 :geometry-vertex-count           - Number of vertices [read-only]
 :geometry-indices-16             - Flag indicating whether index data is 16 or 32 bit [read-only]
 :geometry-index-stream           - Triangle index data (uint16 or uint32, depending on flag)
 :geometry-vertex-position-stream - Vertex position data (float x, y, z)
 :geometry-vertex-tangent-stream  - Vertex tangent frame (TBN) data
                                    (float tx, ty, tz, bx, by, bz, nx, ny, nz)
 :geometry-vertex-static-stream   - Vertex static attribute data (float u0, v0,
                                    float4 jointIndices, float4 jointWeights, float u1, v1)
"
  (:geometry-index-count 201) :int
  :geometry-vertex-count :int
  :geometry-indices-16 :int
  :geometry-index-stream :int
  :geometry-vertex-position-stream :int
  :geometry-vertex-tangent-stream :int
  :geometry-vertex-static-stream :int)

;;; Animation resources

(defcenum animation-resource-elements
  "The available Animation resource element accessors.

 :entity-element      - Stored animation entities (joints and meshes)
"
  (:animation-entity 300))

(deftypedenum animation-resource-parameters
  "The available Animation resource accessors.

 :entity-frame-count  - Number of frames stored for a specific entity [read-only]
"
  (:animation-entity-frame-count 301) :int)

;;; Material resources

(defcenum material-resource-elements
  "The available Material resource element accessors.

 :material-element                  - Base element
 :material-sampler-element          - Sampler element
 :material-uniform-element          - Uniform element
"
  (:material-element 400)
  :material-sampler-element
  :material-uniform-element)

(deftypedenum material-resource-parameters
  "The available Material resource accessors.

 :material-class                    - Material class
 :material-link                     - Material resource that is linked to this material
 :material-shader-resource          - Shader resource
 :material-sampler-name             - Name of sampler (string) [read-only]
 :material-sampler-texture-resource - Texture resource bound to sampler
 :material-uniform-name             - Name of uniform [read-only]
 :material-uniform-value            - Value of uniform (a, b, c, d)
"
  (:material-class 403) :string
  :material-link :resource
  :material-shader-resource :resource
  :material-sampler-name :string
  :material-sampler-texture-resource :resource
  :material-uniform-name :string
  :material-uniform-value :float)

;;; Shader resources

(defcenum shader-resource-elements
  "The available Shader resource element accessors.

 :shader-context-element       - Context element
 :shader-sampler-element       - Sampler element
 :shader-uniform-element       - Uniform element
"
  (:shader-context-element 600)
  :shader-sampler-element
  :shader-uniform-element)

(deftypedenum shader-resource-parameters
  "The available Shader resource accessors.

 :shader-context-name          - Name of context [read-only]
 :shader-sampler-name          - Name of sampler [read-only]
 :shader-uniform-name          - Name of uniform [read-only]
 :shader-uniform-default-value - Default value of uniform (a, b, c, d)
"
  (:shader-context-name 603) :string
  :shader-sampler-name :string
  :shader-uniform-name :string
  :shader-uniform-default-value :float)

;;; Texture resources

(defcenum texture-resource-elements
    "The available Texture resource element accessors.

 :texture-element             - Base element
 :texture-image-element       - Subresources of the texture. A texture consists, depending on the type,
                                of a number of equally sized slices which again can have a fixed number
                                of mipmaps. Each image element represents the base image of a slice or
                                a single mipmap level of the corresponding slice.
"
  (:texture-element 700)
  :texture-image-element)

(deftypedenum texture-resource-parameters
  "The available Texture resource accessors.

 :texture-format              - Texture format [read-only]
 :texture-slice-count         - Number of slices (1 for 2D texture and 6 for cubemap) [read-only]
 :texture-image-width         - Image width [read-only]
 :texture-image-height        - Image height [read-only]
 :texture-image-pixel-stream  - Pixel data of an image. The data layout matches the layout specified
                                by the texture format with the exception that half-float is converted
                                to float. The first element in the data array corresponds to the lower
                                left corner.
"
  :texture-format :int
  :texture-slice-count :int
  :texture-image-width :int
  :texture-image-height :int
  :texture-image-pixel-stream :int)

;;; Particle effect resources

(defcenum particle-effect-resource-elements
  "The available ParticleEffect resource element accessors.

 :particle-element               - General particle configuration
"
  (:particle-element 800))

(deftypedenum particle-effect-resource-parameters
  "The available ParticleEffect resource accessors.

 :particle-channel-move-velocity - Velocity channel
 :particle-channel-rot-velocity  - Angular velocity channel
 :particle-channel-size          - Size channel
 :particle-channel-color-red     - Red color component channel
 :particle-channel-color-green   - Green color component channel
 :particle-channel-color-blue    - Blue color component channel
 :particle-channel-color-alpha   - Alpha channel
 :particle-life-min              - Minimum value of random life time (in seconds)
 :particle-life-max              - Maximum value of random life time (in seconds)
 :particle-channel-start-min     - Minimum for selecting initial random value of channel
 :particle-channel-start-max     - Maximum for selecting initial random value of channel
 :particle-channel-end-rate      - Remaining percentage of initial value when particle is dying
"
  (:particle-channel-move-velocity 801) :float
  :particle-channel-rot-velocity :float
  :particle-channel-size :float
  :particle-channel-color-red :float
  :particle-channel-color-green :float
  :particle-channel-color-blue :float
  :particle-channel-color-alpha :float
  :particle-life-min :float
  :particle-life-max :float
  :particle-channel-start-min :float
  :particle-channel-start-max :float
  :particle-channel-end-rate :float)

;;; Pipeline resources

(defcenum pipeline-resource-elements
    "The available Pipeline resource element accessors.

 :pipeline-stage-element    - Pipeline stage
 :pipeline-stage-name       - Name of stage [read-only]
 :pipeline-stage-activation - Flag indicating whether stage is active
"
  (:pipeline-stage-element 900))

(deftypedenum pipeline-resource-parameters
    "The available Pipeline resource accessors.

 :pipeline-stage-name       - Name of stage [read-only]
 :pipeline-stage-activation - Flag indicating whether stage is active
"
  (:pipeline-stage-name 901) :string
  :pipeline-stage-activation :int)


(defctype resource-parameter
    (multi-enum :enums (geometry-resource-parameters
                        animation-resource-parameters
                        material-resource-parameters
                        shader-resource-parameters
                        texture-resource-parameters
                        particle-effect-resource-parameters
                        pipeline-resource-parameters
                        #+horde3d-terrain-extension terrain-resource-parameters
                        #+horde3d-sound-extension sound-resource-parameters))
  "Parameters for all resources.")

(defctype resource-element
    (multi-enum :enums (geometry-resource-elements
                        animation-resource-elements
                        material-resource-elements
                        shader-resource-elements
                        texture-resource-elements
                        particle-effect-resource-elements
                        pipeline-resource-elements
                        #+horde3d-terrain-extension terrain-resource-elements
                        #+horde3d-sound-extension sound-resource-elements))
  "All resource parameters that denote a resource element.")

;;;; ----------------------------------------------------------------------------
;;;; Node types

(defcenum node-types
    "The available scene node types.

   :undefined       - An undefined node type, returned by get-node-type in case of error
   :group           - Group of different scene nodes
   :model           - 3D model with optional skeleton
   :mesh            - Subgroup of a model with triangles of one material
   :joint           - Joint for skeletal animation
   :light           - Light source
   :camera          - Camera giving view on scene
   :emitter         - Particle system emitter
"
  (:undefined 0)
  :group
  :model
  :mesh
  :joint
  :light
  :camera
  :emitter)

(defctype node-type
    (multi-enum :enums (node-types
                        #+horde3d-terrain-extension terrain-node-types
                        #+horde3d-sound-extension sound-node-types))
  "The type of a Horde3D node.")

;;; ----------------------------------------------------------------------------
;;; Node parameters

(deftypedenum node-parameters
  "The available scene node parameters:

   :node-name       - Name of the scene node [type: string]
   :node-attachment - Optional application-specific meta data for a node
                      encapsulated in an 'Attachment' XML string [type: string]
"
  (:node-name 1) :string
  :node-attachment :string)

(deftypedenum model-node-parameters
  "The available Model node parameters:

 :model-geometry-resource    - Geometry resource used for the model [type: resource]
 :model-software-skinning    - Enables or disables software skinning (default: 0) [type: int]
 :model-lod-dist-1           - Distance to camera from which on LOD1 is used (default: infinite) [type: float]
                         (must be a positive value larger than 0.0)
 :model-lod-dist-2           - Distance to camera from which on LOD2 is used
                         (may not be smaller than LodDist1) (default: infinite) [type: float]
 :model-lod-dist-3           - Distance to camera from which on LOD3 is used
                         (may not be smaller than LodDist2) (default: infinite) [type: float]
 :model-lod-dist-4           - Distance to camera from which on LOD4 is used
                         (may not be smaller than LodDist3) (default: infinite) [type: float]
"
  (:model-geometry-resource 200) :resource
  :model-software-skinning :int
  :model-lod-dist-1 :float
  :model-lod-dist-2 :float
  :model-lod-dist-3 :float
  :model-lod-dist-4 :float)


(deftypedenum mesh-node-parameters
  "The available Mesh node parameters.
		
 :mesh-material-resource - Material resource used for the mesh
 :mesh-batch-start       - First triangle index of mesh in Geometry resource of parent Model node [read-only]
 :mesh-batch-count       - Number of triangle indices used for drawing mesh [read-only]
 :mesh-vertex-rstart     - First vertex in Geometry resource of parent Model node [read-only]
 :mesh-vertex-rend       - Last vertex in Geometry resource of parent Model node [read-only]
 :mesh-lod-level         - LOD level of Mesh; the mesh is only rendered if its LOD level corresponds to
                           the model's current LOD level which is calculated based on the LOD distances (default: 0)
"
  (:mesh-material-resource 300) :resource
  :mesh-batch-start :int
  :mesh-batch-count :int
  :mesh-vertex-rstart :int
  :mesh-vertex-rend :int
  :mesh-lod-level :int)


(deftypedenum joint-node-parameters
  "The available Joint node parameters.

 :joint-index  - Index of joint in Geometry resource of parent Model node [read-only]
"
  ( :joint-index 400) :int)

(deftypedenum light-node-parameters
  "The available Light node parameters.
		
 :light-material-resource   - Material resource used for the light
 :light-radius              - Radius of influence (default: 100.0)
 :light-fov                 - Field of view (FOV) angle (default: 90.0)
 :light-color               - Diffuse color RGB (default: 1.0, 1.0, 1.0)
 :light-shadow-map-count    - Number of shadow maps used for light source (values: 0, 1, 2, 3, 4; default: 0)]
 :light-shadow-split-lambda - Constant determining segmentation of view frustum for Parallel Split Shadow Maps (default: 0.5)
 :light-shadow-map-bias     - Bias value for shadow mapping to reduce shadow acne (default: 0.005)
 :light-lighting-context    - Name of shader context used for computing lighting
 :light-shadow-context      - Name of shader context used for generating shadow map
"
  (:light-material-resource 500) :resource
  :light-radius :float
  :light-fov :float
  :light-color :float
  :light-shadow-map-count :int
  :light-shadow-split-lambda :float
  :light-shadow-map-bias :float
  :light-lighting-context :string
  :light-shadow-context :string)

(deftypedenum camera-node-parameters
    "The available Camera node parameters.
		
 :camera-pipeline-resource    - Pipeline resource used for rendering
 :camera-out-texture-resource - 2D Texture resource used as output buffer (can be 0 to use main framebuffer) (default: 0)
 :camera-out-buffer-index     - Index of the output buffer for stereo rendering (values: 0 for left eye, 1 for right eye) (default: 0)
 :camera-left-plane           - Coordinate of left plane relative to near plane center (default: -0.055228457)
 :camera-right-plane          - Coordinate of right plane relative to near plane center (default: 0.055228457)
 :camera-bottom-plane         - Coordinate of bottom plane relative to near plane center (default: -0.041421354f)
 :camera-top-plane            - Coordinate of top plane relative to near plane center (default: 0.041421354f)
 :camera-near-plane           - Distance of near clipping plane (default: 0.1)
 :camera-far-plane            - Distance of far clipping plane (default: 1000)
 :camera-ortho                - Flag for setting up an orthographic frustum instead of a perspective one (default: 0)
 :camera-occlussion-culling   - Flag for enabling occlusion culling (default: 0)
"
  (:camera-pipeline-resource 600) :resource
  :camera-out-texture-resource :resource
  :camera-out-buffer-index :int
  :camera-left-plane :float
  :camera-right-plane :float
  :camera-bottom-plane :float
  :camera-top-plane :float
  :camera-near-plane :float
  :camera-far-plane :float
  :camera-ortho :int
  :camera-occlussion-culling :int)

(deftypedenum emitter-node-parameters
  "The available Emitter node parameters.
		
 :emitter-material-resource        - Material resource used for rendering
 :emitter-particle-effect-resource - ParticleEffect resource which configures particle properties
 :emitter-max-count                - Maximal number of particles living at the same time
 :emitter-respawn-count            - Number of times a single particle is recreated after dying (-1 for infinite)
 :emitter-delay                    - Time in seconds before emitter begins creating particles (default: 0.0)
 :emitter-emission-rate            - Maximal number of particles to be created per second (default: 0.0)
 :emitter-spread-angle             - Angle of cone for random emission direction (default: 0.0)
 :emitter-force                    - Force vector XYZ applied to particles (default: 0.0, 0.0, 0.0)
"
  (:emitter-material-resource 700) :resource
  :emitter-particle-effect-resource :resource
  :emitter-max-count :int
  :emitter-respawn-count :int
  :emitter-delay :float
  :emitter-emission-rate :float
  :emitter-spread-angle :float
  :emitter-force :float)

(defctype node-parameter
    (multi-enum :enums (node-parameters
                        model-node-parameters
                        mesh-node-parameters
                        joint-node-parameters
                        light-node-parameters
                        camera-node-parameters
                        emitter-node-parameters
                        #+horde3d-terrain-extension terrain-node-parameters
                        #+horde3d-sound-extension listener-node-parameters
                        #+horde3d-sound-extension sound-node-parameters))
  "The type of a Horde3D node.")

;;; enums.lisp ends here
