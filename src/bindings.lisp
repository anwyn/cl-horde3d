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

(in-package :horde3d-cffi)

;;;; Constants: Predefined constants

(defconstant +root-node+ 1
  "Scene root node handle.")

;;;; ---------------------------------------------------------------------------
;;;; * Group: Basic functions

;; const char* h3dGetVersionString();
(defh3fun ("h3dGetVersionString" get-version-string) string
  "Returns the engine version string.

This function returns a pointer to a string containing the current version of
Horde3D.

Parameters:
      none

Returns:
      pointer to the version string
")

;; bool h3dCheckExtension( const char* extensionName );
(defh3fun ("h3dCheckExtension" check-extension) boolean
  "Checks if an extension is part of the engine library.

This function checks if a specified extension is contained in the DLL/shared
object of the engine.

Parameters:
    extension-name   - name of the extension

Returns:
    true if extension is implemented, otherwise false
"
  (extension-name string))

;; bool h3dGetError();
(defh3fun ("h3dGetError" get-error) boolean
  "Checks if an error occured.

This function checks if an error occured in a previous API function
call. If an error flag is set, the function resets the flag and
returns true. The function will solely report errors that originate
from a violated precondition, like an invalid parameter that is passed
to an API function. Errors that happen during the execution of a
function, for example failure of initializing the engine due to a
missing hardware feature, can be catched by checking the return value
of the corresponding API function.  More information about the error
can be retrieved by checking the message queue, provided that the
message level is set accordingly.

Parameters:
	none

Returns:
	true in there was an error, otherwise false
")

;; void h3dInit();
(defh3fun ("h3dInit" init) boolean
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

;; void h3dRelease();
(defh3fun ("h3dRelease" release) void
  "Releases the engine.

 This function releases the engine and frees all objects and associated
 memory. It should be called when the application is destroyed.

 Parameters:
         none

 Returns:
         nothing
")

;; void h3dSetupViewport( int x, int y, int width, int height, bool resizeBuffers);
(defh3fun ("h3dSetupViewport" setup-viewport) void
  "Sets the location and size of the viewport.

  This function sets the location and size of the viewport. It has to be called
  after engine initialization and whenever the size of the rendering context/window
  changes. The resizeBuffers parameter specifies whether render targets with a size
  relative to the viewport dimensions should be resized. This is usually desired
  after engine initialization and when the window is resized but not for just rendering
  to a part of the framebuffer.


  Parameters:
          x              - the x-position of the lower left corner of the viewport rectangle
          y              - the y-position of the lower left corner of the viewport rectangle
          width          - the width of the viewport
          height         - the height of the viewport
          resizeBuffers  - specifies whether render targets should be adapted to new size

  Returns:
          nothing
"
  (x int) (y int) (width int) (heigth int) (resize-buffers boolean))



;; bool h3dRender( H3DNode cameraNode );
(defh3fun ("h3dRender" render) void
  "Main rendering function.

  This is the main function of the engine. It executes all the rendering,
  animation and other tasks. The function can be called several times per frame,
  for example in order to write to different output buffers.

  Parameters:
          cameraNode      - camera node used for rendering scene

  Returns:
          nothing
"
  (camera-node node))


;; bool h3dFinalizeFrame();
(defh3fun ("h3dFinalizeFrame" finalize-frame) void
  "Marker for end of frame.

This function tells the engine that the current frame is finished and that all
subsequent rendering operations will be for the next frame.

Parameters:
        none

Returns:
        nothing
")

;; void h3dClear();
(defh3fun ("h3dClear" clear) void
  "Removes all resources and scene nodes.

  This function removes all nodes from the scene graph except the root node and
  releases all resources.

  Warning: All resource and node IDs are invalid after calling this function

  Parameters:
          none

  Returns:
          nothing
")

;; const char *h3dGetMessage( int *level, float *time );
(defh3fun ("h3dGetMessage" get-message) string
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
  (level (:pointer int))
  (time (:pointer float)))

;; float h3dGetOption( H3DOptions::List param );
(defh3fun ("h3dGetOption" get-option) float
  "Gets an option parameter of the engine.

  This function gets a specified option parameter and returns its value.

  Parameters:
          parameter   - option parameter

  Returns:
          current value of the specified option parameter
"
  (parameter option))

;; bool h3dSetOption( H3DOptions::List param, float value );
(defh3fun ("h3dSetOption" set-option) boolean
  "Sets an option parameter for the engine.

  This function sets a specified option parameter to a specified value.

  Parameters:
          parameter   - option parameter
          value   - value of the option parameter

  Returns:
          true if the option could be set to the specified value, otherwise false
"
  (parameter option)
  (value float))

;; float h3dGetStat( EngineStats::List param, bool reset );
(defh3fun ("h3dGetStat" get-statistics) float
  "Gets a statistic value of the engine.

This function returns the value of the specified statistic. The reset flag makes
it possible to reset the statistic value after reading.

Parameters:
        parameter   - statistic parameter
        reset   - flag specifying whether statistic value should be reset

Returns:
        current value of the specified statistic parameter
"
  (parameter statistics)
  (reset boolean))

;; void h3dShowOverlay( float x_tl, float y_tl, float u_tl, float v_tl,
;;                          float x_bl, float y_bl, float u_bl, float v_bl,
;;                          float x_br, float y_br, float u_br, float v_br,
;;                          float x_tr, float y_tr, float u_tr, float v_tr,
;;                          float colR, float colG, float colB, float colA,
;;                          H3DRes materialRes, int layer );
(defh3fun ("h3dShowOverlay" show-overlay) void
  "Shows an overlay on the screen.

This function displays an overlay with a specified material at a specified position on the screen.
An overlay is a 2D image that can be used to render 2D GUI elements. The coordinate system
used has its origin (0, 0) at the top-left corner of the screen and its maximum (1, 1) at
the bottom-right corner. Texture coordinates are using a system where the coordinates (0, 0)
correspond to the lower left corner of the image.
Overlays can have different layers which describe the order in which they are drawn. Overlays with
smaller layer numbers are drawn before overlays with higher layer numbers.
Note that the overlays have to be removed manually using the function clearOverlays.

Parameters:
        x-tl, y-tl, u-tl, v-tl      - position and texture coordinates of the top-left corner
        x-bl, y-bl, u-bl, v-bl      - position and texture coordinates of the bottom-left corner
        x-br, y-br, u-br, v-br      - position and texture coordinates of the bottom-right corner
        x-tr, y-tr, u-tr, v-tr      - position and texture coordinates of the top-right corner
        col-r, col-g, col-b, col-a  - color of the overlay that is set for the material's shader
        material-resource           - material resource used for rendering
        layer                       - layer index of the overlay (Values: from 0 to 7)

Returns:
        nothing
"
  (x-tl float) (y-tl float) (u-tl float) (v-tl float)
  (x-bl float) (y-bl float) (u-bl float) (v-bl float)
  (x-br float) (y-br float) (u-br float) (v-br float)
  (x-tr float) (y-tr float) (u-tr float) (v-tr float)
  (col-r float) (col-g float) (col-b float) (col-a float)
  (material-resource resource)
  (layer int))

;; void h3dClearOverlays();
(defh3fun ("h3dClearOverlays" clear-overlays) void
  "Removes all overlays.

This function removes all overlays that were added using showOverlay.

Parameters:
        none

Returns:
        nothing
")

;;;; ---------------------------------------------------------------------------
;;;; * Group: General resource management functions

;; int h3dGetResType( H3DRes res );
(defh3fun ("h3dGetResType" get-resource-type) resource-type
  "Returns the type of a resource.

This function returns the type of a specified resource. If the resource handle
is invalid, he function returns the resource type 'Unknown'.

Parameters:
        resource  - handle to the resource

Returns:
        type of the resource
"
  (resource resource))


;; const char *h3dGetResName( H3DRes res );
(defh3fun ("h3dGetResName" get-resource-name) string
  "Returns the name of a resource.

This function returns a pointer to the name of a specified resource. If the
resource handle is invalid, the function returns an empty string.

Important Note: The pointer is const and allows only read access to the data. Do never try to modify the
data of the pointer since that can corrupt the engine's internal states!*

Parameters:
        resource  - handle to the resource

Returns:
        name of the resource or empty string in case of failure
"
  (resource resource))


;; H3DRes h3dGetNextResource( int type, H3DRes start );
(defh3fun ("h3dGetNextResource" get-next-resource) resource
  "Returns the next resource of the specified type.

This function searches the next resource of the specified type and returns its handle.
The search begins after the specified start handle. If a further resource of the queried type
does not exist, a zero handle is returned. The function can be used to iterate over all
resources of a given type by using as start the return value of the previous iteration step.
The first iteration step should start at 0 and iteration can be ended when the function returns 0.

Parameters:
        type   - type of resource to be searched (ResourceTypes::Undefined for all types)
        start  - resource handle after which the search begins (can be 0 for beginning of resource list)

Returns:
        handle to the found resource or 0 if it does not exist
"
  (type resource-type) (start resource))


;; H3DRes h3dFindResource( int type, const char *name );
(defh3fun ("h3dFindResource" find-resource) resource
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
  (name string))


;; H3DRes h3dAddResource( int type, const char *name, int flags );
(defh3fun ("h3dAddResource" add-resource) resource
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
  (name string)
  (flags resource-flags))

;; H3DRes h3dCloneResource( H3DRes sourceRes, const char *name );
(defh3fun ("h3dCloneResource" clone-resource) resource
  "Duplicates a resource.

This function duplicates a specified resource. In the cloning process a new
resource with the specified name is added to the resource manager and filled
with the data of the specified source resource. If the specified name for the
new resource is already in use, the function fails and returns 0. If the name
string is empty, a unique name for the resource is generated automatically.

*Note: The name string may not contain a colon character (:)*

Parameters:
        resource - handle to resource to be cloned
        name     - name of new resource (can be empty for auto-naming)

Returns:
        handle to the cloned resource or 0 in case of failure
"
  (resource resource)
  (name string))


;; int h3dRemoveResource( H3DRes res );
(defh3fun ("h3dRemoveResource" remove-resource) int
  "Removes a resource.

This function decreases the user reference count of a specified resource. When
the user reference count is zero and there are no internal references, the
resource can be released and removed using the API fuction
releaseUnusedResources.

Parameters:
        resource  - handle to the resource to be removed

Returns:
        the number of references that the application is still holding after removal or -1 in case of an error
"
  (resource resource))

;; bool h3dIsResLoaded( H3DRes res );
(defh3fun ("h3dIsResLoaded" resource-loaded-p) boolean
  "Checks if a resource is loaded.

This function checks if the specified resource has been successfully loaded.

Parameters:
        resource   - handle to the resource to be checked

Returns:
        true if resource is loaded, otherwise or in case of failure false
"
  (resource resource))

;; bool h3dLoadResource( H3DRes res, const char *data, int size );
(defh3fun ("h3dLoadResource" load-resource) boolean
  "Loads a resource.

This function loads data for a resource that was previously added to the
resource manager.  f data is a NULL-pointer the resource manager is told that
the resource doesn't have any data
e.g. the corresponding file was not found. In this case, the resource remains in the unloaded state
but is no more returned when querying unloaded resources. When the specified resource is already loaded,
the function returns false.

Important Note: XML-data must be NULL-terminated*

Parameters:
        resource - handle to the resource for which data will be loaded
        data     - pointer to the data to be loaded
        size     - size of the data block

Returns:
        true in case of success, otherwise false
"
  (resource resource)
  (data :pointer)
  (size int))

;; void h3dUnloadResource( H3DRes res );
(defh3fun ("h3dUnloadResource" unload-resource) void
  "Unloads a resource.

This function unloads a previously loaded resource and restores the
default values t had before loading. The state is set back to unloaded
which makes it possible to load he resource again.

Parameters:
        resource  - handle to resource to be unloaded

Returns:
        nothing
"
  (resource resource))

;; int h3dGetResElemCount( H3DRes res, int elem );
(defh3fun ("h3dGetResElemCount" get-resource-element-count) int
  "Gets the number of elements in a resource.

This function returns how many elements of the specified element type
a specified resource has.

Parameters:
        resource - handle to the resource to be accessed
        element  - element type

Returns:
        number of elements
"
  (resource resource)
  (element resource-element))

;; int h3dFindResElem( H3DRes res, int elem, int param, const char *value );
(defh3fun ("h3dFindResElem" find-resource-element) int
  "Finds a resource element with the specified property value.

This function searches in a specified resource for the first element
of the specified type that has the property with the specified name
set to the specified search value.  If such element is found, its
index is returned, otherwise the function returns -1. All string
comparisons done for the search are case-sensitive.

Parameters:
        resource   - handle to the resource to be accessed
        element    - element type
        parameter  - parameter name
        value      - parameter value to be searched for

Returns:
        index of element or -1 if element not found
"
  (resource resource)
  (element resource-element)
  (parameter resource-parameter)
  (value string))

;; int h3dGetResParamI( H3DRes res, int element, int elemIdx, int param );
(defh3fun ("h3dGetResParamI" get-resource-parameter-i) int
  "Gets a property of a resource element.

This function returns a specified property of the specified resource
element.  The property must be of the type int.

Parameters:
        resource       - handle to the resource to be accessed
        element        - element type
        element-index  - index of element
        parameter      - parameter to be accessed

Returns:
        value of the parameter
"
  (resource resource)
  (element resource-element)
  (element-index int)
  (parameter resource-parameter))

;; void h3dSetResParamI( H3DRes res, int element, int elemIdx, int param, int value );
(defh3fun ("h3dSetResParamI" set-resource-parameter-i) void
  "Sets a property of a resource element.

This function sets a specified property of the specified resource
element to a specified value.  The property must be of the type int.

Parameters:
        resource       - handle to the resource to be accessed
        element        - element type
        element-index  - index of element
        parameter      - parameter to be modified
        value          - new value for the specified parameter

Returns:
         nothing
"
  (resource resource)
  (element resource-element)
  (element-index int)
  (parameter resource-parameter)
  (value int))

;; float h3dGetResParamF( H3DRes res, int elem, int elemIdx, int param, int compIdx );
(defh3fun ("h3dGetResParamF" get-resource-parameter-f) float
  "Gets a property of a resource element.

This function returns a specified property of the specified resource
element.  The property must be of the type float.

Parameters:
        resource         - handle to the resource to be accessed
        element          - element type
        element-index    - index of element
        parameter        - parameter to be accessed
	component-index  - component of the parameter to be accessed

Returns:
        value of the parameter
"
  (resource resource)
  (element resource-element)
  (element-index int)
  (parameter resource-parameter)
  (component-index int))

;; void h3dSetResParamF( H3DRes res, int elem, int elemIdx, int param, int compIdx, float value );
(defh3fun ("h3dSetResParamF" set-resource-parameter-f) void
  "Sets a property of a resource element.

This function sets a specified property of the specified resource
element to a specified value.  The property must be of the type float.

Parameters:
        resource         - handle to the resource to be accessed
        element          - element type
        element-index    - index of element
        parameter        - parameter to be accessed
	component-index  - component of the parameter to be accessed
        value            - new value for the specified parameter

Returns:
         nothing
"
  (resource resource)
  (element resource-element)
  (element-index int)
  (parameter resource-parameter)
  (component-index int)
  (value float))

;; const char *h3dGetResParamStr( H3DRes res, int elem, int elemIdx, int param );
(defh3fun ("h3dGetResParamStr" get-resource-parameter-str) string
  "Gets a property of a resource element.

This function returns a specified property of the specified resource
element.  The property must be of the type string (const char *).

*Important Note: The pointer is const and allows only read access to the data. Do never try to modify the
ata of the pointer since that can corrupt the engine's internal states!*

Parameters:
        resource       - handle to the resource to be accessed
        element        - element type
        element-index  - index of element
        parameter      - parameter to be accessed

Returns:
        value of the property or empty string if no such property exists
"
  (resource resource)
  (element resource-element)
  (element-index int)
  (parameter resource-parameter))


;; void h3dSetResParamStr( H3DRes res, int elem, int elemIdx, int param, const char *value );
(defh3fun ("h3dSetResParamStr" set-resource-parameter-str) void
  "Sets a property of a resource element.

This function sets a specified property of the specified resource
element to a specified value.  The property must be of the type
string (const char *).

Parameters:
        resource       - handle to the resource to be accessed
        element        - element type
        element-index  - index of element
        parameter      - parameter to be accessed
        value          - new value for the specified parameter

Returns:
         true in case of success otherwise false
"
  (resource resource)
  (element resource-element)
  (element-index int)
  (parameter resource-parameter)
  (value string))

;; void *h3dMapResStream( H3DRes res, int elem, int elemIdx, int stream, bool read, bool write );
(defh3fun ("h3dMapResStream" map-resource-stream) :pointer
  "Maps the stream of a resource element.

This function maps the specified stream of a specified resource
element and returns a pointer to the stream data. The required access
to the data can be specified with the read write parameters. If read
is false, the pointer will usually not contain meaningful data.  Not
all resource streams can be mapped with both read and write access. If
it is not possible to map the stream, the function will return a NULL
pointer. A mapped stream should be unmapped again as soon as possible
but always before subsequent API calls are made. It is only possible
to map one stream per resource at a time.

Parameters:
        resource       - handle to the resource to be accessed
        element        - element type
        element-index  - index of element
	stream         - stream to be mapped
	read           - flag indicating whether read access is required
	write          - flag indicating whether write access is required

Returns:
	pointer to stream data or NULL if stream cannot be mapped
"
  (resource resource)
  (element resource-element)
  (element-index int)
  (stream int)
  (read boolean)
  (write boolean))

;; void h3dUnmapResStream( H3DRes res );
(defh3fun ("h3dUnmapResStream" unmap-resource-stream) void
  "Unmaps a previously mapped resource stream.

This function unmaps a resource stream that has been mapped before.

Parameters:
	resource  - handle to the resource to be unmapped

Returns:
	nothing
"
  (resource resource))

;; H3DRes h3dQueryUnloadedResource( int index );
(defh3fun ("h3dQueryUnloadedResource" query-unloaded-resource) resource
  "Returns handle to an unloaded resource.

This function looks for a resource that is not yet loaded and returns its
handle.  If there are no unloaded resources or the zero based index specified is
greater than the number of the currently unloaded resources, 0 is returned.

Parameters:
        index    - index of unloaded resource within the internal list of unloaded resources (starting with 0)

Returns:
        handle to an unloaded resource or 0
"
  (index int))


;; void h3dReleaseUnusedResources();
(defh3fun ("h3dReleaseUnusedResources" release-unused-resources) void
  "Frees resources that are no longer used.

This function releases resources that are no longer used. Unused resources were
either told to be released by the user calling removeResource or are no more
referenced by any other engine objects.

Parameters:
        none

Returns:
        nothing
")

;;;; ---------------------------------------------------------------------------
;;;; * Group: Specific resource management functions

;; H3DRes h3dCreateTexture( const char *name, int width, int height, int fmt, int flags );
(defh3fun ("h3dCreateTexture" create-texture) resource
  "Adds a Texture2D resource.

This function tries to create and add a Texture resource with the specified
name to the resource manager. If a Texture resource with the same name is
already existing, the function fails. The texture is initialized with the
specified dimensions and the resource is declared as loaded. This function is
especially useful to create dynamic textures (e.g. for displaying videos) or
output buffers for render-to-texture.

*Note: The name string may not contain a colon character (:)*

Parameters:
        name    - name of the resource
        width   - width of the texture image
        height  - height of the texture image
        format  - texture format (see stream formats)
        flags   - flags used for creating the resource

Returns:
        handle to the created resource or 0 in case of failure
"
  (name string)
  (width int)
  (height int)
  (format resource-format)
  (flags resource-flags))


;; void h3dSetShaderPreambles( const char *vertPreamble, const char *fragPreamble );
(defh3fun ("h3dSetShaderPreambles" set-shader-preambles) void
  "Sets preambles of all Shader resources.

This function defines a header that is inserted at the beginning of all
shaders. The preamble is used when a shader is compiled, so changing it will not
affect any shaders that are already compiled. The preamble is useful for setting
platform-specific defines that can be employed for creating several shader code
paths, e.g. for supporting different hardware capabilities.

Parameters:
        vertPreamble  - preamble text of vertex shaders (default: empty string)
        fragPreamble  - preamble text of fragment shaders (default: empty string)

Returns:
        nothing
"
  (vert-preamble string)
  (frag-preamble string))

;; bool h3dSetMaterialUniform( H3DRes materialRes, const char *name, float a, float b, float c, float d );
(defh3fun ("h3dSetMaterialUniform" set-material-uniform) boolean
  "Sets a shader uniform of a Material resource.

This function sets the specified shader uniform of the specified material to the
specified values.

Parameters:
        material-resource  - handle to the material resource to be accessed
        name               - name of the uniform as defined in Material resource
        a, b, c, d         - values of the four components

Returns:
        true if uniform was found, otherwise false
"
  (material-resource resource)
  (name string)
  (a float)
  (b float)
  (c float)
  (d float))


;; bool h3dGetPipelineRenderTargetData( H3DRes pipelineRes, const char *targetName,
;;                                      int bufIndex, int *width, int *height, int *compCount,
;;                                      float *dataBuffer, int bufferSize );
(defh3fun ("h3dGetPipelineRenderTargetData" get-pipeline-render-target-data) boolean
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
        pipeline-resource  - handle to pipeline resource
        target-name        - unique name of render target to access
        buffer-index       - index of buffer to be accessed
        width              - pointer to variable where the width of the buffer will be stored (can be NULL)
        height             - pointer to variable where the height of the buffer will be stored (can be NULL)
        component-count    - pointer to variable where the number of components will be stored (can be NULL)
        data-buffer        - pointer to float array where the pixel data will be stored (can be NULL)
        buffer-size        - size of dataBuffer array in bytes

Returns:
        true if render target could be found, otherwise false
"
  (pipeline-resource resource)
  (target-name string)
  (buffer-index int)
  (width (:pointer int))
  (height (:pointer int))
  (component-count (:pointer int))
  (data-buffer (:pointer float))
  (buffer-size int))

;;;; ---------------------------------------------------------------------------
;;;; * Group: General Scene Graph Functions


;; int h3dGetNodeType( H3DNode node );
(defh3fun ("h3dGetNodeType" get-node-type) node-type
  "Returns the type of a scene node.

This function returns the type of a specified scene node. If the node handle is
invalid, he function returns the node type 'Unknown'.

Parameters:
        node    - handle to the scene node

Returns:
        type of the scene node
"
  (node node))


;; H3DNode h3dGetNodeParent( H3DNode node );
(defh3fun ("h3dGetNodeParent" get-node-parent) node
  "Returns the parent of a scene node.

This function returns the handle to the parent node of a specified scene
node. If the specified node handle is invalid or the root node, 0 is returned.

Parameters:
        node    - handle to the scene node

Returns:
        handle to parent node or 0 in case of failure
"
  (node node))


;; bool h3dSetNodeParent( H3DNode node, H3DNode parent );
(defh3fun ("h3dSetNodeParent" set-node-parent) boolean
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
  (node node) (parent node))


;; H3DNode h3dGetNodeChild( H3DNode node, int index );
(defh3fun ("h3dGetNodeChild" get-node-child) node
  "Returns the handle to a child node.

This function looks for the n-th (index) child node of a specified node and
returns its handle. If the child doesn't exist, the function returns 0.

Parameters:
        node    - handle to the parent node
        index   - index of the child node

Returns:
        handle to the child node or 0 if child doesn't exist
"
  (node node) (index int))


;; H3DNode h3dAddNodes( H3DNode parent, H3DRes sceneGraphRes );
(defh3fun ("h3dAddNodes" add-nodes) node
  "Adds nodes from a SceneGraph resource to the scene.

This function creates several new nodes as described in a SceneGraph resource
and ttaches them to a specified parent node. If an invalid scenegraph resource
is specified or the scenegraph resource is unloaded, the function returns 0.

Parameters:
        parent                 - handle to parent node to which the root of the
                                 new nodes will be attached
        scene-graph-resource   - handle to loaded SceneGraph resource

Returns:
        handle to the root of the created nodes or 0 in case of failure
"
  (parent node) (scene-graph-resource resource))


;; void h3dRemoveNode( H3DNode node );
(defh3fun ("h3dRemoveNode" remove-node) void
  "Removes a node from the scene.

This function removes the specified node and all of it's children from the
scene.

Parameters:
        node  - handle to the node to be removed

Returns:
        nothing
"
  (node node))


;; void h3dSetNodeActivation( H3DNode node, bool active );
(defh3fun ("h3dSetNodeActivation" set-node-activation) void
  "Sets the activation (visibility) state of a node.

This function sets the activation state of the specified node to active or
inactive. Inactive odes with all their children are excluded from rendering.

Parameters:
        node    - handle to the node to be modified
        active  - boolean value indicating whether node shall be active or inactive

Returns:
        nothing
"
  (node node) (active boolean))


;; bool h3dCheckNodeTransFlag( H3DNode node, bool reset );
(defh3fun ("h3dCheckNodeTransFlag" check-node-transform-flag) boolean
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
  (node node) (reset boolean))


;; bool h3dGetNodeTransform( H3DNode node, float *tx, float *ty, float *tz,
;;                                         float *rx, float *ry, float *rz,
;;                                         float *sx, float *sy, float *sz );
(defh3fun ("h3dGetNodeTransform" get-node-transform) void
  "Gets the relative transformation of a node.

This function gets the translation, rotation and scale of a specified scene node
object. The coordinates are in local space and contain the transformation of the
node relative to its parent.

Parameters:
        node        - handle to the node which will be accessed
        tx, ty, tz  - pointers to variables where translation of the node will be stored (can be NULL)
        rx, ry, rz  - pointers to variables where rotation of the node in Euler angles
                      will be stored (can be NULL)
        sx, sy, sz  - pointers to variables where scale of the node will be stored (can be NULL)

Returns:
        nothing
"
  (node node)
  (tx (:pointer float)) (ty (:pointer float)) (tz (:pointer float))
  (rx (:pointer float)) (ry (:pointer float)) (rz (:pointer float))
  (sx (:pointer float)) (sy (:pointer float)) (sz (:pointer float)))


;; bool h3dSetNodeTransform( H3DNode node, float tx, float ty, float tz,
;;                                         float rx, float ry, float rz,
;;                                         float sx, float sy, float sz );
(defh3fun ("h3dSetNodeTransform" set-node-transform) void
  "Sets the relative transformation of a node.

This function sets the relative translation, rotation and scale of a specified
scene node object.  The coordinates are in local space and contain the
transformation of the node relative to its parent.

Parameters:
        node        - handle to the node which will be modified
        tx, ty, tz  - translation of the node
        rx, ry, rz  - rotation of the node in Euler angles
        sx, sy, sz  - scale of the node

Returns:
        nothing
"
  (node node)
  (tx float) (ty float) (tz float)
  (rx float) (ry float) (rz float)
  (sx float) (sy float) (sz float))


;; void h3dGetNodeTransMats( H3DNode node, const float **relMat, const float **absMat );
(defh3fun ("h3dGetNodeTransMats" get-node-transform-matrices) void
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
        nothing
"
  (node node)
  (rel-mat :pointer)
  (abs-mat :pointer))

;; void h3dSetNodeTransMat( H3DNode node, const float *mat4x4 );
(defh3fun ("h3dSetNodeTransMat" set-node-transform-matrix) void
  "Sets the relative transformation matrix of a node.

This function sets the relative transformation matrix of the specified scene
node. It is basically the ame as setNodeTransform but takes directly a matrix
instead of individual transformation parameters.

Parameters:
        node    - handle to the node which will be modified
        mat4x4  - pointer to a 4x4 matrix in column major order

Returns:
        nothing
"
  (node node)
  (mat-4x4 :pointer))

;; int h3dGetNodeParamI( H3DNode node, int param );
(defh3fun ("h3dGetNodeParamI" get-node-parameter-i) int
  "Gets a property of a scene node.

This function returns a specified property of the specified node.  The
property must be of the type int or H3DRes.

Parameters:
        node       - handle to the node to be accessed
        parameter  - parameter to be accessed

Returns:
        value of the parameter
"
  (node node)
  (parameter node-parameter))

;; bool h3dSetNodeParamI( H3DNode node, int param, int value );
(defh3fun ("h3dSetNodeParamI" set-node-parameter-i) void
  "Sets a property of a scene node.

This function sets a specified property of the specified node to a
specified value.  The property must be of the type int or H3DRes.

Parameters:
        node       - handle to the node to be modified
        parameter  - parameter to be modified
        value      - new value for the specified parameter

Returns:
        nothing
"
  (node node)
  (parameter node-parameter)
  (value int))

;; float h3dGetNodeParamF( H3DNode node, int param, int compIdx );
(defh3fun ("h3dGetNodeParamF" get-node-parameter-f) float
  "Gets a property of a scene node.

This function returns a specified property of the specified node.  The
property must be of the type float.

Parameters:
        node             - handle to the node to be accessed
        parameter        - parameter to be accessed
        component-index  - component of the parameter to be modified

Returns:
         value of the parameter
"
  (node node)
  (parameter node-parameter)
  (component-index int))

;; void h3dSetNodeParamF( H3DNode node, int param, int compIdx, float value );
(defh3fun ("h3dSetNodeParamF" set-node-parameter-f) void
  "Sets a property of a scene node.

This function sets a specified property of the specified node to a
specified value.  The property must be of the type float.

Parameters:
        node             - handle to the node to be modified
        parameter        - parameter to be modified
        component-index  - component of the parameter to be modified
        value            - new value for the specified parameter

Returns:
        nothing
"
  (node node)
  (parameter node-parameter)
  (component-index int)
  (value float))

;; const char *h3dGetNodeParamStr( H3DNode node, int param );
(defh3fun ("h3dGetNodeParamStr" get-node-parameter-str) string
  "Gets a property of a scene node.

This function returns a specified property of the specified node.  The property
must be of the type string (const char *).

*Important Note: The pointer is const and allows only read access to
the data. Do never try to modify the ata of the pointer since that can
corrupt the engine's internal states!*

Parameters:
        node       - handle to the node to be accessed
        parameter  - parameter to be accessed

Returns:
         value of the property or empty string if no such property exists
"
  (node node)
  (parameter node-parameter))

;; void h3dSetNodeParamstr( H3DNode node, int param, const char *value );
(defh3fun ("h3dSetNodeParamStr" set-node-parameter-str) void
  "Sets a property of a scene node.

This function sets a specified property of the specified node to a specified
value.  The property must be of the type string (const char *).

Parameters:
        node       - handle to the node to be modified
        parameter  - parameter to be modified
        value      - new value for the specified parameter

Returns:
        nothing
"
  (node node)
  (parameter node-parameter)
  (value string))

;; void h3dGetNodeAABB( H3DNode node, float *minX, float *minY, float *minZ,
;;                                    float *maxX, float *maxY, float *maxZ );
(defh3fun ("h3dGetNodeAABB" get-node-aabb) void
  "Gets the bounding box of a scene node.

This function stores the world coordinates of the axis aligned bounding box of a
specified node in the specified variables. The bounding box is represented using
the minimum and maximum coordinates on all three axes.

Parameters:
        node                 - handle to the node which will be accessed
        min-x, min-y, min-z  - pointers to variables where minimum coordinates will be stored
        max-x, max-y, max-z  - pointers to variables where maximum coordinates will be stored

Returns:
        nothing
"
  (node node)
  (min-x (:pointer float)) (min-y (:pointer float)) (min-z (:pointer float))
  (max-x (:pointer float)) (max-y (:pointer float)) (max-z (:pointer float)))

;; int h3dFindNodes( H3DNode startNode, const char *name, int type );
(defh3fun ("h3dFindNodes" find-nodes) int
  "Finds scene nodes with the specified properties.

This function loops recursively over all children of startNode and adds them to
an internal list of results if they match the specified name and type. The
result list is cleared each time this function is called. The function returns
the number of nodes which were found and added to the list.

Parameters:
        start-node      - handle to the node where the search begins
        name            - name of nodes to be searched (empty string for all nodes)
        type            - type of nodes to be searched (SceneNodeTypes::Undefined for all types)

Returns:
        number of search results
"
  (start-node node)
  (name string)
  (type node-type))

;; H3DNode h3dGetNodeFindResult( int index );
(defh3fun ("h3dGetNodeFindResult" get-node-find-result) node
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
  (index int))


;; int h3dCastRay( H3DNode node, float ox, float oy, float oz,
;;                 float dx, float dy, float dz, int numNearest );
(defh3fun ("h3dCastRay" cast-ray) int
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
        num-nearest     - maximum number of intersection points to be stored (0 for all)

Returns:
        number of intersections
"
  (node node)
  (ox float) (oy float) (oz float)
  (dx float) (dy float) (dz float)
  (num-nearest int))

;; bool h3dGetCastRayResult( int index, H3DNode *node, float *distance, float *intersection );
(defh3fun ("h3dGetCastRayResult" get-cast-ray-result) boolean
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
  (index int)
  (node (:pointer node))
  (distance (:pointer float))
  (intersection (:pointer float)))


;; int h3dCheckNodeVisibility( H3DNode node, H3DNode cameraNode, bool checkOcclusion, bool calcLod );

(defh3fun ("h3dCheckNodeVisibility" check-node-visibility) int
  "Checks if a node is visible.

This function checks if a specified node is visible from the perspective of a specified
camera. The function always checks if the node is in the camera's frustum. If checkOcclusion
is true, the function will take into account the occlusion culling information from the previous
frame (if occlusion culling is disabled the flag is ignored). The flag calcLod determines whether the
detail level for the node should be returned in case it is visible. The function returns -1 if the node
is not visible, otherwise 0 (base LOD level) or the computed LOD level.

Parameters:
        node             - node to be checked for visibility
        camera-node      - camera node from which the visibility test is done
        occlusion-p      - specifies if occlusion info from previous frame should be taken into account
        calc-lod         - specifies if LOD level should be computed

Returns:
        computed LOD level or -1 if node is not visible
"
  (node node)
  (camera-node node)
  (occlusion-p boolean)
  (calc-lod boolean))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Group-specific scene graph functions


;; H3DNode h3dAddGroupNode( H3DNode parent, const char *name );
(defh3fun ("h3dAddGroupNode" add-group-node) node
  "Adds a Group node to the scene.

This function creates a new Group node and attaches it to the specified parent
node.

Parameters:
        parent  - handle to parent node to which the new node will be attached
        name    - name of the node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node)
  (name string))


;;;; ---------------------------------------------------------------------------
;;;; * Group: Model-specific scene graph functions


;; H3DNode h3dAddModelNode( H3DNode parent, const char *name, H3DRes geometryRes );
(defh3fun ("h3dAddModelNode" add-model-node) node
  "Adds a Model node to the scene.

This function creates a new Model node and attaches it to the specified parent
node.

Parameters:
        parent            - handle to parent node to which the new node will be attached
        name              - name of the node
        geometry-resource - Geometry resource used by Model node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node)
  (name string)
  (geometry-resource resource))


;; void h3dSetupModelAnimStage( H3DNode modelNode, int stage, H3DRes animationRes,
;;                              int layer, const char *startNode, bool additive);
(defh3fun ("h3dSetupModelAnimStage" setup-model-animation-stage) void
  "Configures an animation stage of a Model node.

This function is used to setup the specified animation stage (channel) of the specified Model node.

The function is used for animation blending. There is a fixed number of
stages (by default 16) on which different animations can be played. The start
node determines the first node (Joint or Mesh) to which the animation is
recursively applied. If the start node is an empty string, the animation affects
all animatable nodes (Joints and Meshes) of the model. If a NULL is used
for animationRes, the stage is cleared and the previous animation is removed.

A simple way to do animation mixing is using additive animations. If a stage is
configured to be additive the engine calculates the difference between the
current frame and the first frame in the animation and adds this delta to the
current transformation of the joints or meshes.

Parameters:
        model-node          - handle to the Model node to be modified
        stage               - index of the animation stage to be configured
        animation-resource  - handle to Animation resource (can be 0)
        layer               - layer id
        start-node          - name of first node to which animation shall be applied (or empty string)
        additive            - flag indicating whether stage is additive

Returns:
        nothing
"
  (model-node node)
  (stage int)
  (animation-resource resource)
  (layer int)
  (start-node string)
  (additive boolean))


;; void h3dSetModelAnimParams( H3DNode modelNode, int stage, float time, float weight );
(defh3fun ("h3dSetModelAnimParams" set-model-animation-parameters) void
  "Sets the parameters of an animation stage in a Model node.

This function sets the current animation time and weight for a specified stage
of the specified model.  The time corresponds to the frames of the animation and
the animation is looped if the time is higher than the maximum number of frames
in the Animation resource. The weight is used for animation blending and
determines how much influence the stage has compared to the other active
stages. When the sum of the weights of all stages is more than one, the
animations on the lower stages get priority.

Parameters:
        model-node      - handle to the Model node to be modified
        stage           - index of the animation stage to be modified
        time            - new animation time/frame
        weight          - new blend weight

Returns:
         nothing
"
  (model-node node)
  (stage int)
  (time float)
  (weight float))

;; bool h3dSetModelMorpher( H3DNode modelNode, const char *target, float weight );
(defh3fun ("h3dSetModelMorpher" set-model-morpher) boolean
  "Sets the weight of a morph target.

This function sets the weight of a specified morph target. If the
target parameter is an empty string the weight of all morph targets in
the specified Model node is modified.  If the specified morph target
is not found the function returns false.

Parameters:
        modelNode       - handle to the Model node to be modified
        target          - name of morph target
        weight          - new weight for morph target

Returns:
        true if morph target was found, otherwise false
"
  (model-node node)
  (target string)
  (weight float))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Mesh-specific scene graph functions


;; H3DNode h3dAddMeshNode( H3DNode parent, const char *name, H3DRes materialRes,
;;                         int batchStart, int batchCount,
;;                         int vertRStart, int vertREnd );
(defh3fun ("h3dAddMeshNode" add-mesh-node) node
  "Adds a Mesh node to the scene.

This function creates a new Mesh node and attaches it to the specified parent
node.

Parameters:
        parent-node       - handle to parent node to which the new node will be attached
        name              - name of the node
        material-resource - material resource used by Mesh node
        batch-start       - first triangle index of mesh in Geometry resource of parent Model node
        batch-count       - number of triangle indices used for drawing mesh
        vertex-rstart     - first vertex in Geometry resource of parent Model node
        vertex-rend       - last vertex in Geometry resource of parent Model node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent-node node)
  (name string)
  (material-resource resource)
  (batch-start int)
  (batch-count int)
  (vertex-r-start int)
  (vertex-r-end int))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Joint-specific scene graph functions

;; H3DNode h3dAddJointNode( H3DNode parent, const char *name, int jointIndex );
(defh3fun ("h3dAddJointNode" add-joint-node) node
  "Adds a Joint node to the scene.

This function creates a new Joint node and attaches it to the specified parent
node.

Parameters:
        parent-node     - handle to parent node to which the new node will be attached
        name            - name of the node
        joint-index     - index of joint in Geometry resource of parent Model node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent-node node)
  (name string)
  (joint-index int))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Light-specific scene graph functions

;; H3DNode h3dAddLightNode( H3DNode parent, const char *name, H3DRes materialRes,
;;                          const char *lightingContext, const char *shadowContext );
(defh3fun ("h3dAddLightNode" add-light-node) node
  "Adds a Light node to the scene.

This function creates a new Light node and attaches it to the specified parent
node. The direction vector of the untransformed light node is pointing along the
the negative z-axis. The specified material resource can define uniforms and
projective textures. Furthermore it can contain a shader for doing lighting
calculations if deferred shading s used. If no material is required the
parameter can be zero. The context names define which shader contexts are used
when rendering shadow maps or doing light calculations for forward rendering
configurations.

Parameters:
        parent-node       - handle to parent node to which the new node will be attached
        name              - name of the node
        material-resource - material resource for light configuration or 0 if not used
        lighting-context  - name of the shader context used for doing light calculations
        shadow-context    - name of the shader context used for doing shadow map rendering

Returns:
         handle to the created node or 0 in case of failure
"
  (parent-node node)
  (name string)
  (material-resource resource)
  (lighting-context string)
  (shadow-context string))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Camera-specific scene graph functions

;; H3DNode h3dAddCameraNode( H3DNode parent, const char *name, H3DRes pipelineRes );
(defh3fun ("h3dAddCameraNode" add-camera-node) node
  "Adds a Camera node to the scene.

This function creates a new Camera node and attaches it to the specified parent
node.

Parameters:
        parent-node       - handle to parent node to which the new node will be attached
        name              - name of the node
        pipeline-resource - pipeline resource used for rendering

Returns:
         handle to the created node or 0 in case of failure
"
  (parent-node node)
  (name string)
  (pipeline-resource resource))

;; void h3dSetupCameraView( H3DNode cameraNode, float fov, float aspect,
;;                          float nearDist, float farDist );
(defh3fun ("h3dSetupCameraView" setup-camera-view) void
  "Sets the planes of a camera viewing frustum.

This function calculates the view frustum planes of the specified camera node
using the specified view arameters.

Parameters:
        camera-node   - handle to the Camera node which will be modified
        fov           - field of view (FOV) angle
        aspect        - aspect ratio
        near-distance - distance of near clipping plane
        far-distance  - distance of far clipping plane

Returns:
         nothing
"
  (camera-node node)
  (fov float)
  (aspect float)
  (near-distance float)
  (far-distance float))

;; void h3dGetCameraProjMat( H3DNode cameraNode, float *projMat );
(defh3fun ("h3dGetCameraProjMat" get-camera-projection-matrix) void
  "Gets the camera projection matrix.

This function gets the camera projection matrix used for bringing the
geometry to screen space and copies it to the specified array.

Parameters:
        camera-node       - handle to Camera node
        projection-matrix - pointer to float array with 16 elements

Returns:
        nothing
"
  (camera-node node)
  (projection-matrix (:pointer float)))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Emitter-specific scene graph functions

;; H3DNode h3dAddEmitterNode( H3DNode parent, const char *name,
;;                            H3DRes materialRes, H3DRes effectRes,
;;                            int maxParticleCount, int respawnCount );
(defh3fun ("h3dAddEmitterNode" add-emitter-node) node
  "Adds a Emitter node to the scene.

This function creates a new Emitter node and attaches it to the specified parent
node.

Parameters:
        parent-node        - handle to parent node to which the new node will be attached
        name               - name of the node
        material-resource  - handle to Material resource used for rendering
        effect-resource    - handle to Effect resource used for configuring particle properties
        max-particle-count - maximal number of particles living at the same time
        respawn-count      - number of times a single particle is recreated after dying (-1 for infinite)


Returns:
         handle to the created node or 0 in case of failure
"
  (parent-node node)
  (name string)
  (material-resource resource)
  (effect-resource resource)
  (max-particle-count int)
  (respawn-count int))

;; void h3dAdvanceEmitterTime( H3DNode emitterNode, float timeDelta );
(defh3fun ("h3dAdvanceEmitterTime" advance-emitter-time) void
  "Advances the time value of an Emitter node.

This function advances the simulation time of a particle system and
continues the particle simulation with timeDelta being the time
elapsed since the last call of this function. The specified node must
be an Emitter node.

Parameters:
        emitter-node - handle to the Emitter node which will be modified
        time-delta   - time delta in seconds

Returns:
         true in case of success otherwise false
"
  (emitter-node node)
  (time-delta float))

;; bool h3dHasEmitterFinished( H3DNode emitterNode );
(defh3fun ("h3dHasEmitterFinished" emitter-finished-p) boolean
  "Checks if an Emitter node is still alive.

This function checks if a particle system is still active and has
living particles or will spawn new particles. The specified node must
be an Emitter node. The function can be used to check when a not
infinitely running emitter for an effect like an explosion can be
removed from the scene.

Parameters:
        emitter-node  - handle to the Emitter node which is checked

Returns:
         true if Emitter will no more emit any particles, otherwise or in case of failure false
"
  (emitter-node node))

;;;; -----------------------------------------------------------------------
;;;; * Horde3D Utilities

(defconstant +max-stat-mode+ 2
  "Maximum stat mode number supported in show-frame-stats.")

;; void h3dutFreeMem( char **ptr );
(defh3ufun ("h3dutFreeMem" free-mem) void
  "Frees memory allocated by the Utility Library.

This utility function frees the memory that was allocated by another function of the Utility Library.

Parameters:
  ptr  - address of a pointer that references to memory allocated by the Utility Library

Returns:
  nothing
"
  (ptr :pointer))

;; bool h3dutDumpMessages();
(defh3ufun ("h3dutDumpMessages" dump-messages) boolean
  "Writes all messages in the queue to a log file.

This utility function pops all messages from the message queue and writes them
to a HTML formated log file 'EngineLog.html'.

Parameters:
        none

Returns:
        true in case of success, otherwise false
")

;;;; ---------------------------------------------------------------------------
;;;; * Group: OpenGL-related functions

;; bool h3dutInitOpenGL( int hDC );
(defh3ufun ("h3dutInitOpenGL" init-open-gl) boolean
  "Initializes OpenGL.

This utility function initializes an OpenGL rendering context in a specified
window component.  *Currently this function is only available on Windows
platforms.*

Parameters:
        hDC    - handle to device context for which OpenGL context shall be created

Returns:
        true in case of success, otherwise false
"
  (h-dc int))

;; void h3dutReleaseOpenGL();
(defh3ufun ("h3dutReleaseOpenGL" release-open-gl) void
  "Releases OpenGL.

This utility function destroys the previously created OpenGL rendering context.
*Currently this function is only available on Windows platforms.*

Parameters:
        none

Returns:
        nothing
")

;; void h3dutSwapBuffers();
(defh3ufun ("h3dutSwapBuffers" swap-buffers) void
  "Displays the rendered image on the screen.

This utility function displays the image rendered to the previously initialized
OpenGL context on the screen by copying it from the backbuffer to the
frontbuffer.  *Currently this function is only available on Windows platforms.*

Parameters:
        none

Returns:
        nothing
")

;;;; ---------------------------------------------------------------------------
;;;; * Group: Resource management

;; const char *h3dutGetResourcePath( int type );
(defh3ufun ("h3dutGetResourcePath" get-resource-path) string
  "Returns  the search path of a resource type.

This function returns the search path of a specified resource type.

Parameters:
        type    - type of resource

Returns:
        pointer to the search path string
"
  (type resource-type))

;; void h3dutSetResourcePath( int type, const char *path );
(defh3ufun ("h3dutSetResourcePath" set-resource-path) void
  "Sets the search path for a resource type.

This function sets the search path for a specified resource type.

Parameters:
        type    - type of resource
        path    - path where the resources can be found ((back-)slashes at end are removed)

Returns:
        nothing
"
  (type resource-type)
  (path string))

;; bool h3dutLoadResourcesFromDisk( const char *contentDir );
(defh3ufun ("h3dutLoadResourcesFromDisk" load-resources-from-disk) boolean
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
  (content-dir string))


;; bool h3dutCreateTGAImage( const unsigned char *pixels, int width, int height,
;;                           int bpp, char **outData, int *outSize );
(defh3ufun ("h3dutCreateTGAImage" create-tga-image) boolean
  "Creates a TGA image in memory.

This utility function allocates memory at the pointer outData and creates a TGA
image from the specified pixel data. The dimensions of the image have to be
specified as well as the bit depth.  The created TGA-image-data can be used as
Texture2D or TexureCube resource in the engine.  *Note: The memory allocated by
this routine has to freed manually using the freeMem function.*

Parameters:
        pixels    - pointer to pixel source data in BGR(A) format from which TGA-image is constructed;
                    memory layout: pixel with position (x, y) in image (origin of image is upper left
                    corner) has memory location (y * width + x) * (bpp / 8) in pixels-array
        width     - width of source image
        height    - height of source image
        bpp       - color bit depth of source data (valid values: 24, 32)
        out-data  - address of a pointer to which the address of the created memory block is written
        out-size  - variable to which to size of the created memory block is written

Returns:
        false if at least one resource could not be loaded, otherwise true
"
  (pixels (:pointer :uchar))
  (width int) (height int)
  (bpp int)
  (out-data :pointer)
  (out-size (:pointer int)))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Scene graph

;; void h3dutPickRay( H3DNode cameraNode, float nwx, float nwy,
;;               float *ox, float *oy, float *oz, float *dx, float *dy, float *dz );
(defh3ufun ("h3dutPickRay" pick-ray) void
  "*      Calculates the ray originating at the specified camera and window coordinates

This utility function takes normalized window coordinates (ranging from 0 to 1
with the origin being the bottom left corner of the window) and returns ray
origin and direction for the given camera. The function is especially useful for
selecting objects by clicking on them.

Parameters:
        camera-node - camera used for picking
        nwx, nwy    - normalized window coordinates
        ox, oy, oz  - calculated ray origin
        dx, dy, dz  - calculated ray direction

Returns:
        nothing
"
  (camera-node node)
  (nwx float) (nwy float)
  (ox (:pointer float)) (oy (:pointer float)) (oz (:pointer float))
  (dx (:pointer float)) (dy (:pointer float)) (dz (:pointer float)))


;; H3DNode h3dutPickNode( H3DNode cameraNode, float nwx, float nwy );
(defh3ufun ("h3dutPickNode" pick-node) node
  "Returns the scene node which is at the specified window coordinates.

This utility function takes normalized window coordinates (ranging
from 0 to 1 with the origin being the bottom left corner of the
window) and returns the scene node which is visible at that
location. The function is especially useful for selecting objects by
clicking on them. Currently picking is only working for Meshes.

Parameters:
        camera-node - camera used for picking
        nwx, nwy   - normalized window coordinates

Returns:
        handle of picked node or 0 if no node was hit
"
  (camera-node node)
  (nwx float)
  (nwy float))

;;;; ---------------------------------------------------------------------------
;;;; * Group: Overlays

;; void h3dutShowText( const char *text, float x, float y, float size,
;;                    float colR, float colG, float colB,
;;                    H3DRes fontMaterialRes, int layer );

(defh3ufun ("h3dutShowText" show-text) void
  "Shows text on the screen using a font texture.

This utility function uses overlays to display a text string at a
specified position on the screen.  The font texture of the specified
font material has to be a regular 16x16 grid containing all ASCII
characters in row-major order. The layer corresponds to the layer
parameter of overlays.

Parameters:
        text                - text string to be displayed
        x, y                - position of the lower left corner of
                              the first character; for more details on coordinate
                              system see overlay documentation
        size                - size (scale) factor of the font
        col-r, col-g, col-b - font color
        font-material-res   - font material resource used for rendering
        layer               - layer index of the font overlays

Returns:
        nothing
"
  (text string)
  (x float)
  (y float)
  (size float)
  (col-r float)
  (col-g float)
  (col-b float)
  (font-material-resource resource)
  (layer int))

;; void h3dutShowFrameStats( H3DRes fontMaterialRes, H3DRes panelMaterialRes, int mode );
(defh3ufun ("h3dutShowFrameStats" show-frame-statistics) void
  "Shows frame statistics on the screen.

This utility function displays statistics for the current frame in the
upper left corner of the screen. Since the statistic counters are
reset after the call, it should be called exactly once per frame to
obtain correct values.

Parameters:
        font-material-resource  - font material resource used for drawing text
        panel-material-resource - material resource used for drawing info box
        mode                    - display mode, specifying which data is shown
                                  (<= +max-stat-mode+)

Returns:
        nothing
"
  (font-material-resource resource)
  (panel-material-resource resource)
  (mode int))

;;; bindings.lisp ends here
