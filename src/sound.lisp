;;; sound.lisp --- bindings to the horde3d sound extension
;;;                            _ 
;;;  ___  ___  _   _ _ __   __| |
;;; / __|/ _ \| | | | '_ \ / _` |
;;; \__ \ (_) | |_| | | | | (_| |
;;; |___/\___/ \__,_|_| |_|\__,_|
                             
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :horde3d-cffi-binding)

(defcenum sound-resource-types
    "The available resource types.

 :sound        - A sound resource
"
  (:sound 200))

(defcenum sound-node-types
    "The available scene node types.

   :listener        - Listener node
   :sound           - Sound node
"
  (:listener 201)
  (:sound 202))


(defcenum listener-node-parameters
    "The available Listener node parameters.

 :listener-master-gain    - Amplitude multiplier (volume), this affects all sounds (default: 1.0) [type: float]
 :listener-doppler-factor - *currently has no effect* Exaggeration factor for doppler effect (default: 1.0) [type: float]
 :listener-speed-of-sound - *currently has no effect* Speed of sound in same units as velocities (default: 343.3) [type: float]
"
  (:listener-master-gain 20000)
  :listener-doppler-factor
  :listener-speed-of-sound)

(defcenum sound-node-parameters
    "The available Sound node parameters.

   :sound-resource          - The resource used for audio playback [type: resource]
   :sound-gain               - Amplitude multiplier (volume) (default: 1.0) [type: float]
   :sound-pitch              - Pitch shift (value range: 0.5-2.0; default: 1.0) [type: float]
   :sound-offset             - Position of the sound's playback in seconds [type: float]
   :sound-loop               - Determines if the sound should loop after it reaches the end (Values: 0, 1) [type: int]
   :sound-max-distance       - This is used for distance based attenuation calculations, for explanation on how this
                               exactly is used in each distance model see the DistanceModels section. This will also clamp
                               distances greater than this value if the InverseDistanceClamped, LinearDistanceClamped or
                               ExponentDistanceClamped distance model is used. (default: MAX_FLOAT) [type: float]
   :sound-rolloff-factor     - This is used for distance based attenuation calculations, for explanation on how this
                               exactly is used in each distance model see the DistanceModels section. (default: 1.0) [type: float]
   :sound-reference-distance - This is used for distance based attenuation calculations, for explanation on how this
                               exactly is used in each distance model see the DistanceModels section. This will also clamp
                               distances below this value if the InverseDistanceClamped, LinearDistanceClamped or
                               ExponentDistanceClamped distance model is used. (default: 1.0) [type: float]
   :sound-min-gain           - This indicates the minimal gain guaranteed for the sound. At the end of processing of various attenuation
                               factors such as distance based attenuation and the sound's Gain value, the effective gain calculated is
                               compared to this value. If the effective gain is lower than this value, this value is applied. This happens
                               before the listener's MasterGain is applied. (value range: 0.0-1.0; default: 0.0) [type: float]
   :sound-max-gain           - This indicates the maximal gain permitted for the sound. At the end of processing of various attenuation
                               factors such as distance based attenuation and the sound's Gain value, the effective gain calculated is
                               compared to this value. If the effective gain is higher than this value, this value is applied. This happens
                               before the listener's MasterGain is applied. (value range: 0.0-1.0; default: 1.0) [type: float]
"
  (:sound-resource 21000)
   :sound-gain
   :sound-pitch
   :sound-offset
   :sound-loop
   :sound-max-distance
   :sound-rolloff-factor
   :sound-reference-distance
   :sound-min-gain
   :sound-max-gain)

(defcenum distance-model
  "The available distance models.

The variables used in the calculations explained::
  - Distance = the distance between the listener and sound node
  - Gain = the sound's volume after distance based attenuation
  - ReferenceDistance, RolloffFactor and MaxDistance = sound node parameters

  :none                      - No distance based attenuation will be applied.
  :inverse-distance          - Distance based attenuation will be calculated using the following formula:
                               Gain = ReferenceDistance / (ReferenceDistance + RolloffFactor * (Distance-ReferenceDistance));
                               ReferenceDistance indicates the distance which the listener will experience the sound's Gain value.
                               RolloffFactor can used to increase or decrease the range of a sound by increasing or decreasing the attenuation, respectivly.
  :inverse-distance-clamped  - This is the same as InverseDistance but it clamps the Distance before calculating the effective Gain:
                               Distance = max(Distance, ReferenceDistance);
                               Distance = min(Distance, MaxDistance);
                               (default distance model)
  :linear-distance           - This models a linear drop-off in gain as distance increases between the sound and listener.
                               Distance based attenuation will be calculated using the following formula:
                               Gain = (1-RolloffFactor * (Distance-ReferenceDistance) / (MaxDistance-ReferenceDistance));
  :linear-distance-clamped   - This is the same as LinearDistance but it clamps the Distance before calculating the effective Gain:
                               Distance = max(Distance, ReferenceDistance);
                               Distance = min(Distance, MaxDistance);
  :exponent-distance         - This models an exponential drop-off in gain as distance increases between the sound and listener.
                               Distance based attenuation will be calculated using the following formula:
                               Gain = (Distance / ReferenceDistance) ^ (-RolloffFactor);
                               where the ^ operation raises it's first operand to the power of it's second operand.
  :exponent-distance-clamped - This is the same as ExponentDistance but it clamps the Distance before calculating the effective Gain:
                               Distance = max(Distance, ReferenceDistance);
                               Distance = min(Distance, MaxDistance);
"
  :none
  :inverse-distance
  :inverse-distance-clamped
  :linear-distance
  :linear-distance-clamped
  :exponent-distance
  :exponent-distance-clamped)


;;;; Group: Sound Extension

;;; Basic functions 

(defh3fun ("h3dOpenDevice" open-device) boolean
  "Opens a sound device for playback.

This function opens and initializes a sound device for playback. This needs to be called before
any sound resources or nodes can be created or used. It will fail if another device is already
open.

Parameters:
        device  - name of the device to open (use NULL for default device)

Returns:
        true in case of success, otherwise false
"
  (device string))


(defh3fun ("h3dCloseDevice" close-device) void
  "Closes the currently open sound device.

This function closes the currently open sound device.

Parameters:
        none

Returns:
        nothing
")


(defh3fun ("h3dGetOpenDevice" get-open-device) string
  "Gets the name of the currently open sound device.

This function returns the name of the currently open sound device.

Parameters:
        none

Returns:
        name of the open device or NULL is none is open.
")


(defh3fun ("h3dQueryDevice" query-device) string
  "Returns the name of a sound device.

This function returns the name of a sound device from an internal list. If the index specified
is greater than the number of the available sound devices, NULL is returned.

Parameters:
        index   - index of sound device within the internal list (starting with 0)

Returns:
        name of a sound device or NULL
"
          (index int))


(defh3fun ("h3dGetDistanceModel" get-distance-model) distance-model
  "Gets the active distance model.

This function return the distance model used for calculating distance based attenuation.

Parameters:
        none

Returns:
        currently active distance model
")


(defh3fun ("h3dSetDistanceModel" set-distance-model) boolean
  "Sets the active distance model.

This function sets the distance model used for calculating distance based attenuation.

Parameters:
        model   - distance model to use

Returns:
        true if the distance model could be set, otherwise false
"
  (model distance-model))

;;;; Group: Listener-specific scene graph functions 


;;; NodeHandle addListenerNode( NodeHandle parent, const char *name );
(defh3fun ("h3dAddListenerNode" add-listener-node) node
  "Adds a Listener node to the scene.

This function creates a new Listener node and attaches it to the specified parent node.

Parameters:
        parent  - handle to parent node to which the new node will be attached
        name    - name of the node

Returns:
         handle to the created node or 0 in case of failure
"
  (parent node) (name string))


;;; NodeHandle getActiveListener();
(defh3fun ("h3dGetActiveListener" get-active-listener) node
  "Returns the handle of the active Listener node.

This function returns the handle of the currently active Listener node.

Parameters:
        none

Returns:
        handle to active Listener node or 0 if there is no active Listener node
")


;;; bool setActiveListener( NodeHandle listenerNode );
(defh3fun ("h3dSetActiveListener" set-active-listener) boolean
  "Sets the active Listener node.

This function sets the currently active Listener node. This node will act as the
ears and all 3D sound calculations will be based on this node's position and orientation.

Parameters:
        listener-node   - handle to the Listener node.

Returns:
        true in case of success, otherwise false
"
  (listener-node node))


;;;; Group: Sound-specific scene graph functions 

;;; NodeHandle addSoundNode( NodeHandle parent, const char *name, ResHandle soundRes );
(defh3fun ("h3dAddSoundNode" add-sound-node) node
  "Adds a Sound node to the scene.

This function creates a new Sound node and attaches it to the specified parent node.

Parameters:
        parent          - handle to parent node to which the new node will be attached
        name            - name of the node
        sound-resource  - handle to Sound resource which will be used for playback

Returns:
        handle to the created node or 0 in case of failure
"
  (parent node) (name string) (sound-resource resource))


;;; bool isSoundPlaying( NodeHandle soundNode );
(defh3fun ("h3dIsSoundPlaying" sound-playing-p) boolean
  "Checks if an Sound node is playing.

This function returns whether the Sound node is currently playing or not.

Parameters:
        sound-node - handle to the Sound node

Returns:
        true if the Sound node is currently playing, otherwise false
"
  (sound-node node))


;;; void playSound( NodeHandle soundNode );
(defh3fun ("h3dPlaySound" play-sound) void
  "Starts the audio playback of a Sound node.

This function will start the audio playback of a Sound node.

Parameters:
        sound-node - handle to the Sound node

Returns:
        nothing
"
  (sound-node node))


;;; void pauseSound( NodeHandle soundNode );
(defh3fun ("h3dPauseSound" pause-sound) void
  "Pauses the playback of a Sound node.

This function will pause the playback of a Sound node.

Parameters:
        sound-node - handle to the Sound node

Returns:
        nothing
"
  (sound-node node))


;;; void rewindSound( NodeHandle soundNode );
(defh3fun ("h3dRewindSound" rewind-sound) void
  "Rewinds the playback of a Sound node.

This function will rewind the playback of a Sound node. If the Sound node is
playing while being rewinded it will continue to play from it's rewinded position.

Parameters:
        sound-node - handle to the Sound node

Returns:
        nothing
"
  (sound-node node))

;;; sound.lisp ends here
