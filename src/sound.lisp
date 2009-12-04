;;; sound.lisp --- bindings to the horde3d sound extension
;;;                            _ 
;;;  ___  ___  _   _ _ __   __| |
;;; / __|/ _ \| | | | '_ \ / _` |
;;; \__ \ (_) | |_| | | | | (_| |
;;; |___/\___/ \__,_|_| |_|\__,_|
                             
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 


;;; Make symbols available in the horde3d package

(in-package :horde3d)

(import-export %h3d-sound:open-device
               %h3d-sound:close-device
               %h3d-sound:get-open-device
               %h3d-sound:query-device
               %h3d-sound:get-distance-model
               %h3d-sound:set-distance-model
               %h3d-sound:add-listener-node
               %h3d-sound:get-active-listener
               %h3d-sound:set-active-listener
               %h3d-sound:add-sound-node
               %h3d-sound:sound-playing-p
               %h3d-sound:play-sound
               %h3d-sound:pause-sound
               %h3d-sound:rewind-sound)

;;; sound.lisp ends here
