;;; horde3d.lisp --- lispification for the horde3d wrapper
;;;  _                   _      _____     _ 
;;; | |__   ___  _ __ __| | ___|___ /  __| |
;;; | '_ \ / _ \| '__/ _` |/ _ \ |_ \ / _` |
;;; | | | | (_) | | | (_| |  __/___) | (_| |
;;; |_| |_|\___/|_|  \__,_|\___|____/ \__,_|
                                        
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :horde3d)

(defmacro import-export (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; Unintern first to avoid conflicts.
     (dolist (sym ',symbols)
       (let ((s (find-symbol (symbol-name sym))))
         (when s (unintern s))))
     ;; Import and re-export.
     (import ',symbols)
     (export ',symbols)))

(defmacro with-unique-names (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                          (stem (if (every #'alpha-char-p symbol-name)
                                    symbol-name
                                    (concatenate 'string symbol-name "-"))))
                     `(,symbol (gensym ,stem))))
                 symbols)
     ,@body))

(defmacro define-get-function (name (&rest head-args) &body clauses)
  (with-unique-names (return-type return-count p)
    `(defun ,name (,@head-args ,return-type &optional (,return-count 1))
       (with-foreign-object (,p ,return-type ,return-count)
         (ecase ,return-type
           ,@(loop for (func . types) in clauses
                collect `((,@types) (,func ,@head-args ,p))))
         (if (= ,return-count 1)
             (mem-aref ,p ,return-type 0)
             (loop for i from 0 to (1- ,return-count)
                collect (mem-aref ,p ,return-type i)))))))

;;;; Typedefs and constants

(import-export %h3d:+root-node+)

;;;; Enumerations

;;;; Basic functions

(import-export %h3d:get-version-string
               %h3d:check-extension
               %h3d:init
               %h3d:release
               %h3d:resize
               %h3d:render
               %h3d:clear)

(defun get-message ()
  (with-foreign-objects ((level :int)
                         (time :float))
    (values (h3d:get-message level time)
            (mem-ref level :int)
            (mem-ref time :float))))

(import-export %h3d:get-option
               %h3d:set-option)

(defun set-options (&rest plist
                    &key max-log-level
                    max-num-messages
                    trilinear-filtering
                    anisotropy-factor
                    tex-compression
                    load-textures
                    fast-animation
                    shadow-map-size
                    sample-count
                    wireframe-mode
                    debug-view-mode)
  (loop for (key . rest) on plist by #'cddr
     do (set-option key (car rest))))

(import-export %h3d:get-stat
               %h3d:show-overlay
               %h3d:clear-overlays)

;;;; General resource management functions

(import-export %h3d:get-resource-type
               %h3d:get-resource-name
               %h3d:find-resource
               %h3d:add-resource
               %h3d:clone-resource
               %h3d:remove-resource
               %h3d:is-resource-loaded
               %h3d:load-resource
               %h3d:unload-resource)

(defparameter param-types-alist
  '((:vertex-count . :int) 
    (:index-count . :int)
    (:vertex-data . :data)
    (:index-data . :data)

    (:frame-count . :int) 

    (:class . :string) 
    (:link . :int)
    (:shader . :int)
    (:tex-unit-0 . :int)
    (:tex-unit-1 . :int)
    (:tex-unit-2 . :int)
    (:tex-unit-3 . :int)
    (:tex-unit-4 . :int)
    (:tex-unit-5 . :int)
    (:tex-unit-6 . :int)
    (:tex-unit-7 . :int)
    (:tex-unit-8 . :int)
    (:tex-unit-9 . :int)
    (:tex-unit-10 . :int)
    (:tex-unit-11 . :int)

    (:pixel-data . :data)
    (:width . :int)
    (:height . :int)
    (:comps . :int)
    (:hdr . :int)
  
    (:life-min . :float)
    (:life-max . :float)
    (:move-vel-min . :float)
    (:move-vel-max . :float)
    (:move-vel-end-rate . :float)
    (:rot-vel-min . :float)
    (:rot-vel-max . :float)
    (:rot-vel-end-rate . :float)
    (:size-min . :float)
    (:size-max . :float)
    (:size-end-rate . :float)
    (:col-r-min . :float)
    (:col-r-max . :float)
    (:col-r-end-rate . :float)
    (:col-g-min . :float)
    (:col-g-max . :float)
    (:col-g-end-rate . :float)
    (:col-b-min . :float)
    (:col-b-max . :float)
    (:col-b-end-rate . :float)
    (:col-a-min . :float)
    (:col-a-max . :float)
    (:col-a-end-rate . :float)))

(defun resource-param-to-type (param)
  (second (assoc param param-types-alist)))

(defun get-resource-parameter (handle param)
  (let ((return-type (resource-param-to-type param)))
    (ecase return-type
      (:int
       (%h3d:get-resource-parami handle param))
      (:float
       (%h3d:get-resource-paramf handle param))
      (:string
       (%h3d:get-resource-paramstr handle param)))))


;;;; General scene graph functions

;;;; Group-specific scene graph functions 

;;;; Model-specific scene graph functions 

;;;; Mesh-specific scene graph functions 

;;;; Joint-specific scene graph functions

;;;; Light-specific scene graph functions 

;;;; Camera-specific scene graph functions

;;;; Emitter-specific scene graph functions

;;;; Resource management

(defun set-resource-paths (&rest plist
                           &key scene-graph
                           geometry
                           animation
                           material
                           code
                           shader
                           texture-2d
                           texture-cube
                           effect
                           pipeline)
  (loop for (key . rest) on plist by #'cddr
     do (set-resource-path key (car rest))))
  
;;;; Scene graph

;;;; Overlays

;;;; Terrain Extension

;;; horde3d.lisp ends here
