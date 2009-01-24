;;; libraries.lisp --- library definitions
;;;  _ _ _                    _           
;;; | (_) |__  _ __ __ _ _ __(_) ___  ___ 
;;; | | | '_ \| '__/ _` | '__| |/ _ \/ __|
;;; | | | |_) | | | (_| | |  | |  __/\__ \
;;; |_|_|_.__/|_|  \__,_|_|  |_|\___||___/
                                      
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :horde3d)

(define-foreign-library horde3d
  (:darwin (:framework "Horde3D"))
  (:windows "Horde3D.dll" :calling-convention :stdcall)
  (:unix (:or "libHorde3D.so" "libHorde3D.so.1")))

(define-foreign-library horde3d-utils
  (:darwin (:framework "Horde3DUtils"))
  (:windows "Horde3DUtils.dll" :calling-convention :stdcall)
  (:unix (:or "libHorde3DUtils.so" "libHorde3DUtils.so.1")))

(use-foreign-library horde3d)
(use-foreign-library horde3d-utils)

;;; libraries.lisp ends here
