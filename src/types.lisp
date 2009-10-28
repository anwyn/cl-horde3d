;;; types.lisp --- cffi types for the horde3d lisp bindings
;;;  _                         
;;; | |_ _   _ _ __   ___  ___ 
;;; | __| | | | '_ \ / _ \/ __|
;;; | |_| |_| | |_) |  __/\__ \
;;;  \__|\__, | .__/ \___||___/
;;;      |___/|_|              
;;;
;;; Copyright (C) 2009 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :horde3d-cffi)

;;;; CFFI types

(define-foreign-type ensure-integer ()
  ()
  (:actual-type :int)
  (:simple-parser ensure-integer))

(defmethod translate-to-foreign (value (type ensure-integer))
  (truncate value))

(defmethod expand-to-foreign (value (type ensure-integer))
  (if (constantp value)
      (truncate (eval value))
      `(truncate ,value)))

(define-foreign-type ensure-float ()
  ()
  (:actual-type :float)
  (:simple-parser ensure-float))

(defmethod translate-to-foreign (value (type ensure-float))
  (cl:float value 1.0))

(defmethod expand-to-foreign (value (type ensure-float))
  (if (constantp value)
      (cl:float (eval value) 1.0)
      `(cl:float ,value 1.0)))

(define-foreign-type ensure-double ()
  ()
  (:actual-type :double)
  (:simple-parser ensure-double))

(defmethod translate-to-foreign (value (type ensure-double))
  (cl:float value 1.0d0))

(defmethod expand-to-foreign (value (type ensure-double))
  (if (constantp value)
      (cl:float (eval value) 1.0d0)
      `(cl:float ,value 1.0d0)))

;;;; C types

(defctype boolean (:boolean :unsigned-char))

(defctype int ensure-integer)
(defctype sizei ensure-integer)

(defctype void :void)
(defctype string :string)

(defctype float ensure-float)
(defctype double ensure-double)


;;;; Enums

(define-foreign-type multi-enum ()
  ((enums :accessor enums :initarg :enums :initform nil))
  (:actual-type :int)
  (:simple-parser multi-enum))

(defmethod translate-to-foreign (value (type multi-enum))
  (or (some (lambda (enum)
              (foreign-enum-value enum value :errorp nil)) (enums type))
      (error "~S is not defined as a value for multi enum type ~S."
               value type)))

(defmethod translate-from-foreign (value (type multi-enum))
  (or (some (lambda (enum)
              (foreign-enum-keyword enum value :errorp nil)) (enums type))
      (error "~S is not defined as a keyword for multi enum type ~S."
             value type)))

(defmethod expand-to-foreign (value (type multi-enum))
  (if (constantp value)
      (or (some (lambda (enum)
                  (foreign-enum-value enum (eval value) :errorp nil)) (enums type))
          (error "~S is not defined as a value for multi enum type ~S."
                 value type))
      `(or (some (lambda (enum)
                   (foreign-enum-value enum ,value :errorp nil)) ',(enums type))
           (error "~S is not defined as a value for multi enum type ~S."
                  ,value ,type))))

(defmethod expand-from-foreign (value (type multi-enum))
  (if (constantp value)
      (or (some (lambda (enum)
                  (foreign-enum-keyword enum (eval value) :errorp nil)) (enums type))
          (error "~S is not defined as a keyword for multi enum type ~S."
                 value type))
      `(or (some (lambda (enum)
                   (foreign-enum-keyword enum ,value :errorp nil)) ',(enums type))
           (error "~S is not defined as a keyword for multi enum type ~S."
                  ,value ,type))))

;;;; Group: Typedefs and constants

(defctype resource ensure-integer
  "Handle to a resource (int).")

(defctype node ensure-integer
  "Handle to a scene node (int).")

;;;; Macros


;;; Helper macro to define a horde3d API function and declare it inline.
(defmacro defh3fun ((cname lname) result-type &body body)
  `(progn
     (declaim (inline ,lname))
     (defcfun (,cname ,lname :library horde3d) ,result-type ,@body)))

;;; Helper macro to define a horde3d utils API function and declare it inline.
(defmacro defh3ufun ((cname lname) result-type &body body)
  `(progn
     (declaim (inline ,lname))
     (defcfun (,cname ,lname :library horde3d-utils) ,result-type ,@body)))


;;; types.lisp ends here
