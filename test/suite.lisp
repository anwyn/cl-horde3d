;;; suite.lisp --- Main test suite for the horde3d wrappers
;;;            _ _
;;;  ___ _   _(_) |_ ___
;;; / __| | | | | __/ _ \
;;; \__ \ |_| | | ||  __/
;;; |___/\__,_|_|\__\___|

;;;
;;; Copyright (C) 2009 Ole Arndt <anwyn@sugarshark.com>
;;;

(defpackage :horde3d-test
  (:use :common-lisp :stefil :horde3d))

(in-package :horde3d-test)

(defsuite* horde3d-tests)


;;; suite.lisp ends here
