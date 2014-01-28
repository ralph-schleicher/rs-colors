;;; types.lisp --- basic data types.

;; Copyright (C) 2014 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :rs-colors)

;;;; Color Data Type

(export 'color)
(defclass color ()
  ()
  (:documentation "Base class for a color."))

(export 'colorp)
(defun colorp (object)
  "Return true if OBJECT is a color."
  (typep object 'color))

(export 'color-coordinates)
(defgeneric color-coordinates (color)
  (:documentation "Return the color space coordinates of the color.

Argument COLOR is a color object.")
  (:method ((color color))
    (declare (ignore color))
    (values)))

(defmethod print-object ((color color) stream)
  (print-unreadable-object (color stream :type t :identity t)
    (princ (multiple-value-list (color-coordinates color)) stream)))

;;;; RGB Color Data Type

(export 'rgb-color)
(defclass rgb-color (color)
  ((r
    :initarg :red
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the red primary, default zero.")
   (g
    :initarg :green
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the green primary, default zero.")
   (b
    :initarg :blue
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the blue primary, default zero."))
  (:documentation "Color class for a RGB color space."))

(defmethod color-coordinates ((color rgb-color))
  (with-slots (r g b) color
    (values r g b)))

;;; types.lisp ends here
