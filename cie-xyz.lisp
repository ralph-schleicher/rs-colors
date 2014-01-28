;;; cie-xyz.lisp --- CIE XYZ color space.

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

(export 'cie-xyz-color)
(defclass cie-xyz-color (color)
  ((x
    :initarg :x
    :initform 0
    :type (real 0)
    :documentation "First tristimulus value, default zero.")
   (y
    :initarg :y
    :initform 0
    :type (real 0)
    :documentation "Second tristimulus value, default zero.")
   (z
    :initarg :z
    :initform 0
    :type (real 0)
    :documentation "Third tristimulus value, default zero."))
  (:documentation "Color class for the CIE XYZ color space."))

(defmethod color-coordinates ((color cie-xyz-color))
  (with-slots (x y z) color
    (values x y z)))

(export 'make-cie-xyz-color)
(defun make-cie-xyz-color (x y z)
  "Create a new color in the CIE XYZ color space.

Arguments X, Y, and Z are the tristimulus values."
  (make-instance 'cie-xyz-color :x x :y y :z z))

(export 'cie-xyz-color-coordinates)
(defgeneric cie-xyz-color-coordinates (color)
  (:documentation "Return the CIE XYZ color space coordinates of the color.

Argument COLOR is a color object.

Values are the X, Y, and Z tristimulus values.")
  (:method ((color cie-xyz-color))
    (color-coordinates color)))

(export 'the-cie-xyz-color)
(defun the-cie-xyz-color (color)
  "Coerce the color into the CIE XYZ color space.

Argument COLOR is a color object.

If argument COLOR is already a color in the CIE XYZ color space,
return COLOR as is (no conversion).  Otherwise, return a new color
with the color coordinates of COLOR converted into the CIE XYZ color
space."
  (if (eq (type-of color) 'cie-xyz-color)
      color
    (multiple-value-call #'make-cie-xyz-color
      (cie-xyz-color-coordinates color))))

;;; cie-xyz.lisp ends here
