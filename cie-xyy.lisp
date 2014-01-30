;;; cie-xyy.lisp --- CIE xyY color space.

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

(export 'cie-xyy-color)
(defclass cie-xyy-color (color)
  ((x*
    :initarg :x*
    :initform 0
    :type (real 0 1)
    :documentation "First chromaticity coordinate, default zero.")
   (y*
    :initarg :y*
    :initform 0
    :type (real 0 1)
    :documentation "Second chromaticity coordinate, default zero.")
   (y
    :initarg :y
    :initform 0
    :type (real 0)
    :documentation "Second tristimulus value, default zero."))
  (:documentation "Color class for the CIE xyY color space."))

(defmethod color-coordinates ((color cie-xyy-color))
  (with-slots (x* y* y) color
    (values x* y* y)))

(export 'make-cie-xyy-color)
(defun make-cie-xyy-color (x* y* y)
  "Create a new color in the CIE xyY color space.

Arguments X* and Y* are the chromaticity coordinates.
Argument Y is the second tristimulus value (luminance)."
  (make-instance 'cie-xyy-color :x* x* :y* y* :y y))

(defun cie-xyy-from-cie-xyz (x y z)
  "Convert CIE XYZ color space coordinates
into CIE xyY color space coordinates."
  (declare (type real x y z))
  (let ((s (+ x y z)))
    (declare (type real s))
    (when (zerop s)
      (error (make-condition 'division-by-zero
			     :operation 'cie-xyy-from-cie-xyz
			     :operands (list x y z))))
    (values (/ x s) (/ y s) y)))

(defun cie-xyz-from-cie-xyy (x* y* y)
  "Convert CIE xyY color space coordinates
into CIE XYZ color space coordinates."
  (declare (type real x* y* y))
  (when (zerop y*)
    (error (make-condition 'division-by-zero
			   :operation 'cie-xyz-from-cie-xyy
			   :operands (list x* y* y))))
  (let ((s (/ y y*)))
    (declare (type real s))
    (values (* x* s) y (* (- 1 x* y*) s))))

(export 'cie-xyy-color-coordinates)
(defgeneric cie-xyy-color-coordinates (color)
  (:documentation "Return the CIE xyY color space coordinates of the color.

Argument COLOR is a color object.

Values are the X and Y chromaticity coordinates and the Y tristimulus
value (luminance).")
  (:method ((color cie-xyy-color))
    (color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color))
    (multiple-value-call #'cie-xyy-from-cie-xyz
      (cie-xyz-color-coordinates color))))

(defmethod cie-xyz-color-coordinates ((color cie-xyy-color))
  (multiple-value-call #'cie-xyz-from-cie-xyy
    (color-coordinates color)))

(export 'the-cie-xyy-color)
(defun the-cie-xyy-color (color)
  "Coerce the color object into the CIE xyY color space.

Argument COLOR is a color object.

If argument COLOR is already a color in the CIE xyY color space,
return COLOR as is (no conversion).  Otherwise, return a new color
with the color coordinates of COLOR converted into the CIE xyY color
space."
  (if (eq (type-of color) 'cie-xyy-color)
      color
    (multiple-value-call #'make-cie-xyy-color
      (cie-xyy-color-coordinates color))))

;;; cie-xyy.lisp ends here
