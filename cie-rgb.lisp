;;; cie-rgb.lisp --- CIE RGB color space.

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

(export 'cie-rgb-color)
(defclass cie-rgb-color (rgb-color)
  ()
  (:documentation "Color class for the CIE RGB color space."))

(export 'make-cie-rgb-color)
(defun make-cie-rgb-color (red green blue)
  "Create a new color in the CIE RGB color space.

First argument RED is the intensity of the red primary.
Second argument GREEN is the intensity green primary.
Third argument BLUE is the intensity of the blue primary.

Arguments RED, GREEN, and BLUE have to be normalized intensity values
in the closed interval [0, 1]."
  (make-instance 'cie-rgb-color :red red :green green :blue blue))

(let ((c (make-matrix 49000/100000 31000/100000 20000/100000
		      17697/100000 81240/100000  1063/100000
		          0/100000  1000/100000 99000/100000)))
  (defconst cie-rgb-from-cie-xyz-transformation-matrix (matrix-inverse (copy-matrix c))
    "Transformation matrix to convert CIE XYZ color space coordinates
into CIE RGB color space coordinates.")
  (defconst cie-xyz-from-cie-rgb-transformation-matrix c
    "Transformation matrix to convert CIE RGB color space coordinates
into CIE XYZ color space coordinates.")
  (values))

(defun cie-rgb-from-cie-xyz (x y z)
  "Convert CIE XYZ color space coordinates
into CIE RGB color space coordinates."
  (declare (type real x y z))
  (multiple-value-bind (r g b)
      (linear-transformation cie-rgb-from-cie-xyz-transformation-matrix x y z)
    (values (alexandria:clamp r 0 1)
	    (alexandria:clamp g 0 1)
	    (alexandria:clamp b 0 1))))

(defun cie-xyz-from-cie-rgb (r g b)
  "Convert CIE RGB color space coordinates
into CIE XYZ color space coordinates."
  (declare (type real r g b))
  (linear-transformation cie-xyz-from-cie-rgb-transformation-matrix r g b))

(export 'cie-rgb-color-coordinates)
(defgeneric cie-rgb-color-coordinates (color)
  (:documentation "Return the CIE RGB color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the red, green, and blue primary.")
  (:method ((color cie-rgb-color))
    (color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color))
    (multiple-value-call #'cie-rgb-from-cie-xyz
      (cie-xyz-color-coordinates color))))

(defmethod cie-xyz-color-coordinates ((color cie-rgb-color))
  (multiple-value-call #'cie-xyz-from-cie-rgb
    (color-coordinates color)))

(export 'the-cie-rgb-color)
(defun the-cie-rgb-color (color)
  "Coerce the color object into the CIE RGB color space.

Argument COLOR is a color object.

If argument COLOR is already a color in the CIE RGB color space,
return COLOR as is (no conversion).  Otherwise, return a new color
with the color coordinates of COLOR converted into the CIE RGB color
space."
  (if (eq (type-of color) 'cie-rgb-color)
      color
    (multiple-value-call #'make-cie-rgb-color
      (cie-rgb-color-coordinates color))))

(defmethod update-instance-for-different-class :after ((old color) (new cie-rgb-color) &key)
  (with-slots (r g b) new
    (multiple-value-setq (r g b)
      (cie-rgb-color-coordinates old))))

;;; cie-rgb.lisp ends here
