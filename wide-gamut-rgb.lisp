;;; wide-gamut-rgb.lisp --- wide-gamut RGB color space.

;; Copyright (C) 2016 Ralph Schleicher

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
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :rs-colors)

(defclass wide-gamut-rgb-color (rgb-color-object)
  ()
  (:documentation "Color class for the wide-gamut RGB color space."))

(defun make-wide-gamut-rgb-color (red green blue &key byte-size)
  "Create a new color in the wide-gamut RGB color space.

First argument RED is the intensity of the red primary.
Second argument GREEN is the intensity of the green primary.
Third argument BLUE is the intensity of the blue primary.

Arguments RED, GREEN, and BLUE have to be normalized intensity values
in the closed interval [0, 1].

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  If specified, arguments RED, GREEN, and BLUE are scaled
accordingly.

Example:

     (make-wide-gamut-rgb-color 252/255 175/255 62/255)
     (make-wide-gamut-rgb-color 252 175 62 :byte-size 8)"
  (make-rgb-color 'wide-gamut-rgb-color red green blue byte-size))

(defun make-wide-gamut-rgb-color-from-number (value &key (byte-size 8))
  "Create a new color in the wide-gamut RGB color space.

Argument VALUE is a non-negative integral number.

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  Default is eight bit (one byte).  The most significant bits
denote the intensity of the red primary.

Example:

     (make-wide-gamut-rgb-color-from-number #XFCAF3E)"
  (make-rgb-color-from-number 'wide-gamut-rgb-color value byte-size))

(defconst wide-gamut-rgb-white-point (make-ciexyy-color 3457/10000 3585/10000 1)
  "White point of the wide-gamut RGB color space.")

(defmethod white-point ((color wide-gamut-rgb-color))
  wide-gamut-rgb-white-point)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (multiple-value-bind (rgb-from-xyz xyz-from-rgb)
      (rgb-transformation-matrices #(7347/10000 2653/10000)
				   #(1152/10000 8264/10000)
				   #(1566/10000  177/10000)
				   (multiple-value-bind (x* y*)
				       (ciexyy-color-coordinates wide-gamut-rgb-white-point)
				     (vector x* y*)))
    (defconst wide-gamut-rgb-from-ciexyz-transformation-matrix (float-array rgb-from-xyz 1D0)
      "Transformation matrix to convert normalized CIE XYZ color space coordinates
into linear wide-gamut RGB color space coordinates.")
    (defconst ciexyz-from-wide-gamut-rgb-transformation-matrix (float-array xyz-from-rgb 1D0)
      "Transformation matrix to convert linear wide-gamut RGB color space coordinates
into normalized CIE XYZ color space coordinates.")
    (values)))

(defun wide-gamut-rgb-from-ciexyz (x y z)
  "Convert normalized CIE XYZ color space coordinates
into wide-gamut RGB color space coordinates."
  (declare (type real x y z))
  (multiple-value-bind (r g b)
      (linear-transformation wide-gamut-rgb-from-ciexyz-transformation-matrix x y z)
    (declare (type real r g b))
    (values (adobe-rgb-gamma-encoding (clamp r 0 1))
	    (adobe-rgb-gamma-encoding (clamp g 0 1))
	    (adobe-rgb-gamma-encoding (clamp b 0 1)))))

(defun ciexyz-from-wide-gamut-rgb (r g b)
  "Convert wide-gamut RGB color space coordinates
into normalized CIE XYZ color space coordinates."
  (declare (type real r g b))
  (linear-transformation ciexyz-from-wide-gamut-rgb-transformation-matrix
			 (adobe-rgb-gamma-decoding r)
			 (adobe-rgb-gamma-decoding g)
			 (adobe-rgb-gamma-decoding b)))

(defgeneric wide-gamut-rgb-color-coordinates (color)
  (:documentation "Return the wide-gamut RGB color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the red, green, and blue primary.")
  (:method ((color wide-gamut-rgb-color))
    (color-coordinates color))
  (:method ((color generic-color-object))
    (generic-rgb-color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color-object))
    (multiple-value-call #'wide-gamut-rgb-from-ciexyz
      (ciexyz-color-coordinates color))))

(defmethod generic-rgb-color-coordinates ((color wide-gamut-rgb-color))
  (color-coordinates color))

(defmethod ciexyz-color-coordinates ((color wide-gamut-rgb-color))
  (multiple-value-call #'ciexyz-from-wide-gamut-rgb (color-coordinates color)))

(defmethod update-instance-for-different-class :after ((old color-object) (new wide-gamut-rgb-color) &key)
  (with-slots (r g b) new
    (multiple-value-setq (r g b)
      (wide-gamut-rgb-color-coordinates old))))

;;; wide-gamut-rgb.lisp ends here
