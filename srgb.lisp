;;; srgb.lisp --- sRGB color space.

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

;;; Commentary:

;; See <http://www.w3.org/Graphics/Color/sRGB>
;; and <http://www.itu.int/rec/R-REC-BT.709/en>.

;;; Code:

(in-package :rs-colors)

(export 'srgb-color)
(defclass srgb-color (rgb-color-object)
  ()
  (:documentation "Color class for the sRGB color space."))

(export 'make-srgb-color)
(defun make-srgb-color (red green blue &key byte-size)
  "Create a new color in the sRGB color space.

First argument RED is the intensity of the red primary.
Second argument GREEN is the intensity of the green primary.
Third argument BLUE is the intensity of the blue primary.

Arguments RED, GREEN, and BLUE have to be normalized intensity values
in the closed interval [0, 1].

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  If specified, arguments RED, GREEN, and BLUE are scaled
accordingly.

Example:

     (make-srgb-color 252/255 175/255 62/255)
     (make-srgb-color 252 175 62 :byte-size 8)"
  (make-rgb-color 'srgb-color red green blue byte-size))

(export 'make-srgb-color-from-number)
(defun make-srgb-color-from-number (value &key (byte-size 8))
  "Create a new color in the sRGB color space.

Argument VALUE is a non-negative integral number.

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  Default is eight bit (one byte).  The most significant bits
denote the intensity of the red primary.

Example:

     (make-srgb-color-from-number #XFCAF3E)"
  (make-rgb-color-from-number 'srgb-color value byte-size))

;; ITU-R BT.709 truncates the CIE 1931 color space chromaticity
;; coordinates of the D65 standard illuminant to four decimal
;; places.
(defconst srgb-white-point (make-cie-xyy-color 3127/10000 3290/10000 1)
  "White point of the sRGB color space.")

(defmethod white-point ((color srgb-color))
  srgb-white-point)

(multiple-value-bind (rgb-from-xyz xyz-from-rgb)
    (rgb-transformation-matrices #(64/100 33/100)
				 #(30/100 60/100)
				 #(15/100  6/100)
				 (multiple-value-bind (x* y*)
				     (cie-xyy-color-coordinates srgb-white-point)
				   (vector x* y*)))
  (defconst srgb-from-cie-xyz-transformation-matrix (float-array rgb-from-xyz 1D0)
    "Transformation matrix to convert normalized CIE XYZ color space coordinates
into linear sRGB color space coordinates.")
  (defconst cie-xyz-from-srgb-transformation-matrix (float-array xyz-from-rgb 1D0)
    "Transformation matrix to convert linear sRGB color space coordinates
into normalized CIE XYZ color space coordinates.")
  (values))

(defun srgb-gamma-encoding (c)
  "Convert linear sRGB color space coordinates
into sRGB color space coordinates."
  (declare (type real c))
  (cond ((= c 0)
	 0)
	((= c 1)
	 1)
	((> c 0.0031308D0)
	 (- (* (expt (float c 1D0) 10/24) 1.055D0) 0.055D0))
	(t
	 (* c 12.92D0))))

(defun srgb-gamma-decoding (c)
  "Convert sRGB color space coordinates
into linear sRGB color space coordinates."
  (declare (type real c))
  (cond ((= c 0)
	 0)
	((= c 1)
	 1)
	((> c 0.04045D0)
	 (expt (/ (+ c 0.055D0) 1.055D0) 2.4D0))
	(t
	 (/ c 12.92D0))))

(defun srgb-from-cie-xyz (x y z)
  "Convert normalized CIE XYZ color space coordinates
into sRGB color space coordinates."
  (declare (type real x y z))
  (multiple-value-bind (r g b)
      (linear-transformation srgb-from-cie-xyz-transformation-matrix x y z)
    (declare (type real r g b))
    (values (srgb-gamma-encoding (clamp r 0 1))
	    (srgb-gamma-encoding (clamp g 0 1))
	    (srgb-gamma-encoding (clamp b 0 1)))))

(defun cie-xyz-from-srgb (r g b)
  "Convert sRGB color space coordinates
into normalized CIE XYZ color space coordinates."
  (declare (type real r g b))
  (linear-transformation cie-xyz-from-srgb-transformation-matrix
			 (srgb-gamma-decoding r)
			 (srgb-gamma-decoding g)
			 (srgb-gamma-decoding b)))

(export 'srgb-color-coordinates)
(defgeneric srgb-color-coordinates (color)
  (:documentation "Return the sRGB color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the red, green, and blue primary.")
  (:method ((color srgb-color))
    (color-coordinates color))
  (:method ((color generic-color-object))
    (generic-rgb-color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color-object))
    (multiple-value-call #'srgb-from-cie-xyz
      (cie-xyz-color-coordinates color))))

(defmethod generic-rgb-color-coordinates ((color srgb-color))
  (color-coordinates color))

(defmethod cie-xyz-color-coordinates ((color srgb-color))
  (multiple-value-call #'cie-xyz-from-srgb
    (color-coordinates color)))

(defmethod update-instance-for-different-class :after ((old color-object) (new srgb-color) &key)
  (with-slots (r g b) new
    (multiple-value-setq (r g b)
      (srgb-color-coordinates old))))

;;; srgb.lisp ends here
