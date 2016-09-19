;;; adobe-rgb.lisp --- Adobe RGB color space.

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

;;; Code:

(in-package :rs-colors)

(export 'adobe-rgb-color)
(defclass adobe-rgb-color (rgb-color-object)
  ()
  (:documentation "Color class for the Adobe RGB color space."))

(export 'make-adobe-rgb-color)
(defun make-adobe-rgb-color (red green blue &key byte-size)
  "Create a new color in the Adobe RGB color space.

First argument RED is the intensity of the red primary.
Second argument GREEN is the intensity of the green primary.
Third argument BLUE is the intensity of the blue primary.

Arguments RED, GREEN, and BLUE have to be normalized intensity values
in the closed interval [0, 1].

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  If specified, arguments RED, GREEN, and BLUE are scaled
accordingly.

Example:

     (make-adobe-rgb-color 252/255 175/255 62/255)
     (make-adobe-rgb-color 252 175 62 :byte-size 8)"
  (make-rgb-color 'adobe-rgb-color red green blue byte-size))

(export 'make-adobe-rgb-color-from-number)
(defun make-adobe-rgb-color-from-number (value &key (byte-size 8))
  "Create a new color in the Adobe RGB color space.

Argument VALUE is a non-negative integral number.

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  Default is eight bit (one byte).  The most significant bits
denote the intensity of the red primary.

Example:

     (make-adobe-rgb-color-from-number #XFCAF3E)"
  (make-rgb-color-from-number 'adobe-rgb-color value byte-size))

(defconst adobe-rgb-white-point (make-cie-xyy-color 3127/10000 3290/10000 1)
  "White point of the Adobe RGB color space.")

(defmethod white-point ((color adobe-rgb-color))
  adobe-rgb-white-point)

(multiple-value-bind (rgb-from-xyz xyz-from-rgb)
    (rgb-transformation-matrices #(64/100 33/100)
				 #(21/100 71/100)
				 #(15/100  6/100)
				 (multiple-value-bind (x* y*)
				     (cie-xyy-color-coordinates adobe-rgb-white-point)
				   (vector x* y*)))
  (defconst adobe-rgb-from-cie-xyz-transformation-matrix (float-array rgb-from-xyz)
    "Transformation matrix to convert normalized CIE XYZ color space coordinates
into linear Adobe RGB color space coordinates.")
  (defconst cie-xyz-from-adobe-rgb-transformation-matrix (float-array xyz-from-rgb)
    "Transformation matrix to convert linear Adobe RGB color space coordinates
into normalized CIE XYZ color space coordinates.")
  (values))

;; §4.3.1.2 The Inverse Color Component Transfer Function
;;
;; The value 2.19921875 is obtained from 2 51/256 or
;; hexadecimal 02.33.
(defun adobe-rgb-gamma-encoding (c)
  "Convert linear Adobe RGB color space coordinates
into Adobe RGB color space coordinates."
  (declare (type real c))
  (cond ((= c 0)
	 0)
	((= c 1)
	 1)
	(t
	 (expt c 563/256))))

(defun adobe-rgb-gamma-decoding (c)
  "Convert Adobe RGB color space coordinates
into linear Adobe RGB color space coordinates."
  (declare (type real c))
  (cond ((= c 0)
	 0)
	((= c 1)
	 1)
	(t
	 (expt c 256/563))))

(defun adobe-rgb-from-cie-xyz (x y z)
  "Convert normalized CIE XYZ color space coordinates
into Adobe RGB color space coordinates."
  (declare (type real x y z))
  (multiple-value-bind (r g b)
      (linear-transformation adobe-rgb-from-cie-xyz-transformation-matrix x y z)
    (declare (type real r g b))
    (values (adobe-rgb-gamma-encoding (clamp r 0 1))
	    (adobe-rgb-gamma-encoding (clamp g 0 1))
	    (adobe-rgb-gamma-encoding (clamp b 0 1)))))

(defun cie-xyz-from-adobe-rgb (r g b)
  "Convert Adobe RGB color space coordinates
into normalized CIE XYZ color space coordinates."
  (declare (type real r g b))
  (linear-transformation cie-xyz-from-adobe-rgb-transformation-matrix
			 (adobe-rgb-gamma-decoding r)
			 (adobe-rgb-gamma-decoding g)
			 (adobe-rgb-gamma-decoding b)))

(export 'adobe-rgb-color-coordinates)
(defgeneric adobe-rgb-color-coordinates (color)
  (:documentation "Return the Adobe RGB color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the red, green, and blue primary.")
  (:method ((color adobe-rgb-color))
    (color-coordinates color))
  (:method ((color generic-color-object))
    (generic-rgb-color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color-object))
    (multiple-value-call #'adobe-rgb-from-cie-xyz
      (cie-xyz-color-coordinates color))))

(defmethod generic-rgb-color-coordinates ((color adobe-rgb-color))
  (color-coordinates color))

(defmethod cie-xyz-color-coordinates ((color adobe-rgb-color))
  (multiple-value-call #'cie-xyz-from-adobe-rgb (color-coordinates color)))

(defmethod update-instance-for-different-class :after ((old color-object) (new adobe-rgb-color) &key)
  (with-slots (r g b) new
    (multiple-value-setq (r g b)
      (adobe-rgb-color-coordinates old))))

;;; adobe-rgb.lisp ends here
