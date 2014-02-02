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

;;; Commentary:

;; See <http://www.w3.org/Graphics/Color/sRGB>.

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
  (let (r g b)
    (if (not byte-size)
	(setf r (ensure-type red '(real 0 1))
	      g (ensure-type green '(real 0 1))
	      b (ensure-type blue '(real 0 1)))
      (let ((s (1- (expt 2 (ensure-type byte-size '(integer 1))))))
	(setf r (/ (ensure-type red `(integer 0 ,s)) s)
	      g (/ (ensure-type green `(integer 0 ,s)) s)
	      b (/ (ensure-type blue `(integer 0 ,s)) s))))
    (make-instance 'srgb-color :red r :green g :blue b)))

(export 'make-srgb-color-from-number)
(defun make-srgb-color-from-number (value &key (byte-size 8))
  (ensure-type value '(integer 0))
  (ensure-type byte-size '(integer 1))
  (multiple-value-bind (r g b)
      (decode-triple value byte-size)
    (make-srgb-color r g b :byte-size byte-size)))

(multiple-value-bind (rgb-from-xyz xyz-from-rgb)
    (rgb-transformation-matrices #(64/100 33/100)
				 #(30/100 60/100)
				 #(15/100  6/100)
				 ;; ITU-R BT.709 truncates the CIE 1931
				 ;; color space chromaticity coordinates
				 ;; of the D65 standard illuminant to
				 ;; four decimal places.
				 #(3127/10000 3290/10000))
  (defconst srgb-from-cie-xyz-transformation-matrix (float-array rgb-from-xyz 1D0)
    "Transformation matrix to convert CIE XYZ color space coordinates
into linear sRGB color space coordinates.")
  (defconst cie-xyz-from-srgb-transformation-matrix (float-array xyz-from-rgb 1D0)
    "Transformation matrix to convert linear sRGB color space coordinates
into CIE XYZ color space coordinates.")
  (values))

(defun srgb-from-cie-xyz-gamma-correction (c)
  "Convert linear sRGB color space coordinates
into sRGB color space coordinates."
  (declare (type real c))
  (if (> c 0.00304D0)
      (- (* (expt c 10/24) 1.055D0) 0.055D0)
    (* c 12.92D0)))

(defun cie-xyz-from-srgb-gamma-correction (c)
  "Convert sRGB color space coordinates
into linear sRGB color space coordinates."
  (declare (type real c))
  (if (> c 0.003928D0)
      (expt (/ (+ c 0.055D0) 1.055D0) 24/10)
    (/ c 12.92D0)))

(defun srgb-from-cie-xyz (x y z)
  "Convert CIE XYZ color space coordinates
into sRGB color space coordinates."
  (multiple-value-bind (r g b)
      (linear-transformation srgb-from-cie-xyz-transformation-matrix x y z)
    (values (srgb-from-cie-xyz-gamma-correction (alexandria:clamp r 0 1))
	    (srgb-from-cie-xyz-gamma-correction (alexandria:clamp g 0 1))
	    (srgb-from-cie-xyz-gamma-correction (alexandria:clamp b 0 1)))))

(defun cie-xyz-from-srgb (r g b)
  "Convert sRGB color space coordinates
into CIE XYZ color space coordinates."
  (linear-transformation cie-xyz-from-srgb-transformation-matrix
			 (cie-xyz-from-srgb-gamma-correction r)
			 (cie-xyz-from-srgb-gamma-correction g)
			 (cie-xyz-from-srgb-gamma-correction b)))

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

(export 'the-srgb-color)
(defun the-srgb-color (color)
  "Coerce the color object into the sRGB color space.

Argument COLOR is a color object.

If argument COLOR is already a color in the sRGB color space,
return COLOR as is (no conversion).  Otherwise, return a new color
with the color coordinates of COLOR converted into the sRGB color
space."
  (if (eq (type-of color) 'srgb-color)
      color
    (multiple-value-call #'make-srgb-color
      (srgb-color-coordinates color))))

(defmethod update-instance-for-different-class :after ((old color-object) (new srgb-color) &key)
  (with-slots (r g b) new
    (multiple-value-setq (r g b)
      (srgb-color-coordinates old))))

;;; srgb.lisp ends here
