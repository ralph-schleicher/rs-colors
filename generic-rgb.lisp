;;; generic-rgb.lisp --- generic RGB color space.

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

(export 'generic-rgb-color)
(defclass generic-rgb-color (rgb-color-object generic-color-object)
  ()
  (:documentation "Color class for the generic RGB color space.

The generic RGB color space is a mathematical description of the
RGB color model.  It is not associated with a particular device."))

(export 'make-generic-rgb-color)
(defun make-generic-rgb-color (red green blue &key byte-size)
  "Create a new color in the generic RGB color space.

First argument RED is the intensity of the red primary.
Second argument GREEN is the intensity of the green primary.
Third argument BLUE is the intensity of the blue primary.

Arguments RED, GREEN, and BLUE have to be normalized intensity values
in the closed interval [0, 1].

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  If specified, arguments RED, GREEN, and BLUE are scaled
accordingly.

Example:

     (make-generic-rgb-color 252/255 175/255 62/255)
     (make-generic-rgb-color 252 175 62 :byte-size 8)"
  (make-rgb-color 'generic-rgb-color red green blue byte-size))

(export 'make-generic-rgb-color-from-number)
(defun make-generic-rgb-color-from-number (value &key (byte-size 8))
  "Create a new color in the generic RGB color space.

Argument VALUE is a non-negative integral number.

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  Default is eight bit (one byte).  The most significant bits
denote the intensity of the red primary.

Example:

     (make-generic-rgb-color-from-number #XFCAF3E)"
  (make-rgb-color-from-number 'generic-rgb-color value byte-size))

(export 'generic-hsv-color)
(defclass generic-hsv-color (hsv-color-object generic-color-object)
  ()
  (:documentation "Color class for the generic HSV/HSB color space.

The generic HSV/HSB color space is a different representation of the
RGB color model."))

(export 'make-generic-hsv-color)
(defun make-generic-hsv-color (hue saturation value)
  "Create a new color in the generic HSV color space.

First argument HUE is the angle of the RGB color wheel.
Second argument SATURATION is the saturation.
Third argument VALUE is the brightness.

Arguments SATURATION and VALUE have to be real numbers
in the closed interval [0, 1]."
  (let ((h (mod (ensure-type hue 'real) 360))
	(s (ensure-type saturation '(real 0 1)))
	(v (ensure-type value '(real 0 1))))
    (make-instance 'generic-hsv-color :hue h :saturation s :value v)))

(export 'generic-hsl-color)
(defclass generic-hsl-color (hsl-color-object generic-color-object)
  ()
  (:documentation "Color class for the generic HSL color space.

The generic HSL color space is a different representation of the
RGB color model."))

(export 'make-generic-hsl-color)
(defun make-generic-hsl-color (hue saturation lightness)
  "Create a new color in the generic HSL color space.

First argument HUE is the angle of the RGB color wheel.
Second argument SATURATION is the saturation.
Third argument LIGHTNESS is the lightness.

Arguments SATURATION and LIGHTNESS have to be real numbers
in the closed interval [0, 1]."
  (let ((h (mod (ensure-type hue 'real) 360))
	(s (ensure-type saturation '(real 0 1)))
	(l (ensure-type lightness '(real 0 1))))
    (make-instance 'generic-hsl-color :hue h :saturation s :lightness l)))

;; HSV/HSB, HSL, or HSI from RGB.
(macrolet ((with-hue (bindings &body body)
	     `(let* ((max (max r g b))
		     (min (min r g b))
		     ;; Chroma.
		     (c (- max min))
		     ;; Hue.
		     (h (if (zerop c)
			    0
			  (mod (* 60 (cond ((= r max)
					    (+ 0 (/ (- g b) c)))
					   ((= g max)
					    (+ 2 (/ (- b r) c)))
					   ((= b max)
					    (+ 4 (/ (- r g) c)))
					   (t
					    (error "Should not happen."))))
			       360)))
		     ,@bindings)
		,@body)))
  (defun generic-hsv-from-generic-rgb (r g b)
    "Convert RGB color space coordinates
into HSV color space coordinates."
    (declare (type real r g b))
    (with-hue (;; Value (brightness).
	       (v max)
	       ;; Saturation.
	       (s (if (zerop c)
		      0
		    ;; If chroma is non-zero, value
		    ;; is non-zero, too.
		    (/ c v))))
      (values h s v)))
  (defun generic-hsl-from-generic-rgb (r g b)
    "Convert RGB color space coordinates
into HSL color space coordinates."
    (declare (type real r g b))
    (with-hue (;; Lightness.
	       (2l (+ min max))
	       (l (/ 2l 2))
	       ;; Saturation.
	       (s (if (zerop c)
		      0
		    ;; The denominator can also be
		    ;; expressed as 1 - |2L - 1|.
		    (/ c (if (> 2l 1) (- 2 2l) 2l)))))
      (values h s l)))
  (defun generic-hsi-from-generic-rgb (r g b)
    "Convert RGB color space coordinates
into HSI color space coordinates."
    (declare (type real r g b))
    (with-hue (;; Intensity.
	       (i (/ (+ r g b) 3))
	       ;; Saturation.
	       (s (if (zerop c)
		      0
		    ;; If chroma is non-zero, intensity
		    ;; is non-zero, too.
		    (- 1 (/ min i)))))
      (values h s i)))
  (values))

;; RGB from HSV/HSB or HSL.
(macrolet ((with-chroma (chroma value)
	     `(let* ((c ,chroma)
		     ;; Extrema.
		     (max ,value)
		     (min (- max c)))
		(multiple-value-bind (q r)
		    (truncate (/ h 60))
		  (let ((x (- max (* c (if (oddp q) r (- 1 r))))))
		    (cond ((= q 0) (values max x min))
			  ((= q 1) (values x max min))
			  ((= q 2) (values min max x))
			  ((= q 3) (values min x max))
			  ((= q 4) (values x min max))
			  ((= q 5) (values max min x))
			  (t
			   (error "Should not happen."))))))))
  (defun generic-rgb-from-generic-hsv (h s v)
    "Convert HSV color space coordinates
into RGB color space coordinates."
    (declare (type real h s v))
    (if (zerop s)
	(values v v v)
      (with-chroma (* s v)
	v)))
  (defun generic-rgb-from-generic-hsl (h s l)
    "Convert HSL color space coordinates
into RGB color space coordinates."
    (declare (type real h s l))
    (if (zerop s)
	(values l l l)
      (let ((2l (* 2 l)))
	(declare (type real 2l))
	(with-chroma (* s (if (> 2l 1) (- 2 2l) 2l))
	  (+ l (/ c 2))))))
  (values))

(export 'generic-rgb-color-coordinates)
(defgeneric generic-rgb-color-coordinates (color)
  (:documentation "Return the RGB color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the red, green, and blue primary.")
  (:method ((color generic-rgb-color))
    (color-coordinates color))
  (:method ((color generic-hsv-color))
    (multiple-value-call #'generic-rgb-from-generic-hsv
      (color-coordinates color)))
  (:method ((color generic-hsl-color))
    (multiple-value-call #'generic-rgb-from-generic-hsl
      (color-coordinates color))))

(export 'generic-hsv-color-coordinates)
(defgeneric generic-hsv-color-coordinates (color)
  (:documentation "Return the HSV color space coordinates of the color.

Argument COLOR is a color object.

Values are the hue, saturation, and value (brightness).")
  (:method ((color generic-hsv-color))
    (color-coordinates color))
  (:method ((color color-object))
    (multiple-value-call #'generic-hsv-from-generic-rgb
      (generic-rgb-color-coordinates color))))

(export 'generic-hsl-color-coordinates)
(defgeneric generic-hsl-color-coordinates (color)
  (:documentation "Return the HSL color space coordinates of the color.

Argument COLOR is a color object.

Values are the hue, saturation, and lightness.")
  (:method ((color generic-hsl-color))
    (color-coordinates color))
  (:method ((color color-object))
    (multiple-value-call #'generic-hsl-from-generic-rgb
      (generic-rgb-color-coordinates color))))

(defmethod update-instance-for-different-class :after ((old color-object) (new generic-rgb-color) &key)
  (with-slots (r g b) new
    (multiple-value-setq (r g b)
      (generic-rgb-color-coordinates old))))

(defmethod update-instance-for-different-class :after ((old color-object) (new generic-hsv-color) &key)
  (with-slots (h s v) new
    (multiple-value-setq (h s v)
      (generic-hsv-color-coordinates old))))

(defmethod update-instance-for-different-class :after ((old color-object) (new generic-hsl-color) &key)
  (with-slots (h s l) new
    (multiple-value-setq (h s l)
      (generic-hsl-color-coordinates old))))

;;; generic-rgb.lisp ends here
