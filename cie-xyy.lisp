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
(defclass cie-xyy-color (color-object)
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
  (:method ((color color-object))
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

(defmethod update-instance-for-different-class :after ((old color-object) (new cie-xyy-color) &key)
  (with-slots (x* y* y) new
    (multiple-value-setq (x* y* y)
      (cie-xyy-color-coordinates old))))

;;;; White Points of CIE Standard Illuminants

;; See <http://en.wikipedia.org/wiki/Standard_illuminant>.
(macrolet ((define-white-point (name (x y) &optional doc)
	     `(defconst ,name (make-cie-xyy-color ,x ,y 1)
		,@(when doc (list doc)))))
  (define-white-point CIE-1931-A   (0.44757 0.40745))
  (define-white-point CIE-1931-B   (0.34842 0.35161))
  (define-white-point CIE-1931-C   (0.31006 0.31616))
  (define-white-point CIE-1931-D50 (0.34567 0.35850))
  (define-white-point CIE-1931-D55 (0.33242 0.34743))
  (define-white-point CIE-1931-D65 (0.31271 0.32902))
  (define-white-point CIE-1931-D75 (0.29902 0.31485))
  (define-white-point CIE-1931-E   (1/3     1/3    ))
  (define-white-point CIE-1931-F1  (0.31310 0.33727))
  (define-white-point CIE-1931-F2  (0.37208 0.37529))
  (define-white-point CIE-1931-F3  (0.40910 0.39430))
  (define-white-point CIE-1931-F4  (0.44018 0.40329))
  (define-white-point CIE-1931-F5  (0.31379 0.34531))
  (define-white-point CIE-1931-F6  (0.37790 0.38835))
  (define-white-point CIE-1931-F7  (0.31292 0.32933))
  (define-white-point CIE-1931-F8  (0.34588 0.35875))
  (define-white-point CIE-1931-F9  (0.37417 0.37281))
  (define-white-point CIE-1931-F10 (0.34609 0.35986))
  (define-white-point CIE-1931-F11 (0.38052 0.37713))
  (define-white-point CIE-1931-F12 (0.43695 0.40441))
  (define-white-point CIE-1964-A   (0.45117 0.40594))
  (define-white-point CIE-1964-B   (0.34980 0.35270))
  (define-white-point CIE-1964-C   (0.31039 0.31905))
  (define-white-point CIE-1964-D50 (0.34773 0.35952))
  (define-white-point CIE-1964-D55 (0.33411 0.34877))
  (define-white-point CIE-1964-D65 (0.31382 0.33100))
  (define-white-point CIE-1964-D75 (0.29968 0.31740))
  (define-white-point CIE-1964-E   (1/3     1/3    ))
  (define-white-point CIE-1964-F1  (0.31811 0.33559))
  (define-white-point CIE-1964-F2  (0.37925 0.36733))
  (define-white-point CIE-1964-F3  (0.41761 0.38324))
  (define-white-point CIE-1964-F4  (0.44920 0.39074))
  (define-white-point CIE-1964-F5  (0.31975 0.34246))
  (define-white-point CIE-1964-F6  (0.38660 0.37847))
  (define-white-point CIE-1964-F7  (0.31569 0.32960))
  (define-white-point CIE-1964-F8  (0.34902 0.35939))
  (define-white-point CIE-1964-F9  (0.37829 0.37045))
  (define-white-point CIE-1964-F10 (0.35090 0.35444))
  (define-white-point CIE-1964-F11 (0.38541 0.37123))
  (define-white-point CIE-1964-F12 (0.44256 0.39717))
  (values))

;;; cie-xyy.lisp ends here
