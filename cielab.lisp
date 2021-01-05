;;; cielab.lisp --- CIE L*a*b* color space.

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

(defvar *cielab-default-white-point* cie-1931-white-point-d50
  "The default white point for colors in the CIE L*a*b* color space.
Default value is the CIE 1931 D50 standard illuminant.")

(defclass cielab-color (color-object)
  ((L*
    :initarg :L*
    :initform 0
    :type (real 0)
    :documentation "Lightness, default zero.")
   (a*
    :initarg :a*
    :initform 0
    :type real
    :documentation "Red/green scale, default zero.")
   (b*
    :initarg :b*
    :initform 0
    :type real
    :documentation "Yellow/blue scale, default zero.")
   (white-point
    :initarg :white-point
    :initform *cielab-default-white-point*
    :type color-object
    :documentation "White point, default ‘*cielab-default-white-point*’."))
  (:documentation "Color class for the CIE L*a*b* color space."))

(defmethod color-coordinates ((color cielab-color))
  (with-slots (L* a* b*) color
    (values L* a* b*)))

(defmethod white-point ((color cielab-color))
  (slot-value color 'white-point))

(defun make-cielab-color (L* a* b* &optional (white-point *cielab-default-white-point*))
  "Create a new color in the CIE L*a*b* color space."
  (make-instance 'cielab-color :L* L* :a* a* :b* b* :white-point white-point))

(defun cielab-from-ciexyz (x y z w)
  "Convert CIE XYZ color space coordinates
into CIE L*a*b* color space coordinates.

This conversion requires a reference white point."
  (declare (type real x y z))
  (labels ((encode (c)
	     (if (> c 216/24389)
		 (cube-root c)
	       (+ (* 24389/3132 c) 16/116))))
    (multiple-value-bind (xn yn zn)
	(ciexyz-color-coordinates (or w *cielab-default-white-point*))
      (let* ((x (encode (/ x xn)))
	     (y (encode (/ y yn)))
	     (z (encode (/ z zn)))
	     (L* (- (* 116 y) 16))
	     (a* (* 500 (- x y)))
	     (b* (* 200 (- y z))))
	(values L* a* b*)))))

(defun ciexyz-from-cielab (L* a* b* w)
  "Convert CIE L*a*b* color space coordinates
into CIE XYZ color space coordinates.

This conversion requires a reference white point."
  (declare (type real L* a* b*))
  (labels ((decode (c)
	     (if (> c #.(cube-root 216/24389))
		 (cube c)
	       (/ (- c 16/116) 24389/3132))))
    (multiple-value-bind (xn yn zn)
	(ciexyz-color-coordinates (or w *cielab-default-white-point*))
      (let* ((y (/ (+ L* 16) 116))
	     (x (+ y (/ a* 500)))
	     (z (- y (/ b* 200))))
	(values (* (decode x) xn)
		(* (decode y) yn)
		(* (decode z) zn))))))

(defgeneric cielab-color-coordinates (color)
  (:documentation "Return the CIE L*a*b* color space coordinates of the color.

Argument COLOR is a color object.")
  (:method ((color cielab-color))
    (color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color-object))
    (multiple-value-bind (x y z)
	(ciexyz-color-coordinates color)
      (cielab-from-ciexyz x y z (white-point color)))))

(defmethod ciexyz-color-coordinates ((color cielab-color))
  (multiple-value-bind (L* a* b*)
      (cielab-color-coordinates color)
    (ciexyz-from-cielab L* a* b* (white-point color))))

(defmethod update-instance-for-different-class :after ((old color-object) (new cielab-color) &key)
  (with-slots (L* a* b* white-point) new
    (multiple-value-setq (L* a* b*)
      (cielab-color-coordinates old))
    (setf white-point (or (white-point old) *cielab-default-white-point*))))

;;; cielab.lisp ends here
