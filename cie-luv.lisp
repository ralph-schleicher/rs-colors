;;; cie-luv.lisp --- CIE L*u*v* color space.

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

(export 'cie-luv-color)
(defclass cie-luv-color (color-object)
  ((L*
    :initarg :L*
    :initform 0
    :type (real 0)
    :documentation "Lightness, default zero.")
   (u*
    :initarg :u*
    :initform 0
    :type real
    :documentation "First chromaticity coordinate, default zero.")
   (v*
    :initarg :v*
    :initform 0
    :type real
    :documentation "Second chromaticity coordinate, default zero.")
   (white-point
    :initarg :white-point
    :initform (error "Reference white point is not defined.")
    :type color-object
    :documentation "Reference white point."))
  (:documentation "Color class for the CIE L*u*v* color space."))

(defmethod color-coordinates ((color cie-luv-color))
  (with-slots (L* u* v*) color
    (values L* u* v*)))

(defmethod white-point ((color cie-luv-color))
  (slot-value color 'white-point))

(export 'make-cie-luv-color)
(defun make-cie-luv-color (L* u* v* &optional white-point)
  "Create a new color in the CIE L*u*v* color space."
  (make-instance 'cie-luv-color :L* (ensure-type L* '(real 0)) :u* (ensure-type u* 'real) :v* (ensure-type v* 'real) :white-point (ensure-type (or white-point CIE-1931-D50) 'color-object)))

(defun cie-uv-from-xy (x y s)
  (declare (type real x y s))
  (when (zerop s)
    (error 'division-by-zero
	   :operation 'cie-uv-from-xy
	   :operands (list x y s)))
  (values (/ (* 4 x) s)
	  (/ (* 9 y) s)))

(defun cie-luv-from-cie-xyz (x y z w)
  "Convert CIE XYZ color space coordinates
into CIE L*u*v* color space coordinates.

This conversion requires a reference white point."
  (declare (type real x y z))
  (multiple-value-bind (x*n y*n yn)
      (cie-xyy-color-coordinates w)
    (multiple-value-bind (un vn)
	(cie-uv-from-xy x*n y*n (+ (- (* 2 x*n)) (* 12 y*n) 3))
      (multiple-value-bind (u v)
	  (cie-uv-from-xy x y (+ x (* 15 y) (* 3 z)))
	(let* ((L* (cie-L*-from-Y/Yn (/ y yn)))
	       (u* (* 13 L* (- u un)))
	       (v* (* 13 L* (- v vn))))
	  (values L* u* v*))))))

(defun cie-xyz-from-cie-luv (L* u* v* w)
  "Convert CIE L*u*v* color space coordinates
into CIE XYZ color space coordinates.

This conversion requires a reference white point."
  (declare (type real L* u* v*))
  (multiple-value-bind (x*n y*n yn)
      (cie-xyy-color-coordinates w)
    (multiple-value-bind (un vn)
	(cie-uv-from-xy x*n y*n (+ (- (* 2 x*n)) (* 12 y*n) 3))
      (let* ((u (+ (/ u* (* 13 L*) un)))
	     (v (+ (/ v* (* 13 L*) vn)))
	     (y (* yn (cie-Y/Yn-from-L* L*)))
	     (x (* y (/ (* 9 u) (* 4 v))))
	     (z (* y (/ (- 12 (* 3 u) (* 20 v)) (* 4 v)))))
	(values x y z)))))

(export 'cie-luv-color-coordinates)
(defgeneric cie-luv-color-coordinates (color)
  (:documentation "Return the CIE L*u*v* color space coordinates of the color.

Argument COLOR is a color object.")
  (:method ((color cie-luv-color))
    (color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color-object))
    (multiple-value-bind (x y z)
	(cie-xyz-color-coordinates color)
      (cie-luv-from-cie-xyz x y z (white-point color)))))

(defmethod cie-xyz-color-coordinates ((color cie-luv-color))
  (multiple-value-bind (L* u* v*)
      (cie-luv-color-coordinates color)
    (cie-xyz-from-cie-luv L* u* v* (white-point color))))

(defmethod update-instance-for-different-class :after ((old color-object) (new cie-luv-color) &key)
  (with-slots (L* u* v*) new
    (multiple-value-setq (L* u* v*)
      (cie-luv-color-coordinates old))))

;;; cie-luv.lisp ends here
