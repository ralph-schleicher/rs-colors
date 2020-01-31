;;; cie-lch.lisp --- CIE L*C*h color space.

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

(export '*cie-lch-default-white-point*)
(defvar *cie-lch-default-white-point* cie-1931-d50
  "The default white point for colors in the CIE L*C*h color space.
Default value is the CIE 1931 D50 standard illuminant.")

(export 'cie-lch-color)
(defclass cie-lch-color (color-object)
  ((L*
    :initarg :L*
    :initform 0
    :type (real 0)
    :documentation "Lightness, default zero.")
   (C*
    :initarg :C*
    :initform 0
    :type (real 0)
    :documentation "Chroma, default zero.")
   (h
    :initarg :h
    :initform 0
    :type (real 0 (360))
    :documentation "Hue, default zero.")
   (white-point
    :initarg :white-point
    :initform *cie-lch-default-white-point*
    :type color-object
    :documentation "White point, default ‘*cie-lch-default-white-point*’."))
  (:documentation "Color class for the CIE L*C*h color space.
Hue is measured in degree angle."))

(defmethod color-coordinates ((color cie-lch-color))
  (with-slots (L* C* h) color
    (values L* C* h)))

(defmethod white-point ((color cie-lch-color))
  (slot-value color 'white-point))

(export 'make-cie-lch-color)
(defun make-cie-lch-color (L* C* h &optional (white-point *cie-lch-default-white-point*))
  "Create a new color in the CIE L*C*h color space."
  (make-instance 'cie-lch-color :L* L* :C* C* :h (mod h 360) :white-point white-point))

(defun cie-lch-from-cie-lab (L* a* b*)
  "Convert CIE L*a*b* color space coordinates
into CIE L*C*h color space coordinates."
  (declare (type real L* a* b*))
  ;; Attempt to be exact, see also ‘cie-lab-from-cie-lch’ below.
  (cond ((zerop b*)
	 (values L* (abs a*) (if (minusp a*) 180 0)))
	((zerop a*)
	 (values L* (abs b*) (if (minusp b*) 270 90)))
	(t
	 (let ((C*h (complex (float a* pi) (float b* pi))))
	   (values L* (abs C*h) (mod (degree-from-radian (phase C*h)) 360))))))

(defun cie-lab-from-cie-lch (L* C* h)
  "Convert CIE L*C*h color space coordinates
into CIE L*a*b* color space coordinates."
  (declare (type real L* C* h))
  ;; On IEEE 754 machines, and maybe others, values of sin(π) and
  ;; cos(π/2) are usually non-zero.
  (cond ((zerop C*)
	 (values L* 0 0))
	((= h 0)
	 (values L* C* 0))
	((= h 90)
	 (values L* 0 C*))
	((= h 180)
	 (values L* (- C*) 0))
	((= h 270)
	 (values L* 0 (- C*)))
	(t
	 (let ((C*h (* C* (cis (radian-from-degree (float h pi))))))
	   (values L* (realpart C*h) (imagpart C*h))))))

(export 'cie-lch-color-coordinates)
(defgeneric cie-lch-color-coordinates (color)
  (:documentation "Return the CIE L*C*h color space coordinates of the color.

Argument COLOR is a color object.")
  (:method ((color cie-lch-color))
    (color-coordinates color))
  ;; Otherwise, go via CIE L*a*b*.
  (:method ((color color-object))
    (multiple-value-bind (L* a* b*)
	(cie-lab-color-coordinates color)
      (cie-lch-from-cie-lab L* a* b*))))

(defmethod cie-lab-color-coordinates ((color cie-lch-color))
  (multiple-value-bind (L* C* h)
      (cie-lch-color-coordinates color)
    (cie-lab-from-cie-lch L* C* h)))

(defmethod cie-xyz-color-coordinates ((color cie-lch-color))
  (multiple-value-bind (L* a* b*)
      (cie-lab-color-coordinates color)
    (cie-xyz-from-cie-lab L* a* b* (white-point color))))

(defmethod update-instance-for-different-class :after ((old color-object) (new cie-lch-color) &key)
  (with-slots (L* C* h white-point) new
    (multiple-value-setq (L* C* h)
      (cie-lch-color-coordinates old))
    (setf white-point (or (white-point old) *cie-lch-default-white-point*))))

;;; cie-lch.lisp ends here
