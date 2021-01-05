;;; ciexyz.lisp --- CIE XYZ color space.

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

(defclass ciexyz-color (color-object)
  ((x
    :initarg :x
    :initform 0
    :type (real 0)
    :documentation "First tristimulus value, default zero.")
   (y
    :initarg :y
    :initform 0
    :type (real 0)
    :documentation "Second tristimulus value, default zero.")
   (z
    :initarg :z
    :initform 0
    :type (real 0)
    :documentation "Third tristimulus value, default zero."))
  (:documentation "Color class for the CIE XYZ color space."))

(defmethod color-coordinates ((color ciexyz-color))
  (with-slots (x y z) color
    (values x y z)))

(defun make-ciexyz-color (x y z)
  "Create a new color in the CIE XYZ color space.

Arguments X, Y, and Z are the tristimulus values."
  (make-instance 'ciexyz-color :x x :y y :z z))

(defgeneric ciexyz-color-coordinates (color)
  (:documentation "Return the CIE XYZ color space coordinates of the color.

Argument COLOR is a color object.

Values are the X, Y, and Z tristimulus values.")
  (:method ((color ciexyz-color))
    (color-coordinates color)))

(defmethod update-instance-for-different-class :after ((old color-object) (new ciexyz-color) &key)
  (with-slots (x y z) new
    (multiple-value-setq (x y z)
      (ciexyz-color-coordinates old))))

(defmethod absolute-luminance ((color ciexyz-color))
  (slot-value color 'y))

(defmethod normalize-color ((color ciexyz-color) &key (white 1) (black 0))
  (let ((yw (absolute-luminance white))
	(yk (absolute-luminance black)))
    (multiple-value-bind (x* y* ya)
	(ciexyy-color-coordinates color)
      (with-slots (x y z) color
	(multiple-value-setq (x y z)
	  (ciexyz-from-ciexyy x* y* (/ (- ya yk) (- yw yk)))))))
  color)

(defmethod absolute-color ((color ciexyz-color) &key (white 1) (black 0))
  (let ((yw (absolute-luminance white))
	(yk (absolute-luminance black)))
    (multiple-value-bind (x* y* yn)
	(ciexyy-color-coordinates color)
      (with-slots (x y z) color
	(multiple-value-setq (x y z)
	  (ciexyz-from-ciexyy x* y* (+ yk (* yn (- yw yk))))))))
  color)

;;; ciexyz.lisp ends here
