;;; ciergb.lisp --- CIE RGB color space.

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

(export 'ciergb-color)
(defclass ciergb-color (rgb-color-object)
  ()
  (:documentation "Color class for the CIE RGB color space."))

(export 'make-ciergb-color)
(defun make-ciergb-color (red green blue)
  "Create a new color in the CIE RGB color space.

First argument RED is the intensity of the red primary.
Second argument GREEN is the intensity green primary.
Third argument BLUE is the intensity of the blue primary.

Arguments RED, GREEN, and BLUE have to be normalized intensity values
in the closed interval [0, 1]."
  (make-instance 'ciergb-color :red red :green green :blue blue))

(let ((c (make-matrix 49000/100000 31000/100000 20000/100000
		      17697/100000 81240/100000  1063/100000
		          0/100000  1000/100000 99000/100000)))
  (defconst ciergb-from-ciexyz-transformation-matrix (matrix-inverse (copy-matrix c))
    "Transformation matrix to convert CIE XYZ color space coordinates
into CIE RGB color space coordinates.")
  (defconst ciexyz-from-ciergb-transformation-matrix c
    "Transformation matrix to convert CIE RGB color space coordinates
into CIE XYZ color space coordinates.")
  (values))

(defun ciergb-from-ciexyz (x y z)
  "Convert CIE XYZ color space coordinates
into CIE RGB color space coordinates."
  (declare (type real x y z))
  (multiple-value-bind (r g b)
      (linear-transformation ciergb-from-ciexyz-transformation-matrix x y z)
    (values (clamp r 0 1)
	    (clamp g 0 1)
	    (clamp b 0 1))))

(defun ciexyz-from-ciergb (r g b)
  "Convert CIE RGB color space coordinates
into CIE XYZ color space coordinates."
  (declare (type real r g b))
  (linear-transformation ciexyz-from-ciergb-transformation-matrix r g b))

(export 'ciergb-color-coordinates)
(defgeneric ciergb-color-coordinates (color)
  (:documentation "Return the CIE RGB color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the red, green, and blue primary.")
  (:method ((color ciergb-color))
    (color-coordinates color))
  (:method ((color generic-color-object))
    (generic-rgb-color-coordinates color))
  ;; Otherwise, go via CIE XYZ.
  (:method ((color color-object))
    (multiple-value-call #'ciergb-from-ciexyz
      (ciexyz-color-coordinates color))))

(defmethod generic-rgb-color-coordinates ((color ciergb-color))
  (color-coordinates color))

(defmethod ciexyz-color-coordinates ((color ciergb-color))
  (multiple-value-call #'ciexyz-from-ciergb
    (color-coordinates color)))

(defmethod update-instance-for-different-class :after ((old color-object) (new ciergb-color) &key)
  (with-slots (r g b) new
    (multiple-value-setq (r g b)
      (ciergb-color-coordinates old))))

;;; ciergb.lisp ends here
