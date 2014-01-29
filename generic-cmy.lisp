;;; generic-cmy.lisp --- generic CMY color space.

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

(export 'cmy-color)
(defclass cmy-color (color)
  ((c
    :initarg :cyan
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the cyan ink, default zero.")
   (m
    :initarg :magenta
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the magenta ink, default zero.")
   (y
    :initarg :yellow
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the yellow ink, default zero."))
  (:documentation "Color class for a CMY color space."))

(defmethod color-coordinates ((color cmy-color))
  (with-slots (c m y) color
    (values c m y)))

(defun cmy-from-rgb (r g b)
  "Convert RGB color space coordinates
into CMY color space coordinates."
  (declare (type real r g b))
  (let ((c (- 1 r))
	(m (- 1 g))
	(y (- 1 b)))
    (values c m y)))

(defun rgb-from-cmy (c m y)
  "Convert CMY color space coordinates
into RGB color space coordinates."
  (declare (type real c m y))
  (let ((r (- 1 c))
	(g (- 1 m))
	(b (- 1 y)))
    (values r g b)))

(export 'cmy-color-coordinates)
(defgeneric cmy-color-coordinates (color)
  (:documentation "Return the CMY color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the cyan, magenta, and yellow ink.")
  (:method ((color cmy-color))
    (color-coordinates color))
  (:method ((color color))
    (multiple-value-call #'cmy-from-rgb
      (rgb-color-coordinates color))))

(defmethod rgb-color-coordinates ((color cmy-color))
  (multiple-value-call #'rgb-from-cmy
    (cmy-color-coordinates color)))

;;; generic-cmy.lisp ends here
