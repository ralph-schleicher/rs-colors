;;; generic-cmyk.lisp --- generic CMYK color space.

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

(export 'cmyk-color)
(defclass cmyk-color (color)
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
    :documentation "Intensity of the yellow ink, default zero.")
   (k
    :initarg :black
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the black ink, default zero."))
  (:documentation "Color class for a CMYK color space."))

(defmethod color-coordinates ((color cmyk-color))
  (with-slots (c m y k) color
    (values c m y k)))

(defun cmyk-from-cmy (c m y)
  "Convert CMY color space coordinates
into CMYK color space coordinates."
  (declare (type real c m y))
  (let* ((k (min c m y))
	 (1-k (- 1 k)))
    (if (zerop 1-k)
	(values 0 0 0 k)
      (values (/ (- c k) 1-k)
	      (/ (- m k) 1-k)
	      (/ (- y k) 1-k) k))))

(defun cmy-from-cmyk (c m y k)
  "Convert CMYK color space coordinates
into CMY color space coordinates."
  (declare (type real c m y k))
  (let ((1-k (- 1 k)))
    (values (min 1 (+ (* c 1-k) k))
	    (min 1 (+ (* m 1-k) k))
	    (min 1 (+ (* y 1-k) k)))))

(export 'cmyk-color-coordinates)
(defgeneric cmyk-color-coordinates (color)
  (:documentation "Return the CMYK color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the cyan, magenta, yellow, and black ink.")
  (:method ((color cmyk-color))
    (color-coordinates color))
  (:method ((color color))
    (multiple-value-call #'cmyk-from-cmy
      (cmy-color-coordinates color))))

(defmethod cmy-color-coordinates ((color cmyk-color))
  (multiple-value-call #'cmy-from-cmyk
    (cmyk-color-coordinates color)))

;;; generic-cmyk.lisp ends here
