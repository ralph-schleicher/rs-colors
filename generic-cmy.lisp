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

(export 'generic-cmy-color)
(defclass generic-cmy-color (cmy-color-object generic-color-object)
  ()
  (:documentation "Color class for the generic CMY color space.

The generic CMY color space is a mathematical description of the
CMY color model.  It is not associated with a particular device."))

(export 'make-generic-cmy-color)
(defun make-generic-cmy-color (cyan magenta yellow &key byte-size)
  "Create a new color in the generic CMY color space.

First argument CYAN is the intensity of the cyan ink.
Second argument MAGENTA is the intensity of the magenta ink.
Third argument YELLOW is the intensity of the yellow ink.

Arguments CYAN, MAGENTA, and YELLOW have to be normalized color values
in the closed interval [0, 1].

Keyword argument BYTE-SIZE is the number of bits used to represent a
color value.  If specified, arguments CYAN, MAGENTA, and YELLOW are
scaled accordingly.

Example:

     (make-generic-cmy-color 3/255 80/255 193/255)
     (make-generic-cmy-color 3 80 193 :byte-size 8)"
  (let (c m y)
    (if (not byte-size)
	(setf c (ensure-type cyan '(real 0 1))
	      m (ensure-type magenta '(real 0 1))
	      y (ensure-type yellow '(real 0 1)))
      (let ((s (1- (expt 2 (ensure-type byte-size '(integer 1))))))
	(setf c (/ (ensure-type cyan `(integer 0 ,s)) s)
	      m (/ (ensure-type magenta `(integer 0 ,s)) s)
	      y (/ (ensure-type yellow `(integer 0 ,s)) s))))
    (make-instance 'generic-cmy-color :cyan c :magenta m :yellow y)))

(export 'make-generic-cmy-color-from-number)
(defun make-generic-cmy-color-from-number (value &key (byte-size 8))
  "Create a new color in the generic CMY color space.

Argument VALUE is a non-negative integral number.

Keyword argument BYTE-SIZE is the number of bits used to represent a
primary.  Default is eight bit (one byte).  The most significant bits
denote the intensity of the cyan primary.

Example:

     (make-generic-cmy-color-from-number #X0350C1)"
  (ensure-type value '(integer 0))
  (ensure-type byte-size '(integer 1))
  (multiple-value-bind (cyan magenta yellow)
      (decode-triple value byte-size)
    (make-generic-cmy-color cyan magenta yellow byte-size)))

(defun generic-cmy-from-generic-rgb (r g b)
  "Convert RGB color space coordinates
into CMY color space coordinates."
  (declare (type real r g b))
  (let ((c (- 1 r))
	(m (- 1 g))
	(y (- 1 b)))
    (values c m y)))

(defun generic-rgb-from-generic-cmy (c m y)
  "Convert CMY color space coordinates
into RGB color space coordinates."
  (declare (type real c m y))
  (let ((r (- 1 c))
	(g (- 1 m))
	(b (- 1 y)))
    (values r g b)))

(export 'generic-cmy-color-coordinates)
(defgeneric generic-cmy-color-coordinates (color)
  (:documentation "Return the CMY color space coordinates of the color.

Argument COLOR is a color object.

Values are the intensities of the cyan, magenta, and yellow ink.")
  (:method ((color generic-cmy-color))
    (color-coordinates color))
  (:method ((color color-object))
    (multiple-value-call #'generic-cmy-from-generic-rgb
      (generic-rgb-color-coordinates color))))

(defmethod generic-rgb-color-coordinates ((color generic-cmy-color))
  (multiple-value-call #'generic-rgb-from-generic-cmy
    (color-coordinates color)))

(defmethod update-instance-for-different-class :after ((old color-object) (new generic-cmy-color) &key)
  (with-slots (c m y) new
    (multiple-value-setq (c m y)
      (generic-cmy-color-coordinates old))))

;;; generic-cmy.lisp ends here
