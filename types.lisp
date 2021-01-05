;;; types.lisp --- basic data types.

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

(defclass color-object ()
  ()
  (:documentation "Base class for a color."))

(defmethod initialize-instance :after ((color color-object) &key)
  (iter (with class = (class-of color))
	(for slot :in (closer-mop:class-slots class))
	(for slot-name = (closer-mop:slot-definition-name slot))
	(when (slot-boundp color slot-name)
	  (for slot-value = (slot-value color slot-name))
	  ;; Check that the contents of the slot is of the specified
	  ;; data type.
	  (for slot-type = (closer-mop:slot-definition-type slot))
	  (unless (typep slot-value slot-type)
	    (error 'type-error :datum slot-value :expected-type slot-type)))))

(defun colorp (object)
  "Return true if OBJECT is a color object."
  (typep object 'color-object))

(defgeneric color-coordinates (color)
  (:documentation "Return the color space coordinates of the color.

Argument COLOR is a color object.")
  (:method ((color color-object))
    (declare (ignore color))
    (values)))

(defgeneric white-point (color)
  (:documentation "Return the white point of the color.

Argument COLOR is a color object.

Value is the color object of the color's white point, or nil if the
white point is not defined or if multiple white points exist.")
  (:method ((color color-object))
    (declare (ignore color))))

(defgeneric copy-color (color)
  (:documentation "Return a shallow copy of the color.

Argument COLOR is a color object.")
  (:method ((color color-object))
    (iter (with class = (class-of color))
	  (with copy = (allocate-instance class))
	  (for slot :in (closer-mop:class-slots class))
	  (for slot-name = (closer-mop:slot-definition-name slot))
	  (when (slot-boundp color slot-name)
	    (setf (slot-value copy slot-name) (slot-value color slot-name)))
	  (finally
	   (return copy)))))

(defun coerce-color (color color-type)
  "Coerce the color object into the specified color type.

First argument COLOR is a color object.
Second argument COLOR-TYPE is a color data type.

If argument COLOR is already a color of the requested color data
type, return COLOR as is (no conversion).  Otherwise, return a new
color with the color coordinates of COLOR converted into the color
space denoted by COLOR-TYPE."
  (if (eq (type-of color) color-type)
      color
    (change-class (copy-color color) color-type)))

(defmethod print-object ((color color-object) stream)
  (print-unreadable-object (color stream :type t :identity t)
    (princ (multiple-value-list (color-coordinates color)) stream)))

;; Steve Losh wrote:
;;
;; Some implementations (e.g. CCL) are particularly aggressive about
;; inlining constants, and will fail during compilation without a way
;; to dump them into FASLs:
;;
;; [ClozureCL] COMMON-LISP-USER> (ql:quickload 'rs-colors)
;; ...
;; > Error: No MAKE-LOAD-FORM method is defined for #<CIE-XYY-COLOR (0.34567 0.3585 1) #x302001EC0A0D>
(defmethod make-load-form ((color color-object) &optional environment)
  (make-load-form-saving-slots color :environment environment))

(defclass rgb-color-object (color-object)
  ((r
    :initarg :red
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the red primary, default zero.")
   (g
    :initarg :green
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the green primary, default zero.")
   (b
    :initarg :blue
    :initform 0
    :type (real 0 1)
    :documentation "Intensity of the blue primary, default zero."))
  (:documentation "Color class for a RGB color space."))

(defmethod color-coordinates ((color rgb-color-object))
  (with-slots (r g b) color
    (values r g b)))

(defclass hsv-color-object (color-object)
  ((h
    :initarg :hue
    :initform 0
    :type (real 0 (360))
    :documentation "Hue, default zero.")
   (s
    :initarg :saturation
    :initform 0
    :type (real 0 1)
    :documentation "Saturation, default zero.")
   (v
    :initarg :value
    :initform 0
    :type (real 0 1)
    :documentation "Value (brightness), default zero."))
  (:documentation "Color class for a HSV/HSB color space."))

(defmethod color-coordinates ((color hsv-color-object))
  (with-slots (h s v) color
    (values h s v)))

(defclass hsl-color-object (color-object)
  ((h
    :initarg :hue
    :initform 0
    :type (real 0 (360))
    :documentation "Hue, default zero.")
   (s
    :initarg :saturation
    :initform 0
    :type (real 0 1)
    :documentation "Saturation, default zero.")
   (l
    :initarg :lightness
    :initform 0
    :type (real 0 1)
    :documentation "Lightness, default zero."))
  (:documentation "Color class for a HSL color space."))

(defmethod color-coordinates ((color hsl-color-object))
  (with-slots (h s l) color
    (values h s l)))

(defclass cmy-color-object (color-object)
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

(defmethod color-coordinates ((color cmy-color-object))
  (with-slots (c m y) color
    (values c m y)))

;; Do not inherit from ‘cmy-color-object’ because the numerical values
;; of cyan, magenta, and yellow have a different meaning.
(defclass cmyk-color-object (color-object)
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

(defmethod color-coordinates ((color cmyk-color-object))
  (with-slots (c m y k) color
    (values c m y k)))

(defclass generic-color-object (color-object)
  ()
  (:documentation "Color class for the mathematical model of a color space."))

(defgeneric absolute-luminance (object)
  (:documentation "Return absolute luminance.")
  (:method ((object real))
    (ensure-type object '(real 0))))

(defgeneric normalize-color (color &key)
  (:documentation "Convert from absolute color coordinates to normalized color coordinates."))

(defgeneric absolute-color (color &key)
  (:documentation "Convert from normalized color coordinates to absolute color coordinates."))

;;; types.lisp ends here
