;;; io.lisp --- the greek goddess of input and output.

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

(export 'define-color-printer)
(defmacro define-color-printer (style (color stream &key export inline) &body body)
  "Argument STYLE is a string designator."
  (let* ((suffix (string-upcase (string style)))
	 (printer (intern (concatenate 'string "PRINT-COLOR-" suffix)))
	 (formatter (intern (concatenate 'string "COLOR-FORMATTER-" suffix)))
	 (rest (gensym "REST")))
    `(progn
       ,@(when export `((export (quote ,printer))))
       ,@(when inline `((declaim (inline ,printer))))
       (defun ,printer (,color &optional (,stream *standard-output*))
	 ,@body
	 ,color)
       ,@(when export `((export (quote ,formatter))))
       (defvar ,formatter (lambda (,stream &optional (,color (error "Missing required argument.")) &rest ,rest)
			    (,printer ,color ,stream)
			    ,rest))
       (values))))

(export 'define-color-reader)
(defmacro define-color-reader (style (stream &key export inline) &body body)
  "Argument STYLE is a string designator."
  (let* ((suffix (string-upcase (string style)))
	 (reader (intern (concatenate 'string "READ-COLOR-" suffix))))
    `(progn
       ,@(when export `((export (quote ,reader))))
       ,@(when inline `((declaim (inline ,reader))))
       (defun ,reader (&optional (,stream *standard-input*))
	 ,@body))))

(define-color-printer xcms-cie-xyz (color stream :export t)
  (multiple-value-bind (x y z)
      (cie-xyz-color-coordinates color)
    (let ((*read-default-float-format* 'single-float))
      (format stream
	      "CIEXYZ:~A/~A/~A"
	      (float x 1F0)
	      (float y 1F0)
	      (float z 1F0)))))

(define-color-printer xcms-cie-xyy (color stream :export t)
  (multiple-value-bind (x* y* y)
      (cie-xyy-color-coordinates color)
    (let ((*read-default-float-format* 'single-float))
      (format stream
	      "CIExyY:~A/~A/~A"
	      (float x* 1F0)
	      (float y* 1F0)
	      (float y  1F0)))))

(define-color-printer xcms-rgb (color stream :export t)
  (ensure-type color 'rgb-color-object)
  (multiple-value-bind (r g b)
      (color-coordinates color)
    (let ((*read-default-float-format* 'single-float))
      (format stream
	      "RGBi:~A/~A/~A"
	      (float r 1F0)
	      (float g 1F0)
	      (float b 1F0)))))

(define-color-printer html (color stream :export t)
  (multiple-value-bind (r g b)
      (srgb-color-coordinates color)
    (format stream
	    "#~6,'0X"
	    (encode-triple
	     (round (* r 255))
	     (round (* g 255))
	     (round (* b 255))))))

;; Read a numerical HTML color definition, that is
;; a hexadecimal number prefixed by a hash mark.
;;
;; Argument STREAM is an input stream.
;;
;; Value is a color object in the sRGB color space.
(define-color-reader html (stream :export t)
  (unless (char= (read-char stream) #\#)
    (error "Invalid HTML color syntax; expect a '#' character."))
  (iter (with val = 0)
	(with len = 0)
	(for ch = (read-char stream nil))
	(for end-of-file-p = (null ch))
	(until end-of-file-p)
	(for dig = (digit-char-p ch 16))
	(when (null dig)
	  (unread-char ch stream)
	  (finish))
	(setf val (+ (* val 16) dig))
	(incf len)
	(finally
	 (multiple-value-bind (quotient remainder)
	     (truncate len 3)
	   (when (or (= quotient 0) (/= remainder 0))
	     (when end-of-file-p
	       (error 'end-of-file :stream stream))
	     (error "Invalid HTML color syntax; require a multiple of three hexadecimal digits."))
	   (let ((byte-size (* quotient 4)))
	     (multiple-value-bind (r g b)
		 (decode-triple val byte-size)
	       (return (make-srgb-color r g b :byte-size byte-size))))))
	))

(define-color-printer css3-rgb (color stream :export t)
  (multiple-value-bind (r g b)
      (srgb-color-coordinates color)
    (alexandria:if-let ((red   (multiples r 255))
			(green (multiples g 255))
			(blue  (multiples b 255)))
	(format stream "rgb(~A, ~A, ~A)" red green blue)
      (let ((*read-default-float-format* 'single-float))
	(format stream
		"rgb(~A%, ~A%, ~A%)"
		(float (* r 100) 1F0)
		(float (* g 100) 1F0)
		(float (* b 100) 1F0))))))

(define-color-reader css3-rgb (stream :export t)
  ;; Read functional notation of the form ‘rgb(RED,GREEN,BLUE)’.
  ;; Whitespace characters are allowed around the numerical values.
  ;; All RGB colors are specified in the sRGB color space...
  ;; Values outside the device gamut should be clipped...
  (let (r g b number-format)
    (labels ((read-number (stream)
	       (let (value)
		 ;; Skip leading white-space characters.
		 (peek-char t stream)
		 ;; Read the numeric value.
		 (cond ((eq number-format 'integer)
			(setf value (read-integer stream t nil nil
						  :unsigned-number :plus))
			(setf value (clamp value 0 255)))
		       ((eq number-format 'float)
			(setf value (read-float stream t nil nil
						:unsigned-number :plus
						:exponent-marker ()
						:float-format 'double-float))
			(unless (char= (read-char stream) #\%)
			  (error "Invalid CSS color syntax; expect a '%' character."))
			(setf value (clamp (/ value 100D0) 0D0 1D0)))
		       (t
			;; Variable NUMBER-FORMAT is not set.
			(setf value (read-float stream t nil nil
						:unsigned-number :plus
						:exponent-marker ()
						:float-format 'double-float))
			(cond ((char= (peek-char nil stream nil #\Space) #\%)
			       ;; It's a percentage value.
			       (setf value (clamp (/ value 100D0) 0D0 1D0))
			       ;; Next value has to be a percentage, too.
			       (setf number-format 'float)
			       ;; Gobble ‘%’ character.
			       (read-char stream))
			      ((integerp value)
			       (setf value (clamp value 0 255))
			       (setf number-format 'integer))
			      (t
			       ;; Value is a floating-point number.
			       (error "Invalid CSS color syntax; expect a '%' character.")))))
		 ;; Skip trailing white-space characters.
		 (peek-char t stream)
		 ;; Return value.
		 value)))
      (unless (char= (read-char stream) #\r)
	(error "Invalid CSS color syntax; expect a 'r' character."))
      (unless (char= (read-char stream) #\g)
	(error "Invalid CSS color syntax; expect a 'g' character."))
      (unless (char= (read-char stream) #\b)
	(error "Invalid CSS color syntax; expect a 'b' character."))
      (unless (char= (read-char stream) #\()
	(error "Invalid CSS color syntax; expect a '(' character."))
      (setf r (read-number stream))
      (unless (char= (read-char stream) #\,)
	(error "Invalid CSS color syntax; expect a ',' character."))
      (setf g (read-number stream))
      (unless (char= (read-char stream) #\,)
	(error "Invalid CSS color syntax; expect a ',' character."))
      (setf b (read-number stream))
      (unless (char= (read-char stream) #\))
	(error "Invalid CSS color syntax; expect a ')' character."))
      (if (eq number-format 'integer)
	  (make-srgb-color r g b :byte-size 8)
	(make-srgb-color r g b)))))

(define-color-printer css3-hsl (color stream :export t)
  (multiple-value-bind (h s l)
      (multiple-value-call #'generic-hsl-from-generic-rgb
	(srgb-color-coordinates color))
    (let ((*read-default-float-format* 'single-float))
      (format stream
	      "hsl(~A, ~A%, ~A%)"
	      (float h 1F0)
	      (float (* s 100) 1F0)
	      (float (* l 100) 1F0)))))

(define-color-reader css3-hsl (stream :export t)
  ;; Read functional notation of the form ‘hsl(HUE,SATURATION,LIGHTNESS)’.
  ;; Whitespace characters are allowed around the numerical values.
  ;; Values outside the device gamut should be clipped...
  (let (h s l)
    (labels ((read-number (stream &optional percentage)
	       (let (value)
		 ;; Skip leading white-space characters.
		 (peek-char t stream)
		 ;; Read the numeric value.
		 (setf value (read-float stream t nil nil
					 :unsigned-number :plus
					 :exponent-marker ()
					 :float-format 'double-float))
		 (when percentage
		   (unless (char= (read-char stream) #\%)
		     (error "Invalid CSS color syntax; expect a '%' character."))
		   (setf value (clamp (/ value 100D0) 0D0 1D0)))
		 ;; Skip trailing white-space characters.
		 (peek-char t stream)
		 ;; Return value.
		 value)))
      (unless (char= (read-char stream) #\h)
	(error "Invalid CSS color syntax; expect a 'h' character."))
      (unless (char= (read-char stream) #\s)
	(error "Invalid CSS color syntax; expect a 's' character."))
      (unless (char= (read-char stream) #\l)
	(error "Invalid CSS color syntax; expect a 'l' character."))
      (unless (char= (read-char stream) #\()
	(error "Invalid CSS color syntax; expect a '(' character."))
      (setf h (mod (read-number stream) 360))
      (unless (char= (read-char stream) #\,)
	(error "Invalid CSS color syntax; expect a ',' character."))
      (setf s (read-number stream t))
      (unless (char= (read-char stream) #\,)
	(error "Invalid CSS color syntax; expect a ',' character."))
      (setf l (read-number stream t))
      (unless (char= (read-char stream) #\))
	(error "Invalid CSS color syntax; expect a ')' character."))
      (change-class (make-generic-hsl-color h s l) 'srgb-color))))

(define-color-reader css3 (stream :export t)
  "Read a CSS3 color value, i.e. either a numerical HTML color
definition or a RGB value or HSL value in functional notation.

Argument STREAM is an input stream.

Value is a color object in the sRGB color space."
  (case (peek-char nil stream nil #\Space)
    (#\#
     (read-color-html stream))
    (#\r
     (read-color-css3-rgb stream))
    (#\h
     (read-color-css3-hsl stream))
    (t
     (error "Unknown CSS3 color syntax."))))

;;; io.lisp ends here
