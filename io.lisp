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

(define-color-printer :xcms-cie-xyz (color stream :export t)
  (multiple-value-bind (x y z)
      (cie-xyz-color-coordinates color)
    (format stream
	    "CIEXYZ:~A/~A/~A"
	    (float x 1F0)
	    (float y 1F0)
	    (float z 1F0))))

(define-color-printer :xcms-cie-xyy (color stream :export t)
  (multiple-value-bind (x* y* y)
      (cie-xyy-color-coordinates color)
    (format stream
	    "CIExyY:~A/~A/~A"
	    (float x* 1F0)
	    (float y* 1F0)
	    (float y  1F0))))

(define-color-printer :xcms-rgb (color stream :export t)
  (ensure-type color 'rgb-color-object)
  (multiple-value-bind (r g b)
      (color-coordinates color)
    (format stream
	    "RGBi:~A/~A/~A"
	    (float r 1F0)
	    (float g 1F0)
	    (float b 1F0))))

(define-color-printer :html (color stream :export t)
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
;; Value is a color object in the sRGB color space."
(define-color-reader :html (stream :export t)
  (unless (char= (read-char stream) #\#)
    (error "Invalid HTML color syntax; expect a `#' character."))
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

(define-color-printer :css-rgb (color stream :export t)
  (multiple-value-bind (r g b)
      (srgb-color-coordinates color)
    (format stream
	    "rgb(~A%, ~A%, ~A%)"
	    (float (* r 100) 1F0)
	    (float (* g 100) 1F0)
	    (float (* b 100) 1F0))))

(define-color-printer :css-hsl (color stream :export t)
  (multiple-value-bind (h s l)
      (multiple-value-call #'generic-hsl-from-generic-rgb
	(srgb-color-coordinates color))
    (format stream
	    "hsl(~A, ~A%, ~A%)"
	    (float h 1F0)
	    (float (* s 100) 1F0)
	    (float (* l 100) 1F0))))

;;; io.lisp ends here
