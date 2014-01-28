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

(defvar color-printers (make-hash-table :test #'equal)
  "Dictionary of color printers.")

(defvar color-readers (make-hash-table :test #'equal)
  "Dictionary of color readers.")

(export 'define-color-printer)
(defmacro define-color-printer (style (color stream) &body body)
  "Argument STYLE is a string designator."
  (let ((key (gensym))
	(fun (gensym)))
    `(let ((,key (string ,style))
	   (,fun (lambda (,color ,stream) ,@body (values))))
       (setf (gethash ,key color-printers) ,fun)
       (values ,key))))

(export 'define-color-reader)
(defmacro define-color-reader (style (stream) &body body)
  "Argument STYLE is a string designator."
  (let ((key (gensym))
	(fun (gensym)))
    `(let ((,key (string ,style))
	   (,fun (lambda (,stream) ,@body)))
       (setf (gethash ,key color-readers) ,fun)
       (values ,key))))

(export 'print-color)
(defun print-color (style color &optional (stream *standard-output*))
  (multiple-value-bind (printer printer-exists)
      (gethash (string style) color-printers)
    (when (not printer-exists)
      (error "Unknown color format `~A'." style))
    (with-standard-io-syntax
      (funcall printer color stream))))

(export 'format-color)
(defun format-color (destination style color)
  (multiple-value-bind (printer printer-exists)
      (gethash (string style) color-printers)
    (when (not printer-exists)
      (error "Unknown color format `~A'." style))
    (with-standard-io-syntax
      (etypecase destination
	(stream
	 (funcall printer color destination))
	((member t)
	 (funcall printer color *standard-output*))
	(string
	 (with-output-to-string (stream destination)
	   (funcall printer color stream)))
	(null
	 (with-output-to-string (stream)
	   (funcall printer color stream)))
	))))

(export 'read-color)
(defun read-color (style &optional (stream *standard-input*))
  (multiple-value-bind (reader reader-exists)
      (gethash (string style) color-readers)
    (when (not reader-exists)
      (error "Unknown color format `~A'." style))
    (with-standard-io-syntax
      (funcall reader stream))))

(define-color-printer :xcms-cie-xyz (color stream)
  (multiple-value-bind (x y z)
      (cie-xyz-color-coordinates color)
    (format stream
	    "CIEXYZ:~A/~A/~A"
	    (float x 1F0)
	    (float y 1F0)
	    (float z 1F0))))

(define-color-printer :xcms-cie-xyy (color stream)
  (multiple-value-bind (x* y* y)
      (cie-xyy-color-coordinates color)
    (format stream
	    "CIExyY:~A/~A/~A"
	    (float x* 1F0)
	    (float y* 1F0)
	    (float y  1F0))))

(define-color-printer :xcms-rgb (color stream)
  (ensure-type color 'rgb-color)
  (multiple-value-bind (r g b)
      (color-coordinates color)
    (format stream
	    "RGBi:~A/~A/~A"
	    (float r 1F0)
	    (float g 1F0)
	    (float b 1F0))))

(define-color-printer :html (color stream)
  (multiple-value-bind (r g b)
      (srgb-color-coordinates color)
    (format stream
	    "#~6,'0X"
	    (encode-triple
	     (round (* r 255))
	     (round (* g 255))
	     (round (* b 255))))))

(define-color-reader :html (stream)
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

(define-color-printer :css-rgb (color stream)
  (multiple-value-bind (r g b)
      (srgb-color-coordinates color)
    (format stream
	    "rgb(~A%, ~A%, ~A%)"
	    (float (* r 100) 1F0)
	    (float (* g 100) 1F0)
	    (float (* b 100) 1F0))))

;;; io.lisp ends here
