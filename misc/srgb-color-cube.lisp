;;; srgb-color-cube.lisp --- color coordinates of the sRGB color cube.

;; Copyright (C) 2020 Ralph Schleicher

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

(in-package :common-lisp-user)

(defvar *srgb-color-cube-header*
  (labels ((header (list)
	     (let (row)
	       (dolist (elem list)
		 (dolist (tem (rest elem))
		   (push (concatenate 'string (first elem) "_" tem) row)))
	       (nreverse row))))
    (header '(("sRGB" "R" "G" "B")
	      ("RGB" "R" "G" "B")
	      ("HSV" "H" "S" "V")
	      ("HSL" "H" "S" "L")
	      ("CMY" "C" "M" "Y")
	      ("CMYK" "C" "M" "Y" "K")
	      ("CIERGB" "R" "G" "B")
	      ("CIEXYZ" "X" "Y" "Z")
	      ("CIExyY" "x" "y" "Y")
	      ("CIELuv" "L" "u" "v")
	      ("CIELab" "L" "a" "b")
	      ("CIELCh" "L" "C" "h")))))

(defvar *srgb-color-cube-body*
  (labels ((coordinates (color)
	     (mapcar (lambda (number)
		       (coerce number 'single-float))
		     (multiple-value-list
		      (rs-colors:color-coordinates color)))))
    (let (rows (list '(0 31 63 95 127 159 191 223 255)))
      (dolist (red list)
	(dolist (green list)
	  (dolist (blue list)
	    (let* ((srgb (rs-colors:make-srgb-color red green blue :byte-size 8))
		   (rgb (rs-colors:coerce-color srgb 'rs-colors:generic-rgb-color))
		   (hsv (rs-colors:coerce-color rgb 'rs-colors:generic-hsv-color))
		   (hsl (rs-colors:coerce-color rgb 'rs-colors:generic-hsl-color))
		   (cmy (rs-colors:coerce-color rgb 'rs-colors:generic-cmy-color))
		   (cmyk (rs-colors:coerce-color cmy 'rs-colors:generic-cmyk-color))
		   (ciexyz (rs-colors:coerce-color srgb 'rs-colors:ciexyz-color))
		   (ciergb (rs-colors:coerce-color ciexyz 'rs-colors:ciergb-color))
		   (black (and (= red 0) (= green 0) (= blue 0)))
		   (ciexyy (if (not black)
			       (rs-colors:coerce-color ciexyz 'rs-colors:ciexyy-color)
			     (multiple-value-bind (x* y*)
				 (rs-colors:ciexyy-color-coordinates
				  (rs-colors:white-point srgb))
			       (rs-colors:make-ciexyy-color x* y* 0))))
		   (cieluv (if (not black)
			       (rs-colors:coerce-color ciexyz 'rs-colors:cieluv-color)
			     (rs-colors:make-cieluv-color 0 0 0)))
		   (cielab (if (not black)
			       (rs-colors:coerce-color ciexyz 'rs-colors:cielab-color)
			     (rs-colors:make-cielab-color 0 0 0)))
		   (cielch (rs-colors:coerce-color cielab 'rs-colors:cielch-color)))
	      (push (nconc (list red green blue)
			   (coordinates rgb)
			   (coordinates hsv)
			   (coordinates hsl)
			   (coordinates cmy)
			   (coordinates cmyk)
			   (coordinates ciergb)
			   (coordinates ciexyz)
			   (coordinates ciexyy)
			   (coordinates cieluv)
			   (coordinates cielab)
			   (coordinates cielch)) rows)))))
      (nreverse rows))))

(with-open-file (stream "srgb-color-cube.csv"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  (dolist (row (cons *srgb-color-cube-header* *srgb-color-cube-body*))
    (format stream "~{~A~^,~}~%" row)))

;;; srgb-color-cube.lisp ends here
