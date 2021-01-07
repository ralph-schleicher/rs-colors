;;; rs-colors.lisp --- a color data type for Common Lisp.

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

(in-package :common-lisp-user)

(defpackage :rs-colors
  (:use :common-lisp
	:iterate
	:rs-colors-internal)
  (:import-from :alexandria
		#:clamp)
  (:import-from :read-number
		#:read-integer
		#:read-float)
  (:export ;; types
	   #:color-object
	   #:colorp
	   #:color-coordinates
	   #:white-point
	   #:copy-color
	   #:coerce-color
	   #:rgb-color-object
	   #:hsv-color-object
	   #:hsl-color-object
	   #:cmy-color-object
	   #:cmyk-color-object
	   #:generic-color-object
	   #:normalize-color
	   #:absolute-color
	   ;; generic-rgb
	   #:generic-rgb-color
	   #:make-generic-rgb-color
	   #:make-generic-rgb-color-from-number
	   #:generic-hsv-color
	   #:make-generic-hsv-color
	   #:generic-hsl-color
	   #:make-generic-hsl-color
	   #:generic-rgb-color-coordinates
	   #:generic-hsv-color-coordinates
	   #:generic-hsl-color-coordinates
	   ;; generic-cmy
	   #:generic-cmy-color
	   #:make-generic-cmy-color
	   #:make-generic-cmy-color-from-number
	   #:generic-cmy-color-coordinates
	   ;; generic-cmyk
	   #:generic-cmyk-color
	   #:make-generic-cmyk-color
	   #:make-generic-cmyk-color-from-number
	   #:generic-cmyk-color-coordinates
	   ;; ciergb
	   #:ciergb-color
	   #:make-ciergb-color
	   #:ciergb-color-coordinates
	   ;; ciexyz
	   #:ciexyz-color
	   #:make-ciexyz-color
	   #:ciexyz-color-coordinates
	   ;; ciexyy
	   #:ciexyy-color
	   #:make-ciexyy-color
	   #:ciexyy-color-coordinates
	   ;; cieluv
	   #:*cieluv-default-white-point*
	   #:cieluv-color
	   #:make-cieluv-color
	   #:cieluv-color-coordinates
	   ;; cielab
	   #:*cielab-default-white-point*
	   #:cielab-color
	   #:make-cielab-color
	   #:cielab-color-coordinates
	   ;; cielch
	   #:*cielch-default-white-point*
	   #:cielch-color
	   #:make-cielch-color
	   #:cielch-color-coordinates
	   ;; srgb
	   #:srgb-color
	   #:make-srgb-color
	   #:make-srgb-color-from-number
	   #:srgb-white-point
	   #:srgb-color-coordinates
	   ;; adobe-rgb
	   #:adobe-rgb-color
	   #:make-adobe-rgb-color
	   #:make-adobe-rgb-color-from-number
	   #:adobe-rgb-white-point
	   #:adobe-rgb-color-coordinates
	   ;; wide-gamut-rgb
	   #:wide-gamut-rgb-color
	   #:make-wide-gamut-rgb-color
	   #:make-wide-gamut-rgb-color-from-number
	   #:wide-gamut-rgb-white-point
	   #:wide-gamut-rgb-color-coordinates
	   ;; cie-white-points
	   #:cie-1931-white-point-a
	   #:cie-1931-white-point-b
	   #:cie-1931-white-point-c
	   #:cie-1931-white-point-d50
	   #:cie-1931-white-point-d55
	   #:cie-1931-white-point-d65
	   #:cie-1931-white-point-d75
	   #:cie-1931-white-point-e
	   #:cie-1931-white-point-f1
	   #:cie-1931-white-point-f2
	   #:cie-1931-white-point-f3
	   #:cie-1931-white-point-f4
	   #:cie-1931-white-point-f5
	   #:cie-1931-white-point-f6
	   #:cie-1931-white-point-f7
	   #:cie-1931-white-point-f8
	   #:cie-1931-white-point-f9
	   #:cie-1931-white-point-f10
	   #:cie-1931-white-point-f11
	   #:cie-1931-white-point-f12
	   #:cie-1964-white-point-a
	   #:cie-1964-white-point-b
	   #:cie-1964-white-point-c
	   #:cie-1964-white-point-d50
	   #:cie-1964-white-point-d55
	   #:cie-1964-white-point-d65
	   #:cie-1964-white-point-d75
	   #:cie-1964-white-point-e
	   #:cie-1964-white-point-f1
	   #:cie-1964-white-point-f2
	   #:cie-1964-white-point-f3
	   #:cie-1964-white-point-f4
	   #:cie-1964-white-point-f5
	   #:cie-1964-white-point-f6
	   #:cie-1964-white-point-f7
	   #:cie-1964-white-point-f8
	   #:cie-1964-white-point-f9
	   #:cie-1964-white-point-f10
	   #:cie-1964-white-point-f11
	   #:cie-1964-white-point-f12
	   ;; io
	   #:define-color-printer
	   #:define-color-reader
	   #:print-color-xcms-ciergb
	   #:color-formatter-xcms-ciergb
	   #:read-color-xcms-ciergb
	   #:print-color-xcms-ciexyz
	   #:color-formatter-xcms-ciexyz
	   #:read-color-xcms-ciexyz
	   #:print-color-xcms-ciexyy
	   #:color-formatter-xcms-ciexyy
	   #:read-color-xcms-ciexyy
	   #:print-color-xcms-cieluv
	   #:color-formatter-xcms-cieluv
	   #:read-color-xcms-cieluv
	   #:print-color-xcms-cielab
	   #:color-formatter-xcms-cielab
	   #:read-color-xcms-cielab
	   #:print-color-xcms-cielch
	   #:color-formatter-xcms-cielch
	   #:read-color-xcms-cielch
	   #:print-color-xcms-rgbi
	   #:color-formatter-xcms-rgbi
	   #:read-color-xcms-rgbi
	   #:print-color-xcms-rgb
	   #:color-formatter-xcms-rgb
	   #:read-color-xcms-rgb
	   #:read-color-xcms
	   #:print-color-html
	   #:color-formatter-html
	   #:read-color-html
	   #:print-color-css3-rgb
	   #:color-formatter-css3-rgb
	   #:read-color-css3-rgb
	   #:print-color-css3-hsl
	   #:color-formatter-css3-hsl
	   #:read-color-css3-hsl
	   #:read-color-css3
	   ;; color-matching-functions
	   #:cie-1931-standard-observer
	   #:cie-1964-standard-observer
	   #:*color-matching-functions*
	   #:color-matching-functions
	   #:cie-1931-second-radiation-constant
	   #:*second-radiation-constant*
	   #:cie-xy-chromaticity-of-light
	   #:cie-xy-chromaticity-of-black-body
	   ;; black-body
	   #:codata-2018-first-radiation-constant-for-spectral-radiance
	   #:codata-2018-first-radiation-constant
	   #:codata-2018-second-radiation-constant
	   #:black-body-spectral-radiant-exitance
	   #:black-body-spectral-radiance
	   ;; color-difference
	   #:cie76
	   #:cie94)
  (:documentation "A color data type for Common Lisp."))

;;; rs-colors.lisp ends here
