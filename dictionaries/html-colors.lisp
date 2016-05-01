;;; html-colors.lisp --- HTML basic colors.

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

(defpackage :html-color
  (:use :common-lisp
	:rs-colors
	:rs-colors-dictionary)
  (:documentation "HTML basic colors.

The list of basic color keywords is: aqua, black, blue, fuchsia, gray,
green, lime, maroon, navy, olive, purple, red, silver, teal, white,
and yellow.  The color names are case-insensitive.

See <http://www.w3.org/TR/css3-color/#html4>."))

(in-package :html-color)

(defmacro RGB (value name)
  `(define-color-name ,name
     (make-srgb-color-from-number ,value)))

(RGB #X00FFFF aqua)
(RGB #X000000 black)
(RGB #X0000FF blue)
(RGB #XFF00FF fuchsia)
(RGB #X808080 gray)
(RGB #X008000 green)
(RGB #X00FF00 lime)
(RGB #X800000 maroon)
(RGB #X000080 navy)
(RGB #X808000 olive)
(RGB #X800080 purple)
(RGB #XFF0000 red)
(RGB #XC0C0C0 silver)
(RGB #X008080 teal)
(RGB #XFFFFFF white)
(RGB #XFFFF00 yellow)

;;; html-colors.lisp ends here
