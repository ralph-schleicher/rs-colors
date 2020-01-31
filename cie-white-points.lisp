;;; cie-white-points.lisp --- white points of CIE standard illuminants.

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

;; See <http://en.wikipedia.org/wiki/Standard_illuminant>.
(macrolet ((define-white-point (name (x y) &optional doc)
	     `(define-color-name ,name (make-ciexyy-color ,x ,y 1)
		,@(when doc (list doc)))))
  (define-white-point cie-1931-a   (0.44757 0.40745) "White point of CIE standard illuminant A given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-b   (0.34842 0.35161) "White point of CIE standard illuminant B given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-c   (0.31006 0.31616) "White point of CIE standard illuminant C given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-d50 (0.34567 0.35850) "White point of CIE standard illuminant D50 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-d55 (0.33242 0.34743) "White point of CIE standard illuminant D55 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-d65 (0.31271 0.32902) "White point of CIE standard illuminant D65 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-d75 (0.29902 0.31485) "White point of CIE standard illuminant D75 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-e   (1/3     1/3    ) "White point of CIE standard illuminant E given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f1  (0.31310 0.33727) "White point of CIE standard illuminant F1 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f2  (0.37208 0.37529) "White point of CIE standard illuminant F2 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f3  (0.40910 0.39430) "White point of CIE standard illuminant F3 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f4  (0.44018 0.40329) "White point of CIE standard illuminant F4 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f5  (0.31379 0.34531) "White point of CIE standard illuminant F5 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f6  (0.37790 0.38835) "White point of CIE standard illuminant F6 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f7  (0.31292 0.32933) "White point of CIE standard illuminant F7 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f8  (0.34588 0.35875) "White point of CIE standard illuminant F8 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f9  (0.37417 0.37281) "White point of CIE standard illuminant F9 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f10 (0.34609 0.35986) "White point of CIE standard illuminant F10 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f11 (0.38052 0.37713) "White point of CIE standard illuminant F11 given for the CIE 1931 standard observer.")
  (define-white-point cie-1931-f12 (0.43695 0.40441) "White point of CIE standard illuminant F12 given for the CIE 1931 standard observer.")
  (define-white-point cie-1964-a   (0.45117 0.40594) "White point of CIE standard illuminant A given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-b   (0.34980 0.35270) "White point of CIE standard illuminant B given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-c   (0.31039 0.31905) "White point of CIE standard illuminant C given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-d50 (0.34773 0.35952) "White point of CIE standard illuminant D50 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-d55 (0.33411 0.34877) "White point of CIE standard illuminant D55 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-d65 (0.31382 0.33100) "White point of CIE standard illuminant D65 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-d75 (0.29968 0.31740) "White point of CIE standard illuminant D75 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-e   (1/3     1/3    ) "White point of CIE standard illuminant E given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f1  (0.31811 0.33559) "White point of CIE standard illuminant F1 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f2  (0.37925 0.36733) "White point of CIE standard illuminant F2 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f3  (0.41761 0.38324) "White point of CIE standard illuminant F3 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f4  (0.44920 0.39074) "White point of CIE standard illuminant F4 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f5  (0.31975 0.34246) "White point of CIE standard illuminant F5 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f6  (0.38660 0.37847) "White point of CIE standard illuminant F6 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f7  (0.31569 0.32960) "White point of CIE standard illuminant F7 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f8  (0.34902 0.35939) "White point of CIE standard illuminant F8 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f9  (0.37829 0.37045) "White point of CIE standard illuminant F9 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f10 (0.35090 0.35444) "White point of CIE standard illuminant F10 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f11 (0.38541 0.37123) "White point of CIE standard illuminant F11 given for the CIE 1964 standard observer.")
  (define-white-point cie-1964-f12 (0.44256 0.39717) "White point of CIE standard illuminant F12 given for the CIE 1964 standard observer.")
  (values))

;;; cie-white-points.lisp ends here
