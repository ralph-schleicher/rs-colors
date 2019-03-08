;;; color-difference.lisp --- color difference formulas.

;; Copyright (C) 2018 Ralph Schleicher

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

(export 'cie76)
(defun cie76 (first-color second-color)
  "Calculate the CIE76 color difference between two colors.

Value is the Euclidean distance between the two colors in the
CIE L*a*b* color space.  The CIE76 color difference is symmetric,
i.e. CIE76(a,b) = CIE76(b,a)."
  (let (L1 a1 b1 L2 a2 b2)
    (multiple-value-setq (L1 a1 b1)
      (cie-lab-color-coordinates first-color))
    (multiple-value-setq (L2 a2 b2)
      (cie-lab-color-coordinates second-color))
    (hypot3 (- L2 L1) (- a2 a1) (- b2 b1))))

(export 'cie94)
(defun cie94 (reference other &optional textile (lightness (if textile 2 1)) (chroma 1) (hue 1))
  "Calculate the CIE94 color difference between two colors.

First argument REFERENCE is the reference color.
Second argument OTHER is the other color.
If optional third argument TEXTILE is non-null, use parameters
 for calculating the color difference for textiles.  Default is
 to calculate the color difference for graphic arts.
Optional fourth to sixth argument LIGHTNESS, CHROMA, and HUE are
 the weighting factors for differences in lightness, chroma, and
 hue respectively.  Higher value means less weight.  Default is
 one for all weighting factors (if TEXTILE is true, the default
 for LIGHTNESS is two).

The CIE94 color difference is asymmetric, i.e. CIE94(a,b) ≠ CIE94(b,a)."
  (check-type lightness alexandria:positive-real)
  (check-type chroma alexandria:positive-real)
  (check-type hue alexandria:positive-real)
  ;; Get L*a*b* color space coordinates.
  (let (L1 a1 b1 L2 a2 b2)
    (multiple-value-setq (L1 a1 b1)
      (cie-lab-color-coordinates reference))
    (multiple-value-setq (L2 a2 b2)
      (cie-lab-color-coordinates other))
    ;; Differences in the L*C*h color space.
    (let* ((C1 (hypot a1 b1))
	   (C2 (hypot a2 b2))
	   (dL (- L2 L1))
	   (dC (- C2 C1))
	   (dH (sqrt (abs (* 2 (- (* C1 C2) (* a1 a2) (* b1 b2)))))))
      ;; CIE94 color difference.
      (multiple-value-bind (K1 K2)
	  (if (not textile)
	      (values 0.045D0 0.015D0)
	    (values 0.048D0 0.014D0))
	(hypot3 (/ dL lightness)
		(/ dC chroma (1+ (* K1 C1)))
		(/ dH hue (1+ (* K2 C1))))))))

;;; color-difference.lisp ends here
