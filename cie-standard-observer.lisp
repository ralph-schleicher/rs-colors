;;; cie-standard-observer.lisp --- CIE color matching functions.

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

(in-package :rs-colors)

(export 'cie-1931-standard-observer)
(defconst cie-1931-standard-observer
  '((380 0.001368 0.000039 0.006450)
    (385 0.002236 0.000064 0.010550)
    (390 0.004243 0.000120 0.020050)
    (395 0.007650 0.000217 0.036210)
    (400 0.014310 0.000396 0.067850)
    (405 0.023190 0.000640 0.110200)
    (410 0.043510 0.001210 0.207400)
    (415 0.077630 0.002180 0.371300)
    (420 0.134380 0.004000 0.645600)
    (425 0.214770 0.007300 1.039050)
    (430 0.283900 0.011600 1.385600)
    (435 0.328500 0.016840 1.622960)
    (440 0.348280 0.023000 1.747060)
    (445 0.348060 0.029800 1.782600)
    (450 0.336200 0.038000 1.772110)
    (455 0.318700 0.048000 1.744100)
    (460 0.290800 0.060000 1.669200)
    (465 0.251100 0.073900 1.528100)
    (470 0.195360 0.090980 1.287640)
    (475 0.142100 0.112600 1.041900)
    (480 0.095640 0.139020 0.812950)
    (485 0.057950 0.169300 0.616200)
    (490 0.032010 0.208020 0.465180)
    (495 0.014700 0.258600 0.353300)
    (500 0.004900 0.323000 0.272000)
    (505 0.002400 0.407300 0.212300)
    (510 0.009300 0.503000 0.158200)
    (515 0.029100 0.608200 0.111700)
    (520 0.063270 0.710000 0.078250)
    (525 0.109600 0.793200 0.057250)
    (530 0.165500 0.862000 0.042160)
    (535 0.225750 0.914850 0.029840)
    (540 0.290400 0.954000 0.020300)
    (545 0.359700 0.980300 0.013400)
    (550 0.433450 0.994950 0.008750)
    (555 0.512050 1.000000 0.005750)
    (560 0.594500 0.995000 0.003900)
    (565 0.678400 0.978600 0.002750)
    (570 0.762100 0.952000 0.002100)
    (575 0.842500 0.915400 0.001800)
    (580 0.916300 0.870000 0.001650)
    (585 0.978600 0.816300 0.001400)
    (590 1.026300 0.757000 0.001100)
    (595 1.056700 0.694900 0.001000)
    (600 1.062200 0.631000 0.000800)
    (605 1.045600 0.566800 0.000600)
    (610 1.002600 0.503000 0.000340)
    (615 0.938400 0.441200 0.000240)
    (620 0.854450 0.381000 0.000190)
    (625 0.751400 0.321000 0.000100)
    (630 0.642400 0.265000 0.000050)
    (635 0.541900 0.217000 0.000030)
    (640 0.447900 0.175000 0.000020)
    (645 0.360800 0.138200 0.000010)
    (650 0.283500 0.107000 0.000000)
    (655 0.218700 0.081600 0.000000)
    (660 0.164900 0.061000 0.000000)
    (665 0.121200 0.044580 0.000000)
    (670 0.087400 0.032000 0.000000)
    (675 0.063600 0.023200 0.000000)
    (680 0.046770 0.017000 0.000000)
    (685 0.032900 0.011920 0.000000)
    (690 0.022700 0.008210 0.000000)
    (695 0.015840 0.005723 0.000000)
    (700 0.011359 0.004102 0.000000)
    (705 0.008111 0.002929 0.000000)
    (710 0.005790 0.002091 0.000000)
    (715 0.004109 0.001484 0.000000)
    (720 0.002899 0.001047 0.000000)
    (725 0.002049 0.000740 0.000000)
    (730 0.001440 0.000520 0.000000)
    (735 0.001000 0.000361 0.000000)
    (740 0.000690 0.000249 0.000000)
    (745 0.000476 0.000172 0.000000)
    (750 0.000332 0.000120 0.000000)
    (755 0.000235 0.000085 0.000000)
    (760 0.000166 0.000060 0.000000)
    (765 0.000117 0.000042 0.000000)
    (770 0.000083 0.000030 0.000000)
    (775 0.000059 0.000021 0.000000)
    (780 0.000042 0.000015 0.000000))
  "CIE 1931 standard observer.

Value are the color matching functions x(λ), y(λ), and z(λ).
The wavelength λ ranges from 380 nm to 780 nm in steps of 5 nm.

See the ‘*color-matching-functions*’ parameter for more details.")

(export 'cie-1964-standard-observer)
(defconst cie-1964-standard-observer
  '((380 0.000160 0.000017 0.000705)
    (385 0.000662 0.000072 0.002928)
    (390 0.002362 0.000253 0.010482)
    (395 0.007242 0.000769 0.032344)
    (400 0.019110 0.002004 0.086011)
    (405 0.043400 0.004509 0.197120)
    (410 0.084736 0.008756 0.389366)
    (415 0.140638 0.014456 0.656760)
    (420 0.204492 0.021391 0.972542)
    (425 0.264737 0.029497 1.282500)
    (430 0.314679 0.038676 1.553480)
    (435 0.357719 0.049602 1.798500)
    (440 0.383734 0.062077 1.967280)
    (445 0.386726 0.074704 2.027300)
    (450 0.370702 0.089456 1.994800)
    (455 0.342957 0.106256 1.900700)
    (460 0.302273 0.128201 1.745370)
    (465 0.254085 0.152761 1.554900)
    (470 0.195618 0.185190 1.317560)
    (475 0.132349 0.219940 1.030200)
    (480 0.080507 0.253589 0.772125)
    (485 0.041072 0.297665 0.570060)
    (490 0.016172 0.339133 0.415254)
    (495 0.005132 0.395379 0.302356)
    (500 0.003816 0.460777 0.218502)
    (505 0.015444 0.531360 0.159249)
    (510 0.037465 0.606741 0.112044)
    (515 0.071358 0.685660 0.082248)
    (520 0.117749 0.761757 0.060709)
    (525 0.172953 0.823330 0.043050)
    (530 0.236491 0.875211 0.030451)
    (535 0.304213 0.923810 0.020584)
    (540 0.376772 0.961988 0.013676)
    (545 0.451584 0.982200 0.007918)
    (550 0.529826 0.991761 0.003988)
    (555 0.616053 0.999110 0.001091)
    (560 0.705224 0.997340 0.000000)
    (565 0.793832 0.982380 0.000000)
    (570 0.878655 0.955552 0.000000)
    (575 0.951162 0.915175 0.000000)
    (580 1.014160 0.868934 0.000000)
    (585 1.074300 0.825623 0.000000)
    (590 1.118520 0.777405 0.000000)
    (595 1.134300 0.720353 0.000000)
    (600 1.123990 0.658341 0.000000)
    (605 1.089100 0.593878 0.000000)
    (610 1.030480 0.527963 0.000000)
    (615 0.950740 0.461834 0.000000)
    (620 0.856297 0.398057 0.000000)
    (625 0.754930 0.339554 0.000000)
    (630 0.647467 0.283493 0.000000)
    (635 0.535110 0.228254 0.000000)
    (640 0.431567 0.179828 0.000000)
    (645 0.343690 0.140211 0.000000)
    (650 0.268329 0.107633 0.000000)
    (655 0.204300 0.081187 0.000000)
    (660 0.152568 0.060281 0.000000)
    (665 0.112210 0.044096 0.000000)
    (670 0.081261 0.031800 0.000000)
    (675 0.057930 0.022602 0.000000)
    (680 0.040851 0.015905 0.000000)
    (685 0.028623 0.011130 0.000000)
    (690 0.019941 0.007749 0.000000)
    (695 0.013842 0.005375 0.000000)
    (700 0.009577 0.003718 0.000000)
    (705 0.006605 0.002565 0.000000)
    (710 0.004553 0.001768 0.000000)
    (715 0.003145 0.001222 0.000000)
    (720 0.002175 0.000846 0.000000)
    (725 0.001506 0.000586 0.000000)
    (730 0.001045 0.000407 0.000000)
    (735 0.000727 0.000284 0.000000)
    (740 0.000508 0.000199 0.000000)
    (745 0.000356 0.000140 0.000000)
    (750 0.000251 0.000098 0.000000)
    (755 0.000178 0.000070 0.000000)
    (760 0.000126 0.000050 0.000000)
    (765 0.000090 0.000036 0.000000)
    (770 0.000065 0.000025 0.000000)
    (775 0.000046 0.000018 0.000000)
    (780 0.000033 0.000013 0.000000))
  "CIE 1964 standard observer.

Value are the color matching functions x(λ), y(λ), and z(λ).
The wavelength λ ranges from 380 nm to 780 nm in steps of 5 nm.

See the ‘*color-matching-functions*’ parameter for more details.")

(defvar *color-matching-functions* cie-1931-standard-observer
  "A list of three color matching functions.
Default is the CIE 1931 standard observer.

Value is an alist of cons cells of the form ‘(L . (X Y Z))’.
Key L is the wavelength λ in nanometer and values X, Y, and Z
are the discrete values of the color matching functions x(λ),
y(λ), and z(λ) respectively.  The list elements have to be
sorted in strictly increasing order of the wavelength.

Value can also be a list of three functions, i.e. ‘(X Y Z)’,
where X, Y, and Z are the color matching functions x(λ), y(λ),
and z(λ) respectively.

Use the ‘color-matching-functions’ function to evaluate the color
matching functions.")
(proclaim '(type (cons (or cons function)) *color-matching-functions*))

(defun color-matching-functions (wavelength)
  "Evaluate the color matching functions.

The three color matching functions x(λ), y(λ), and z(λ) are
defined by the ‘*color-matching-functions*’ special variable.

Argument WAVELENGTH is the wavelength λ in nanometer.

Values are the values of the color matching functions for the
wavelength λ."
  (let ((object (first *color-matching-functions*)))
    (cond ((consp object)
	   (iter (with l = wavelength)
		 (for p2 :in *color-matching-functions*)
		 (for p1 :previous p2)
		 ;; Check for exact match at grid point.
		 (for l2 = (first p2))
		 (when (= l l2)
		   (leave (values-list (rest p2))))
		 ;; Find enclosing grid points.
		 (when (> l l2)
		   (next-iteration))
		 (when (null p1)
		   (finish))
		 ;; Interpolate.
		 (for l1 = (first p1))
		 ;; So L1 < L < L2.
		 (for x1 = (second p1))
		 (for x2 = (second p2))
		 (for y1 = (third p1))
		 (for y2 = (third p2))
		 (for z1 = (fourth p1))
		 (for z2 = (fourth p2))
		 (for r = (/ (- l l1) (- l2 l1)))
		 (leave (values (+ x1 (* r (- x2 x1)))
				(+ y1 (* r (- y2 y1)))
				(+ z1 (* r (- z2 z1)))))
		 (else
		  ;; Signal domain error.
		  (error 'arithmetic-error
			 :operands (list (car (first *color-matching-functions*)) l
					 (caar (last *color-matching-functions*)))
			 :operation '<=))))
	  ((functionp object)
	   (values (funcall (first  *color-matching-functions*) wavelength)
		   (funcall (second *color-matching-functions*) wavelength)
		   (funcall (third  *color-matching-functions*) wavelength)))
	  (t
	   (error 'type-error
		  :datum *color-matching-functions*
		  :expected-type '(cons (or cons function)))))))

(export 'cie-1931-second-radiation-constant)
(defconst cie-1931-second-radiation-constant 1.435L-2
  "Second radiation constant.

Value used by the CIE when defining the CIE 1931 color space.")

(export '*second-radiation-constant*)
(defvar *second-radiation-constant* codata-2018-second-radiation-constant
  "Second radiation constant.")
(proclaim '(type (real (0)) *second-radiation-constant*))

(export 'cie-xy-chromaticity-of-light)
(defun cie-xy-chromaticity-of-light (wavelength)
  "Calculate the chromaticity coordinates of a single color of light.
These chromaticity coordinates define the border of the chromaticity
space in a chromaticity diagram.

Argument WAVELENGTH is the wavelength λ of the light in nanometer.

Return values are the CIE xyY color space chromaticity coordinates x
and y as multiple values.

The three color matching functions x(λ), y(λ), and z(λ) are defined
by the ‘*color-matching-functions*’ special variable."
  (check-type wavelength (real (0)))
  (multiple-value-bind (x y z)
      (color-matching-functions wavelength)
    (let ((sum (coerce (+ x y z) 'long-float)))
      (values (coerce (/ x sum) 'single-float)
	      (coerce (/ y sum) 'single-float)))))

(export 'cie-xy-chromaticity-of-black-body)
(defun cie-xy-chromaticity-of-black-body (temperature &key (from 380) (to 780) (by 5))
  "Calculate the chromaticity coordinates of a Planckian radiator.
These chromaticity coordinates define the Planckian locus in a
chromaticity diagram.

Argument TEMPERATURE is the temperature of the black body in kelvin.
Keyword arguments FROM, TO, and BY define the boundaries and step size
 for the wavelength λ of the light in nanometer.

Return values are the CIE xyY color space chromaticity coordinates x
and y as multiple values.

The three color matching functions x(λ), y(λ), and z(λ) are defined
by the ‘*color-matching-functions*’ special variable.  The second
radiation constant is defined by the ‘*second-radiation-constant*’
special variable."
  (check-type temperature (real (0)))
  (labels ((kbn+ (sum offs term)
	     "Kahan-Babuška-Neumaier summation."
	     (let ((tem (+ sum term)))
	       ;; Return sum and offset (running compensation).
	       (values tem (+ offs (if (< (abs sum) (abs term))
				       (+ (- term tem) sum)
				     (+ (- sum tem) term)))))))
    (let (;; First radiation constant.  Value is not relevant.
	  ;; With simple summation, I used it to normalize the
	  ;; summands.  With compensated summation, any value
	  ;; is sufficient.  However, I decided to truncate the
	  ;; chromaticity coordinates to single-precision so that
	  ;; the accuracy of the summation should have no effect.
	  (c1 1L-30)
	  ;; Second radiation constant.
	  (c2 *second-radiation-constant*)
	  ;; Tristimulus values.
	  (x 0L0) (xo 0L0)
	  (y 0L0) (yo 0L0)
	  (z 0L0) (zo 0L0))
      (iter (for wavelength :from from :to to :by by)
	    ;; Wavelength in meter.
	    (for l = (* wavelength 1L-9))
	    ;; Spectral radiant exitance.
	    (for m = (/ c1 (expt l 5) (- (exp (/ c2 l temperature)) 1)))
	    ;; Color matching functions x(λ), y(λ), and z(λ).
	    (for (values xm ym zm) = (color-matching-functions wavelength))
	    ;; Integrate.
	    (multiple-value-setq (x xo) (kbn+ x xo (* m xm)))
	    (multiple-value-setq (y yo) (kbn+ y yo (* m ym)))
	    (multiple-value-setq (z zo) (kbn+ z zo (* m zm))))
      ;; Final sums.
      (setf x (+ x xo)
	    y (+ y yo)
	    z (+ z zo))
      ;; Chromaticity coordinates.
      (let ((sum (+ x y z)))
	(values (coerce (/ x sum) 'single-float)
		(coerce (/ y sum) 'single-float))))))

;;; cie-standard-observer.lisp ends here
