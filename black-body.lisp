;;; black-body.lisp --- Planck's law.

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

(let (;; Speed of light in vacuum.
      (c 299792458)
      ;; Planck constant.
      (h 6.62607015L-34)
      ;; Boltzmann constant.
      (k 1.380649L-23))

  (export 'codata-2018-first-radiation-constant-for-spectral-radiance)
  (defconst codata-2018-first-radiation-constant-for-spectral-radiance (* 2 h (expt c 2))
    "First radiation constant for spectral radiance.

2018 CODATA recommended value.")

  (export 'codata-2018-first-radiation-constant)
  (defconst codata-2018-first-radiation-constant (* 2 pi h (expt c 2))
    "First radiation constant.

2018 CODATA recommended value.")

  (export 'codata-2018-second-radiation-constant)
  (defconst codata-2018-second-radiation-constant (/ (* h c) k)
    "Second radiation constant.

2018 CODATA recommended value."))

(export 'black-body-spectral-radiant-exitance)
(defun black-body-spectral-radiant-exitance (wavelength temperature)
  "Calculate the spectral radiant exitance of a black body.

First argument WAVELENGTH is the wavelength of the light in meter.
Second argument TEMPERATURE is the temperature of the black body
 in kelvin.

Return value is the spectral radiant exitance of a black body
around the given wavelength."
  (check-type wavelength (real (0)))
  (check-type temperature (real (0)))
  (symbol-macrolet ((c1 codata-2018-first-radiation-constant)
		    (c2 codata-2018-second-radiation-constant))
    (/ c1 (expt wavelength 5) (- (exp (/ c2 wavelength temperature)) 1))))

(export 'black-body-spectral-radiance)
(defun black-body-spectral-radiance (wavelength temperature)
  "Calculate the spectral radiance of a black body.

First argument WAVELENGTH is the wavelength of the light in meter.
Second argument TEMPERATURE is the temperature of the black body
 in kelvin.

Return value is the spectral radiance of a black body around the
given wavelength."
  (check-type wavelength (real (0)))
  (check-type temperature (real (0)))
  (symbol-macrolet ((c1l codata-2018-first-radiation-constant-for-spectral-radiance)
		    (c2 codata-2018-second-radiation-constant))
    (/ c1l (expt wavelength 5) (- (exp (/ c2 wavelength temperature)) 1))))

;;; black-body.lisp ends here
