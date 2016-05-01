;;; tests.lisp --- test procedure.

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

(defpackage :rs-colors-tests
  (:use :common-lisp
	:iterate
	:lisp-unit
	:rs-colors))

(in-package :rs-colors-tests)

(define-test srgb-regression-test
  (assert-false
   (iter (for value :from 0 :to (1- (expt 2 24)))
	 (for color = (make-srgb-color-from-number value))
	 (multiple-value-bind (ro go bo)
	     (color-coordinates color)
	   (change-class color 'cie-xyz-color)
	   (change-class color 'srgb-color)
	   (multiple-value-bind (r g b)
	       (color-coordinates color)
	     (let ((eps #.(/ (expt 2 31))))
	       (unless (and (< (abs (- r ro)) eps)
			    (< (abs (- g go)) eps)
			    (< (abs (- b bo)) eps))
		 (collect value))))))))

(define-test adobe-rgb-regression-test
  (assert-false
   (iter (for value :from 0 :to (1- (expt 2 24)))
	 (for color = (make-adobe-rgb-color-from-number value))
	 (multiple-value-bind (ro go bo)
	     (color-coordinates color)
	   (change-class color 'cie-xyz-color)
	   (change-class color 'adobe-rgb-color)
	   (multiple-value-bind (r g b)
	       (color-coordinates color)
	     (let ((eps #.(/ (expt 2 15))))
	       (unless (and (< (abs (- r ro)) eps)
			    (< (abs (- g go)) eps)
			    (< (abs (- b bo)) eps))
		 (collect value))))))))

(run-tests)

;;; tests.lisp ends here
