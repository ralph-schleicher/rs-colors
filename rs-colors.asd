;;; rs-colors.asd --- ASDF system definition.

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

(asdf:defsystem :rs-colors
  :description "A color data type for Common Lisp."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "20200124.1824"
  :depends-on (:iterate :alexandria :closer-mop :read-number :rs-colors-internal)
  :serial t
  :components ((:file "rs-colors")
	       (:file "types")
	       (:file "generic-rgb")
	       (:file "generic-cmy")
	       (:file "generic-cmyk")
	       (:file "ciexyz")
	       (:file "ciexyy")
	       (:file "cie-white-points")
	       (:file "cieluv")
	       (:file "cielab")
	       (:file "cielch")
	       (:file "ciergb")
	       (:file "srgb")
	       (:file "adobe-rgb")
	       (:file "io")
	       (:file "black-body")
	       (:file "color-matching-functions")
	       (:file "color-difference")))

;; local variables:
;; time-stamp-time-zone: "UTC"
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: ":version\\s-+\""
;; time-stamp-end: "\""
;; end:

;;; rs-colors.asd ends here
