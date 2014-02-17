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

(in-package :common-lisp-user)

(asdf:defsystem :rs-colors
  :description "A color data type for Common Lisp."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "20140217.2019"
  :depends-on (:alexandria :closer-mop :lisp-unit)
  :serial t
  :components ((:file "rs-colors")
	       (:file "utilities")
	       (:file "types")
	       (:file "generic-rgb")
	       (:file "generic-cmy")
	       (:file "generic-cmyk")
	       (:file "cie-xyz")
	       (:file "cie-xyy")
	       (:file "cie-white-points")
	       (:file "cie-luv")
	       (:file "cie-rgb")
	       (:file "srgb")
	       (:file "adobe-rgb")
	       (:file "io")))

;; local variables:
;; time-stamp-time-zone: "UTC"
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: ":version\\s-+\""
;; time-stamp-end: "\""
;; end:

;;; rs-colors.asd ends here
