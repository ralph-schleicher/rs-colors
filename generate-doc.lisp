;;; generate-doc.lisp --- generate documentation.

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

(ql:quickload :rs-doc) ;private

(let ((dir (ensure-directories-exist
	    (merge-pathnames
	     (make-pathname :directory '(:relative "doc" "api"))
	     (uiop:getcwd)))))
  (mapc (lambda (package)
	  (ql:quickload package)
	  (let ((name (string-downcase (string package))))
	    (rs-doc:generate-doc
	     :package package
	     :generic-functions t
	     :methods nil
	     :output (merge-pathnames (make-pathname :name name :type "html") dir)
	     :output-format :html)
	    (rs-doc:generate-doc
	     :package package
	     :generic-functions t
	     :methods nil
	     :output (merge-pathnames (make-pathname :name name :type "txt") dir)
	     :output-format :text)))
	'(:rs-colors
	  :rs-colors-html
	  :rs-colors-svg
	  :rs-colors-x11
	  :rs-colors-tango
	  :rs-colors-material-io
	  :rs-colors-ral
	  :rs-colors-ral-design)))

;;; generate-doc.lisp ends here
