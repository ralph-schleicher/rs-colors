;;; tango-colors.lisp --- Tango desktop project colors.

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

(defpackage :tango-color
  (:use :common-lisp
	:rs-colors
	:rs-colors-dictionary)
  (:documentation "Tango desktop project colors."))

(in-package :tango-color)

(defmacro RGB (value name &rest aliases)
  `(define-color-names (,name ,@aliases)
     (make-srgb-color-from-number ,value)))

(RGB #X000000 black)
(RGB #XFFFFFF white)

(RGB #XFCE94F yellow1 butter1)
(RGB #XEDD400 yellow2 butter2)
(RGB #XC4A000 yellow3 butter3)

(RGB #XFCAF3E orange1)
(RGB #XF57900 orange2)
(RGB #XCE5C00 orange3)

(RGB #XE9B96E brown1 chocolate1)
(RGB #XC17D11 brown2 chocolate2)
(RGB #X8F5902 brown3 chocolate3)

(RGB #X8AE234 green1 chameleon1)
(RGB #X73D216 green2 chameleon2)
(RGB #X4E9A06 green3 chameleon3)

(RGB #X729FCF blue1 skyblue1)
(RGB #X3465A4 blue2 skyblue2)
(RGB #X204A87 blue3 skyblue3)

(RGB #XAD7FA8 purple1 plum1)
(RGB #X75507B purple2 plum2)
(RGB #X5C3566 purple3 plum3)

(RGB #XEF2929 red1 scarletred1)
(RGB #XCC0000 red2 scarletred2)
(RGB #XA40000 red3 scarletred3)

(RGB #XEEEEEC gray1 grey1 aluminium1)
(RGB #XD3D7CF gray2 grey2 aluminium2)
(RGB #XBABDB6 gray3 grey3 aluminium3)
(RGB #X888A85 gray4 grey4 aluminium4)
(RGB #X555753 gray5 grey5 aluminium5)
(RGB #X2E3436 gray6 grey6 aluminium6)

(export 'yellow)
(defun yellow (&optional (brightness :normal))
  "Return shades of yellow.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     yellow1)
    (:normal
     yellow2)
    (:dark
     yellow3)))

(export 'orange)
(defun orange (&optional (brightness :normal))
  "Return shades of orange.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     orange1)
    (:normal
     orange2)
    (:dark
     orange3)))

(export 'brown)
(defun brown (&optional (brightness :normal))
  "Return shades of brown.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     brown1)
    (:normal
     brown2)
    (:dark
     brown3)))

(export 'green)
(defun green (&optional (brightness :normal))
  "Return shades of green.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     green1)
    (:normal
     green2)
    (:dark
     green3)))

(export 'blue)
(defun blue (&optional (brightness :normal))
  "Return shades of blue.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     blue1)
    (:normal
     blue2)
    (:dark
     blue3)))

(export 'purple)
(defun purple (&optional (brightness :normal))
  "Return shades of purple.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     purple1)
    (:normal
     purple2)
    (:dark
     purple3)))

(export 'red)
(defun red (&optional (brightness :normal))
  "Return shades of red.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     red1)
    (:normal
     red2)
    (:dark
     red3)))

(export 'light-gray)
(defun light-gray (&optional (brightness :normal))
  "Return shades of light gray.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     gray1)
    (:normal
     gray2)
    (:dark
     gray3)))

(export 'dark-gray)
(defun dark-gray (&optional (brightness :normal))
  "Return shades of dark gray.

Optional argument BRIGHTNESS is either :light, :normal, or :dark.
 Default is to return the normal color.

Value is an RGB color object."
  (ecase brightness
    (:light
     gray4)
    (:normal
     gray5)
    (:dark
     gray6)))

;;; tango-colors.lisp ends here
