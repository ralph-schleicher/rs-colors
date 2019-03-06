;;; material-io-colors.lisp --- material design color palette.

;; Copyright (C) 2017 Ralph Schleicher

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

(defpackage :material-io-color
  (:use :common-lisp
	:rs-colors
	:rs-colors-internal)
  (:documentation "Material design color palette.

See <https://material.io/guidelines/style/color.html>."))

(in-package :material-io-color)

(defmacro RGB (value name &rest aliases)
  `(define-color-names (,name ,@aliases)
     (make-srgb-color-from-number ,value)))

(RGB #XFFEBEE red-50)
(RGB #XFFCDD2 red-100)
(RGB #XEF9A9A red-200)
(RGB #XE57373 red-300)
(RGB #XEF5350 red-400)
(RGB #XF44336 red-500 red)
(RGB #XE53935 red-600)
(RGB #XD32F2F red-700)
(RGB #XC62828 red-800)
(RGB #XB71C1C red-900)
(RGB #XFF8A80 red-100*)
(RGB #XFF5252 red-200*)
(RGB #XFF1744 red-400*)
(RGB #XD50000 red-700*)

(RGB #XFCE4EC pink-50)
(RGB #XF8BBD0 pink-100)
(RGB #XF48FB1 pink-200)
(RGB #XF06292 pink-300)
(RGB #XEC407A pink-400)
(RGB #XE91E63 pink-500 pink)
(RGB #XD81B60 pink-600)
(RGB #XC2185B pink-700)
(RGB #XAD1457 pink-800)
(RGB #X880E4F pink-900)
(RGB #XFF80AB pink-100*)
(RGB #XFF4081 pink-200*)
(RGB #XF50057 pink-400*)
(RGB #XC51162 pink-700*)

(RGB #XF3E5F5 purple-50)
(RGB #XE1BEE7 purple-100)
(RGB #XCE93D8 purple-200)
(RGB #XBA68C8 purple-300)
(RGB #XAB47BC purple-400)
(RGB #X9C27B0 purple-500 purple)
(RGB #X8E24AA purple-600)
(RGB #X7B1FA2 purple-700)
(RGB #X6A1B9A purple-800)
(RGB #X4A148C purple-900)
(RGB #XEA80FC purple-100*)
(RGB #XE040FB purple-200*)
(RGB #XD500F9 purple-400*)
(RGB #XAA00FF purple-700*)

(RGB #XEDE7F6 deep-purple-50)
(RGB #XD1C4E9 deep-purple-100)
(RGB #XB39DDB deep-purple-200)
(RGB #X9575CD deep-purple-300)
(RGB #X7E57C2 deep-purple-400)
(RGB #X673AB7 deep-purple-500 deep-purple)
(RGB #X5E35B1 deep-purple-600)
(RGB #X512DA8 deep-purple-700)
(RGB #X4527A0 deep-purple-800)
(RGB #X311B92 deep-purple-900)
(RGB #XB388FF deep-purple-100*)
(RGB #X7C4DFF deep-purple-200*)
(RGB #X651FFF deep-purple-400*)
(RGB #X6200EA deep-purple-700*)

(RGB #XE8EAF6 indigo-50)
(RGB #XC5CAE9 indigo-100)
(RGB #X9FA8DA indigo-200)
(RGB #X7986CB indigo-300)
(RGB #X5C6BC0 indigo-400)
(RGB #X3F51B5 indigo-500 indigo)
(RGB #X3949AB indigo-600)
(RGB #X303F9F indigo-700)
(RGB #X283593 indigo-800)
(RGB #X1A237E indigo-900)
(RGB #X8C9EFF indigo-100*)
(RGB #X536DFE indigo-200*)
(RGB #X3D5AFE indigo-400*)
(RGB #X304FFE indigo-700*)

(RGB #XE3F2FD blue-50)
(RGB #XBBDEFB blue-100)
(RGB #X90CAF9 blue-200)
(RGB #X64B5F6 blue-300)
(RGB #X42A5F5 blue-400)
(RGB #X2196F3 blue-500 blue)
(RGB #X1E88E5 blue-600)
(RGB #X1976D2 blue-700)
(RGB #X1565C0 blue-800)
(RGB #X0D47A1 blue-900)
(RGB #X82B1FF blue-100*)
(RGB #X448AFF blue-200*)
(RGB #X2979FF blue-400*)
(RGB #X2962FF blue-700*)

(RGB #XE1F5FE light-blue-50)
(RGB #XB3E5FC light-blue-100)
(RGB #X81D4FA light-blue-200)
(RGB #X4FC3F7 light-blue-300)
(RGB #X29B6F6 light-blue-400)
(RGB #X03A9F4 light-blue-500 light-blue)
(RGB #X039BE5 light-blue-600)
(RGB #X0288D1 light-blue-700)
(RGB #X0277BD light-blue-800)
(RGB #X01579B light-blue-900)
(RGB #X80D8FF light-blue-100*)
(RGB #X40C4FF light-blue-200*)
(RGB #X00B0FF light-blue-400*)
(RGB #X0091EA light-blue-700*)

(RGB #XE0F7FA cyan-50)
(RGB #XB2EBF2 cyan-100)
(RGB #X80DEEA cyan-200)
(RGB #X4DD0E1 cyan-300)
(RGB #X26C6DA cyan-400)
(RGB #X00BCD4 cyan-500 cyan)
(RGB #X00ACC1 cyan-600)
(RGB #X0097A7 cyan-700)
(RGB #X00838F cyan-800)
(RGB #X006064 cyan-900)
(RGB #X84FFFF cyan-100*)
(RGB #X18FFFF cyan-200*)
(RGB #X00E5FF cyan-400*)
(RGB #X00B8D4 cyan-700*)

(RGB #XE0F2F1 teal-50)
(RGB #XB2DFDB teal-100)
(RGB #X80CBC4 teal-200)
(RGB #X4DB6AC teal-300)
(RGB #X26A69A teal-400)
(RGB #X009688 teal-500 teal)
(RGB #X00897B teal-600)
(RGB #X00796B teal-700)
(RGB #X00695C teal-800)
(RGB #X004D40 teal-900)
(RGB #XA7FFEB teal-100*)
(RGB #X64FFDA teal-200*)
(RGB #X1DE9B6 teal-400*)
(RGB #X00BFA5 teal-700*)

(RGB #XE8F5E9 green-50)
(RGB #XC8E6C9 green-100)
(RGB #XA5D6A7 green-200)
(RGB #X81C784 green-300)
(RGB #X66BB6A green-400)
(RGB #X4CAF50 green-500 green)
(RGB #X43A047 green-600)
(RGB #X388E3C green-700)
(RGB #X2E7D32 green-800)
(RGB #X1B5E20 green-900)
(RGB #XB9F6CA green-100*)
(RGB #X69F0AE green-200*)
(RGB #X00E676 green-400*)
(RGB #X00C853 green-700*)

(RGB #XF1F8E9 light-green-50)
(RGB #XDCEDC8 light-green-100)
(RGB #XC5E1A5 light-green-200)
(RGB #XAED581 light-green-300)
(RGB #X9CCC65 light-green-400)
(RGB #X8BC34A light-green-500 light-green)
(RGB #X7CB342 light-green-600)
(RGB #X689F38 light-green-700)
(RGB #X558B2F light-green-800)
(RGB #X33691E light-green-900)
(RGB #XCCFF90 light-green-100*)
(RGB #XB2FF59 light-green-200*)
(RGB #X76FF03 light-green-400*)
(RGB #X64DD17 light-green-700*)

(RGB #XF9FBE7 lime-50)
(RGB #XF0F4C3 lime-100)
(RGB #XE6EE9C lime-200)
(RGB #XDCE775 lime-300)
(RGB #XD4E157 lime-400)
(RGB #XCDDC39 lime-500 lime)
(RGB #XC0CA33 lime-600)
(RGB #XAFB42B lime-700)
(RGB #X9E9D24 lime-800)
(RGB #X827717 lime-900)
(RGB #XF4FF81 lime-100*)
(RGB #XEEFF41 lime-200*)
(RGB #XC6FF00 lime-400*)
(RGB #XAEEA00 lime-700*)

(RGB #XFFFDE7 yellow-50)
(RGB #XFFF9C4 yellow-100)
(RGB #XFFF59D yellow-200)
(RGB #XFFF176 yellow-300)
(RGB #XFFEE58 yellow-400)
(RGB #XFFEB3B yellow-500 yellow)
(RGB #XFDD835 yellow-600)
(RGB #XFBC02D yellow-700)
(RGB #XF9A825 yellow-800)
(RGB #XF57F17 yellow-900)
(RGB #XFFFF8D yellow-100*)
(RGB #XFFFF00 yellow-200*)
(RGB #XFFEA00 yellow-400*)
(RGB #XFFD600 yellow-700*)

(RGB #XFFF8E1 amber-50)
(RGB #XFFECB3 amber-100)
(RGB #XFFE082 amber-200)
(RGB #XFFD54F amber-300)
(RGB #XFFCA28 amber-400)
(RGB #XFFC107 amber-500 amber)
(RGB #XFFB300 amber-600)
(RGB #XFFA000 amber-700)
(RGB #XFF8F00 amber-800)
(RGB #XFF6F00 amber-900)
(RGB #XFFE57F amber-100*)
(RGB #XFFD740 amber-200*)
(RGB #XFFC400 amber-400*)
(RGB #XFFAB00 amber-700*)

(RGB #XFFF3E0 orange-50)
(RGB #XFFE0B2 orange-100)
(RGB #XFFCC80 orange-200)
(RGB #XFFB74D orange-300)
(RGB #XFFA726 orange-400)
(RGB #XFF9800 orange-500 orange)
(RGB #XFB8C00 orange-600)
(RGB #XF57C00 orange-700)
(RGB #XEF6C00 orange-800)
(RGB #XE65100 orange-900)
(RGB #XFFD180 orange-100*)
(RGB #XFFAB40 orange-200*)
(RGB #XFF9100 orange-400*)
(RGB #XFF6D00 orange-700*)

(RGB #XFBE9E7 deep-orange-50)
(RGB #XFFCCBC deep-orange-100)
(RGB #XFFAB91 deep-orange-200)
(RGB #XFF8A65 deep-orange-300)
(RGB #XFF7043 deep-orange-400)
(RGB #XFF5722 deep-orange-500 deep-orange)
(RGB #XF4511E deep-orange-600)
(RGB #XE64A19 deep-orange-700)
(RGB #XD84315 deep-orange-800)
(RGB #XBF360C deep-orange-900)
(RGB #XFF9E80 deep-orange-100*)
(RGB #XFF6E40 deep-orange-200*)
(RGB #XFF3D00 deep-orange-400*)
(RGB #XDD2C00 deep-orange-700*)

(RGB #XEFEBE9 brown-50)
(RGB #XD7CCC8 brown-100)
(RGB #XBCAAA4 brown-200)
(RGB #XA1887F brown-300)
(RGB #X8D6E63 brown-400)
(RGB #X795548 brown-500 brown)
(RGB #X6D4C41 brown-600)
(RGB #X5D4037 brown-700)
(RGB #X4E342E brown-800)
(RGB #X3E2723 brown-900)

(RGB #XFAFAFA grey-50)
(RGB #XF5F5F5 grey-100)
(RGB #XEEEEEE grey-200)
(RGB #XE0E0E0 grey-300)
(RGB #XBDBDBD grey-400)
(RGB #X9E9E9E grey-500 grey)
(RGB #X757575 grey-600)
(RGB #X616161 grey-700)
(RGB #X424242 grey-800)
(RGB #X212121 grey-900)

(RGB #XECEFF1 blue-grey-50)
(RGB #XCFD8DC blue-grey-100)
(RGB #XB0BEC5 blue-grey-200)
(RGB #X90A4AE blue-grey-300)
(RGB #X78909C blue-grey-400)
(RGB #X607D8B blue-grey-500 blue-grey)
(RGB #X546E7A blue-grey-600)
(RGB #X455A64 blue-grey-700)
(RGB #X37474F blue-grey-800)
(RGB #X263238 blue-grey-900)

(RGB #000000 black)
(RGB #FFFFFF white)

;;; material-io-colors.lisp ends here
