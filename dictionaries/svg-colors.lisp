;;; svg-colors.lisp --- SVG color names.

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

(defpackage :svg-color
  (:use :common-lisp
	:rs-colors
	:rs-colors-dictionary)
  (:shadow #:tan)
  (:documentation "SVG color names.

See <http://www.w3.org/TR/css3-color/#svg-color>
and <http://www.w3.org/TR/SVG11/types.html>."))

(in-package :svg-color)

(defmacro RGB (red green blue name &rest aliases)
  `(define-color-names (,name ,@aliases)
     (make-srgb-color ,red ,green ,blue :byte-size 8)))

(RGB 240 248 255 aliceblue)
(RGB 250 235 215 antiquewhite)
(RGB 127 255 212 aquamarine)
(RGB 240 255 255 azure)
(RGB 245 245 220 beige)
(RGB 255 228 196 bisque)
(RGB   0   0   0 black)
(RGB 255 235 205 blanchedalmond)
(RGB   0   0 255 blue)
(RGB 138  43 226 blueviolet)
(RGB 165  42  42 brown)
(RGB 222 184 135 burlywood)
(RGB  95 158 160 cadetblue)
(RGB 127 255   0 chartreuse)
(RGB 210 105  30 chocolate)
(RGB 255 127  80 coral)
(RGB 100 149 237 cornflowerblue)
(RGB 255 248 220 cornsilk)
(RGB 220  20  60 crimson)
(RGB   0 255 255 cyan
		 aqua)
(RGB   0   0 139 darkblue)
(RGB   0 139 139 darkcyan)
(RGB 184 134  11 darkgoldenrod)
(RGB 169 169 169 darkgray
		 darkgrey)
(RGB   0 100   0 darkgreen)
(RGB 189 183 107 darkkhaki)
(RGB 139   0 139 darkmagenta)
(RGB  85 107  47 darkolivegreen)
(RGB 255 140   0 darkorange)
(RGB 153  50 204 darkorchid)
(RGB 139   0   0 darkred)
(RGB 233 150 122 darksalmon)
(RGB 143 188 143 darkseagreen)
(RGB  72  61 139 darkslateblue)
(RGB  47  79  79 darkslategray
		 darkslategrey)
(RGB   0 206 209 darkturquoise)
(RGB 148   0 211 darkviolet)
(RGB 255  20 147 deeppink)
(RGB  0  191 255 deepskyblue)
(RGB 105 105 105 dimgray
		 dimgrey)
(RGB  30 144 255 dodgerblue)
(RGB 178  34  34 firebrick)
(RGB 255 250 240 floralwhite)
(RGB  34 139  34 forestgreen)
(RGB 220 220 220 gainsboro)
(RGB 248 248 255 ghostwhite)
(RGB 255 215   0 gold)
(RGB 218 165  32 goldenrod)
(RGB 128 128 128 gray
		 grey)
(RGB  0  128   0 green)
(RGB 173 255  47 greenyellow)
(RGB 240 255 240 honeydew)
(RGB 255 105 180 hotpink)
(RGB 205  92  92 indianred)
(RGB  75   0 130 indigo)
(RGB 255 255 240 ivory)
(RGB 240 230 140 khaki)
(RGB 230 230 250 lavender)
(RGB 255 240 245 lavenderblush)
(RGB 124 252   0 lawngreen)
(RGB 255 250 205 lemonchiffon)
(RGB 173 216 230 lightblue)
(RGB 240 128 128 lightcoral)
(RGB 224 255 255 lightcyan)
(RGB 250 250 210 lightgoldenrodyellow)
(RGB 211 211 211 lightgray
		 lightgrey)
(RGB 144 238 144 lightgreen)
(RGB 255 182 193 lightpink)
(RGB 255 160 122 lightsalmon)
(RGB  32 178 170 lightseagreen)
(RGB 135 206 250 lightskyblue)
(RGB 119 136 153 lightslategray
		 lightslategrey)
(RGB 176 196 222 lightsteelblue)
(RGB 255 255 224 lightyellow)
(RGB   0 255   0 lime)
(RGB  50 205  50 limegreen)
(RGB 250 240 230 linen)
(RGB 255   0 255 magenta
		 fuchsia)
(RGB 128   0   0 maroon)
(RGB 102 205 170 mediumaquamarine)
(RGB   0   0 205 mediumblue)
(RGB 186  85 211 mediumorchid)
(RGB 147 112 219 mediumpurple)
(RGB  60 179 113 mediumseagreen)
(RGB 123 104 238 mediumslateblue)
(RGB   0 250 154 mediumspringgreen)
(RGB  72 209 204 mediumturquoise)
(RGB 199  21 133 mediumvioletred)
(RGB  25  25 112 midnightblue)
(RGB 245 255 250 mintcream)
(RGB 255 228 225 mistyrose)
(RGB 255 228 181 moccasin)
(RGB 255 222 173 navajowhite)
(RGB   0   0 128 navy)
(RGB 253 245 230 oldlace)
(RGB 128 128   0 olive)
(RGB 107 142  35 olivedrab)
(RGB 255 165   0 orange)
(RGB 255  69   0 orangered)
(RGB 218 112 214 orchid)
(RGB 238 232 170 palegoldenrod)
(RGB 152 251 152 palegreen)
(RGB 175 238 238 paleturquoise)
(RGB 219 112 147 palevioletred)
(RGB 255 239 213 papayawhip)
(RGB 255 218 185 peachpuff)
(RGB 205 133  63 peru)
(RGB 255 192 203 pink)
(RGB 221 160 221 plum)
(RGB 176 224 230 powderblue)
(RGB 128   0 128 purple)
(RGB 255   0   0 red)
(RGB 188 143 143 rosybrown)
(RGB  65 105 225 royalblue)
(RGB 139  69  19 saddlebrown)
(RGB 250 128 114 salmon)
(RGB 244 164  96 sandybrown)
(RGB  46 139  87 seagreen)
(RGB 255 245 238 seashell)
(RGB 160  82  45 sienna)
(RGB 192 192 192 silver)
(RGB 135 206 235 skyblue)
(RGB 106  90 205 slateblue)
(RGB 112 128 144 slategray
		 slategrey)
(RGB 255 250 250 snow)
(RGB   0 255 127 springgreen)
(RGB  70 130 180 steelblue)
(RGB 210 180 140 tan)
(RGB   0 128 128 teal)
(RGB 216 191 216 thistle)
(RGB 255 99   71 tomato)
(RGB  64 224 208 turquoise)
(RGB 238 130 238 violet)
(RGB 245 222 179 wheat)
(RGB 255 255 255 white)
(RGB 245 245 245 whitesmoke)
(RGB 255 255   0 yellow)
(RGB 154 205  50 yellowgreen)

;;; svg-colors.lisp ends here
