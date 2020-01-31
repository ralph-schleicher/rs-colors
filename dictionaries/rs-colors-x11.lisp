;;; rs-colors-x11.lisp --- X11 color names.

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

(defpackage :rs-colors-x11
  (:nicknames :x11-color)
  (:use :common-lisp
	:rs-colors
	:rs-colors-internal)
  (:shadow #:tan)
  (:documentation "X11 color names."))

(in-package :rs-colors-x11)

(defmacro RGB (r g b name &rest aliases)
  `(define-color-names (,name ,@aliases)
     (make-srgb-color ,r ,g ,b :byte-size 8)
     ,(format nil "X11 color ‘RGB:~(~2,'0X/~2,'0X/~2,'0X~)’." r g b)))

(RGB 255 250 250 snow)
(RGB 248 248 255 ghost-white
		 ghostwhite)
(RGB 245 245 245 white-smoke
		 whitesmoke)
(RGB 220 220 220 gainsboro)
(RGB 255 250 240 floral-white
		 floralwhite)
(RGB 253 245 230 old-lace
		 oldlace)
(RGB 250 240 230 linen)
(RGB 250 235 215 antique-white
		 antiquewhite)
(RGB 255 239 213 papaya-whip
		 papayawhip)
(RGB 255 235 205 blanched-almond
		 blanchedalmond)
(RGB 255 228 196 bisque)
(RGB 255 218 185 peach-puff
		 peachpuff)
(RGB 255 222 173 navajo-white
		 navajowhite)
(RGB 255 228 181 moccasin)
(RGB 255 248 220 cornsilk)
(RGB 255 255 240 ivory)
(RGB 255 250 205 lemon-chiffon
		 lemonchiffon)
(RGB 255 245 238 seashell)
(RGB 240 255 240 honeydew)
(RGB 245 255 250 mint-cream
		 mintcream)
(RGB 240 255 255 azure)
(RGB 240 248 255 alice-blue
		 aliceblue)
(RGB 230 230 250 lavender)
(RGB 255 240 245 lavender-blush
		 lavenderblush)
(RGB 255 228 225 misty-rose
		 mistyrose)
(RGB 255 255 255 white)
(RGB   0   0   0 black)
(RGB  47  79  79 dark-slate-gray
		 darkslategray
		 dark-slate-grey
		 darkslategrey)
(RGB 105 105 105 dim-gray
		 dimgray
		 dim-grey
		 dimgrey)
(RGB 112 128 144 slate-gray
		 slategray
		 slate-grey
		 slategrey)
(RGB 119 136 153 light-slate-gray
		 lightslategray
		 light-slate-grey
		 lightslategrey)
(RGB 190 190 190 gray
		 grey)
(RGB 211 211 211 light-grey
		 lightgrey
		 light-gray
		 lightgray)
(RGB  25  25 112 midnight-blue
		 midnightblue)
(RGB   0   0 128 navy)
(RGB   0   0 128 navy-blue
		 navyblue)
(RGB 100 149 237 cornflower-blue
		 cornflowerblue)
(RGB  72  61 139 dark-slate-blue
		 darkslateblue)
(RGB 106  90 205 slate-blue
		 slateblue)
(RGB 123 104 238 medium-slate-blue
		 mediumslateblue)
(RGB 132 112 255 light-slate-blue
		 lightslateblue)
(RGB   0   0 205 medium-blue
		 mediumblue)
(RGB  65 105 225 royal-blue
		 royalblue)
(RGB   0   0 255 blue)
(RGB  30 144 255 dodger-blue
		 dodgerblue)
(RGB   0 191 255 deep-sky-blue
		 deepskyblue)
(RGB 135 206 235 sky-blue
		 skyblue)
(RGB 135 206 250 light-sky-blue
		 lightskyblue)
(RGB  70 130 180 steel-blue
		 steelblue)
(RGB 176 196 222 light-steel-blue
		 lightsteelblue)
(RGB 173 216 230 light-blue
		 lightblue)
(RGB 176 224 230 powder-blue
		 powderblue)
(RGB 175 238 238 pale-turquoise
		 paleturquoise)
(RGB   0 206 209 dark-turquoise
		 darkturquoise)
(RGB  72 209 204 medium-turquoise
		 mediumturquoise)
(RGB  64 224 208 turquoise)
(RGB   0 255 255 cyan)
(RGB 224 255 255 light-cyan
		 lightcyan)
(RGB  95 158 160 cadet-blue
		 cadetblue)
(RGB 102 205 170 medium-aquamarine
		 mediumaquamarine)
(RGB 127 255 212 aquamarine)
(RGB   0 100   0 dark-green
		 darkgreen)
(RGB  85 107  47 dark-olive-green
		 darkolivegreen)
(RGB 143 188 143 dark-sea-green
		 darkseagreen)
(RGB  46 139  87 sea-green
		 seagreen)
(RGB  60 179 113 medium-sea-green
		 mediumseagreen)
(RGB  32 178 170 light-sea-green
		 lightseagreen)
(RGB 152 251 152 pale-green
		 palegreen)
(RGB   0 255 127 spring-green
		 springgreen)
(RGB 124 252   0 lawn-green
		 lawngreen)
(RGB   0 255   0 green)
(RGB 127 255   0 chartreuse)
(RGB   0 250 154 medium-spring-green
		 mediumspringgreen)
(RGB 173 255  47 green-yellow
		 greenyellow)
(RGB  50 205  50 lime-green
		 limegreen)
(RGB 154 205  50 yellow-green
		 yellowgreen)
(RGB  34 139  34 forest-green
		 forestgreen)
(RGB 107 142  35 olive-drab
		 olivedrab)
(RGB 189 183 107 dark-khaki
		 darkkhaki)
(RGB 240 230 140 khaki)
(RGB 238 232 170 pale-goldenrod
		 palegoldenrod)
(RGB 250 250 210 light-goldenrod-yellow
		 lightgoldenrodyellow)
(RGB 255 255 224 light-yellow
		 lightyellow)
(RGB 255 255   0 yellow)
(RGB 255 215   0 gold)
(RGB 238 221 130 light-goldenrod
		 lightgoldenrod)
(RGB 218 165  32 goldenrod)
(RGB 184 134  11 dark-goldenrod
		 darkgoldenrod)
(RGB 188 143 143 rosy-brown
		 rosybrown)
(RGB 205  92  92 indian-red
		 indianred)
(RGB 139  69  19 saddle-brown
		 saddlebrown)
(RGB 160  82  45 sienna)
(RGB 205 133  63 peru)
(RGB 222 184 135 burlywood)
(RGB 245 245 220 beige)
(RGB 245 222 179 wheat)
(RGB 244 164  96 sandy-brown
		 sandybrown)
(RGB 210 180 140 tan)
(RGB 210 105  30 chocolate)
(RGB 178  34  34 firebrick)
(RGB 165  42  42 brown)
(RGB 233 150 122 dark-salmon
		 darksalmon)
(RGB 250 128 114 salmon)
(RGB 255 160 122 light-salmon
		 lightsalmon)
(RGB 255 165   0 orange)
(RGB 255 140   0 dark-orange
		 darkorange)
(RGB 255 127  80 coral)
(RGB 240 128 128 light-coral
		 lightcoral)
(RGB 255  99  71 tomato)
(RGB 255  69   0 orange-red
		 orangered)
(RGB 255   0   0 red)
(RGB 255 105 180 hot-pink
		 hotpink)
(RGB 255  20 147 deep-pink
		 deeppink)
(RGB 255 192 203 pink)
(RGB 255 182 193 light-pink
		 lightpink)
(RGB 219 112 147 pale-violet-red
		 palevioletred)
(RGB 176  48  96 maroon)
(RGB 199  21 133 medium-violet-red
		 mediumvioletred)
(RGB 208  32 144 violet-red
		 violetred)
(RGB 255   0 255 magenta)
(RGB 238 130 238 violet)
(RGB 221 160 221 plum)
(RGB 218 112 214 orchid)
(RGB 186  85 211 medium-orchid
		 mediumorchid)
(RGB 153  50 204 dark-orchid
		 darkorchid)
(RGB 148   0 211 dark-violet
		 darkviolet)
(RGB 138  43 226 blue-violet
		 blueviolet)
(RGB 160  32 240 purple)
(RGB 147 112 219 medium-purple
		 mediumpurple)
(RGB 216 191 216 thistle)

(RGB 255 250 250 snow1)
(RGB 238 233 233 snow2)
(RGB 205 201 201 snow3)
(RGB 139 137 137 snow4)
(RGB 255 245 238 seashell1)
(RGB 238 229 222 seashell2)
(RGB 205 197 191 seashell3)
(RGB 139 134 130 seashell4)
(RGB 255 239 219 antiquewhite1)
(RGB 238 223 204 antiquewhite2)
(RGB 205 192 176 antiquewhite3)
(RGB 139 131 120 antiquewhite4)
(RGB 255 228 196 bisque1)
(RGB 238 213 183 bisque2)
(RGB 205 183 158 bisque3)
(RGB 139 125 107 bisque4)
(RGB 255 218 185 peachpuff1)
(RGB 238 203 173 peachpuff2)
(RGB 205 175 149 peachpuff3)
(RGB 139 119 101 peachpuff4)
(RGB 255 222 173 navajowhite1)
(RGB 238 207 161 navajowhite2)
(RGB 205 179 139 navajowhite3)
(RGB 139 121  94 navajowhite4)
(RGB 255 250 205 lemonchiffon1)
(RGB 238 233 191 lemonchiffon2)
(RGB 205 201 165 lemonchiffon3)
(RGB 139 137 112 lemonchiffon4)
(RGB 255 248 220 cornsilk1)
(RGB 238 232 205 cornsilk2)
(RGB 205 200 177 cornsilk3)
(RGB 139 136 120 cornsilk4)
(RGB 255 255 240 ivory1)
(RGB 238 238 224 ivory2)
(RGB 205 205 193 ivory3)
(RGB 139 139 131 ivory4)
(RGB 240 255 240 honeydew1)
(RGB 224 238 224 honeydew2)
(RGB 193 205 193 honeydew3)
(RGB 131 139 131 honeydew4)
(RGB 255 240 245 lavenderblush1)
(RGB 238 224 229 lavenderblush2)
(RGB 205 193 197 lavenderblush3)
(RGB 139 131 134 lavenderblush4)
(RGB 255 228 225 mistyrose1)
(RGB 238 213 210 mistyrose2)
(RGB 205 183 181 mistyrose3)
(RGB 139 125 123 mistyrose4)
(RGB 240 255 255 azure1)
(RGB 224 238 238 azure2)
(RGB 193 205 205 azure3)
(RGB 131 139 139 azure4)
(RGB 131 111 255 slateblue1)
(RGB 122 103 238 slateblue2)
(RGB 105  89 205 slateblue3)
(RGB  71  60 139 slateblue4)
(RGB  72 118 255 royalblue1)
(RGB  67 110 238 royalblue2)
(RGB  58  95 205 royalblue3)
(RGB  39  64 139 royalblue4)
(RGB   0   0 255 blue1)
(RGB   0   0 238 blue2)
(RGB   0   0 205 blue3)
(RGB   0   0 139 blue4)
(RGB  30 144 255 dodgerblue1)
(RGB  28 134 238 dodgerblue2)
(RGB  24 116 205 dodgerblue3)
(RGB  16  78 139 dodgerblue4)
(RGB  99 184 255 steelblue1)
(RGB  92 172 238 steelblue2)
(RGB  79 148 205 steelblue3)
(RGB  54 100 139 steelblue4)
(RGB   0 191 255 deepskyblue1)
(RGB   0 178 238 deepskyblue2)
(RGB   0 154 205 deepskyblue3)
(RGB   0 104 139 deepskyblue4)
(RGB 135 206 255 skyblue1)
(RGB 126 192 238 skyblue2)
(RGB 108 166 205 skyblue3)
(RGB  74 112 139 skyblue4)
(RGB 176 226 255 lightskyblue1)
(RGB 164 211 238 lightskyblue2)
(RGB 141 182 205 lightskyblue3)
(RGB  96 123 139 lightskyblue4)
(RGB 198 226 255 slategray1)
(RGB 185 211 238 slategray2)
(RGB 159 182 205 slategray3)
(RGB 108 123 139 slategray4)
(RGB 202 225 255 lightsteelblue1)
(RGB 188 210 238 lightsteelblue2)
(RGB 162 181 205 lightsteelblue3)
(RGB 110 123 139 lightsteelblue4)
(RGB 191 239 255 lightblue1)
(RGB 178 223 238 lightblue2)
(RGB 154 192 205 lightblue3)
(RGB 104 131 139 lightblue4)
(RGB 224 255 255 lightcyan1)
(RGB 209 238 238 lightcyan2)
(RGB 180 205 205 lightcyan3)
(RGB 122 139 139 lightcyan4)
(RGB 187 255 255 paleturquoise1)
(RGB 174 238 238 paleturquoise2)
(RGB 150 205 205 paleturquoise3)
(RGB 102 139 139 paleturquoise4)
(RGB 152 245 255 cadetblue1)
(RGB 142 229 238 cadetblue2)
(RGB 122 197 205 cadetblue3)
(RGB  83 134 139 cadetblue4)
(RGB   0 245 255 turquoise1)
(RGB   0 229 238 turquoise2)
(RGB   0 197 205 turquoise3)
(RGB   0 134 139 turquoise4)
(RGB   0 255 255 cyan1)
(RGB   0 238 238 cyan2)
(RGB   0 205 205 cyan3)
(RGB   0 139 139 cyan4)
(RGB 151 255 255 darkslategray1)
(RGB 141 238 238 darkslategray2)
(RGB 121 205 205 darkslategray3)
(RGB  82 139 139 darkslategray4)
(RGB 127 255 212 aquamarine1)
(RGB 118 238 198 aquamarine2)
(RGB 102 205 170 aquamarine3)
(RGB  69 139 116 aquamarine4)
(RGB 193 255 193 darkseagreen1)
(RGB 180 238 180 darkseagreen2)
(RGB 155 205 155 darkseagreen3)
(RGB 105 139 105 darkseagreen4)
(RGB  84 255 159 seagreen1)
(RGB  78 238 148 seagreen2)
(RGB  67 205 128 seagreen3)
(RGB  46 139  87 seagreen4)
(RGB 154 255 154 palegreen1)
(RGB 144 238 144 palegreen2)
(RGB 124 205 124 palegreen3)
(RGB  84 139  84 palegreen4)
(RGB   0 255 127 springgreen1)
(RGB   0 238 118 springgreen2)
(RGB   0 205 102 springgreen3)
(RGB   0 139  69 springgreen4)
(RGB   0 255   0 green1)
(RGB   0 238   0 green2)
(RGB   0 205   0 green3)
(RGB   0 139   0 green4)
(RGB 127 255   0 chartreuse1)
(RGB 118 238   0 chartreuse2)
(RGB 102 205   0 chartreuse3)
(RGB  69 139   0 chartreuse4)
(RGB 192 255  62 olivedrab1)
(RGB 179 238  58 olivedrab2)
(RGB 154 205  50 olivedrab3)
(RGB 105 139  34 olivedrab4)
(RGB 202 255 112 darkolivegreen1)
(RGB 188 238 104 darkolivegreen2)
(RGB 162 205  90 darkolivegreen3)
(RGB 110 139  61 darkolivegreen4)
(RGB 255 246 143 khaki1)
(RGB 238 230 133 khaki2)
(RGB 205 198 115 khaki3)
(RGB 139 134  78 khaki4)
(RGB 255 236 139 lightgoldenrod1)
(RGB 238 220 130 lightgoldenrod2)
(RGB 205 190 112 lightgoldenrod3)
(RGB 139 129  76 lightgoldenrod4)
(RGB 255 255 224 lightyellow1)
(RGB 238 238 209 lightyellow2)
(RGB 205 205 180 lightyellow3)
(RGB 139 139 122 lightyellow4)
(RGB 255 255   0 yellow1)
(RGB 238 238   0 yellow2)
(RGB 205 205   0 yellow3)
(RGB 139 139   0 yellow4)
(RGB 255 215   0 gold1)
(RGB 238 201   0 gold2)
(RGB 205 173   0 gold3)
(RGB 139 117   0 gold4)
(RGB 255 193  37 goldenrod1)
(RGB 238 180  34 goldenrod2)
(RGB 205 155  29 goldenrod3)
(RGB 139 105  20 goldenrod4)
(RGB 255 185  15 darkgoldenrod1)
(RGB 238 173  14 darkgoldenrod2)
(RGB 205 149  12 darkgoldenrod3)
(RGB 139 101   8 darkgoldenrod4)
(RGB 255 193 193 rosybrown1)
(RGB 238 180 180 rosybrown2)
(RGB 205 155 155 rosybrown3)
(RGB 139 105 105 rosybrown4)
(RGB 255 106 106 indianred1)
(RGB 238  99  99 indianred2)
(RGB 205  85  85 indianred3)
(RGB 139  58  58 indianred4)
(RGB 255 130  71 sienna1)
(RGB 238 121  66 sienna2)
(RGB 205 104  57 sienna3)
(RGB 139  71  38 sienna4)
(RGB 255 211 155 burlywood1)
(RGB 238 197 145 burlywood2)
(RGB 205 170 125 burlywood3)
(RGB 139 115  85 burlywood4)
(RGB 255 231 186 wheat1)
(RGB 238 216 174 wheat2)
(RGB 205 186 150 wheat3)
(RGB 139 126 102 wheat4)
(RGB 255 165  79 tan1)
(RGB 238 154  73 tan2)
(RGB 205 133  63 tan3)
(RGB 139  90  43 tan4)
(RGB 255 127  36 chocolate1)
(RGB 238 118  33 chocolate2)
(RGB 205 102  29 chocolate3)
(RGB 139  69  19 chocolate4)
(RGB 255  48  48 firebrick1)
(RGB 238  44  44 firebrick2)
(RGB 205  38  38 firebrick3)
(RGB 139  26  26 firebrick4)
(RGB 255  64  64 brown1)
(RGB 238  59  59 brown2)
(RGB 205  51  51 brown3)
(RGB 139  35  35 brown4)
(RGB 255 140 105 salmon1)
(RGB 238 130  98 salmon2)
(RGB 205 112  84 salmon3)
(RGB 139  76  57 salmon4)
(RGB 255 160 122 lightsalmon1)
(RGB 238 149 114 lightsalmon2)
(RGB 205 129  98 lightsalmon3)
(RGB 139  87  66 lightsalmon4)
(RGB 255 165   0 orange1)
(RGB 238 154   0 orange2)
(RGB 205 133   0 orange3)
(RGB 139  90   0 orange4)
(RGB 255 127   0 darkorange1)
(RGB 238 118   0 darkorange2)
(RGB 205 102   0 darkorange3)
(RGB 139  69   0 darkorange4)
(RGB 255 114  86 coral1)
(RGB 238 106  80 coral2)
(RGB 205  91  69 coral3)
(RGB 139  62  47 coral4)
(RGB 255  99  71 tomato1)
(RGB 238  92  66 tomato2)
(RGB 205  79  57 tomato3)
(RGB 139  54  38 tomato4)
(RGB 255  69   0 orangered1)
(RGB 238  64   0 orangered2)
(RGB 205  55   0 orangered3)
(RGB 139  37   0 orangered4)
(RGB 255   0   0 red1)
(RGB 238   0   0 red2)
(RGB 205   0   0 red3)
(RGB 139   0   0 red4)
(RGB 215   7  81 debianred)
(RGB 255  20 147 deeppink1)
(RGB 238  18 137 deeppink2)
(RGB 205  16 118 deeppink3)
(RGB 139  10  80 deeppink4)
(RGB 255 110 180 hotpink1)
(RGB 238 106 167 hotpink2)
(RGB 205  96 144 hotpink3)
(RGB 139  58  98 hotpink4)
(RGB 255 181 197 pink1)
(RGB 238 169 184 pink2)
(RGB 205 145 158 pink3)
(RGB 139  99 108 pink4)
(RGB 255 174 185 lightpink1)
(RGB 238 162 173 lightpink2)
(RGB 205 140 149 lightpink3)
(RGB 139  95 101 lightpink4)
(RGB 255 130 171 palevioletred1)
(RGB 238 121 159 palevioletred2)
(RGB 205 104 137 palevioletred3)
(RGB 139  71  93 palevioletred4)
(RGB 255  52 179 maroon1)
(RGB 238  48 167 maroon2)
(RGB 205  41 144 maroon3)
(RGB 139  28  98 maroon4)
(RGB 255  62 150 violetred1)
(RGB 238  58 140 violetred2)
(RGB 205  50 120 violetred3)
(RGB 139  34  82 violetred4)
(RGB 255   0 255 magenta1)
(RGB 238   0 238 magenta2)
(RGB 205   0 205 magenta3)
(RGB 139   0 139 magenta4)
(RGB 255 131 250 orchid1)
(RGB 238 122 233 orchid2)
(RGB 205 105 201 orchid3)
(RGB 139  71 137 orchid4)
(RGB 255 187 255 plum1)
(RGB 238 174 238 plum2)
(RGB 205 150 205 plum3)
(RGB 139 102 139 plum4)
(RGB 224 102 255 mediumorchid1)
(RGB 209  95 238 mediumorchid2)
(RGB 180  82 205 mediumorchid3)
(RGB 122  55 139 mediumorchid4)
(RGB 191  62 255 darkorchid1)
(RGB 178  58 238 darkorchid2)
(RGB 154  50 205 darkorchid3)
(RGB 104  34 139 darkorchid4)
(RGB 155  48 255 purple1)
(RGB 145  44 238 purple2)
(RGB 125  38 205 purple3)
(RGB  85  26 139 purple4)
(RGB 171 130 255 mediumpurple1)
(RGB 159 121 238 mediumpurple2)
(RGB 137 104 205 mediumpurple3)
(RGB  93  71 139 mediumpurple4)
(RGB 255 225 255 thistle1)
(RGB 238 210 238 thistle2)
(RGB 205 181 205 thistle3)
(RGB 139 123 139 thistle4)

(RGB   0   0   0 gray0
		 grey0)
(RGB   3   3   3 gray1
		 grey1)
(RGB   5   5   5 gray2
		 grey2)
(RGB   8   8   8 gray3
		 grey3)
(RGB  10  10  10 gray4
		 grey4)
(RGB  13  13  13 gray5
		 grey5)
(RGB  15  15  15 gray6
		 grey6)
(RGB  18  18  18 gray7
		 grey7)
(RGB  20  20  20 gray8
		 grey8)
(RGB  23  23  23 gray9
		 grey9)
(RGB  26  26  26 gray10
		 grey10)
(RGB  28  28  28 gray11
		 grey11)
(RGB  31  31  31 gray12
		 grey12)
(RGB  33  33  33 gray13
		 grey13)
(RGB  36  36  36 gray14
		 grey14)
(RGB  38  38  38 gray15
		 grey15)
(RGB  41  41  41 gray16
		 grey16)
(RGB  43  43  43 gray17
		 grey17)
(RGB  46  46  46 gray18
		 grey18)
(RGB  48  48  48 gray19
		 grey19)
(RGB  51  51  51 gray20
		 grey20)
(RGB  54  54  54 gray21
		 grey21)
(RGB  56  56  56 gray22
		 grey22)
(RGB  59  59  59 gray23
		 grey23)
(RGB  61  61  61 gray24
		 grey24)
(RGB  64  64  64 gray25
		 grey25)
(RGB  66  66  66 gray26
		 grey26)
(RGB  69  69  69 gray27
		 grey27)
(RGB  71  71  71 gray28
		 grey28)
(RGB  74  74  74 gray29
		 grey29)
(RGB  77  77  77 gray30
		 grey30)
(RGB  79  79  79 gray31
		 grey31)
(RGB  82  82  82 gray32
		 grey32)
(RGB  84  84  84 gray33
		 grey33)
(RGB  87  87  87 gray34
		 grey34)
(RGB  89  89  89 gray35
		 grey35)
(RGB  92  92  92 gray36
		 grey36)
(RGB  94  94  94 gray37
		 grey37)
(RGB  97  97  97 gray38
		 grey38)
(RGB  99  99  99 gray39
		 grey39)
(RGB 102 102 102 gray40
		 grey40)
(RGB 105 105 105 gray41
		 grey41)
(RGB 107 107 107 gray42
		 grey42)
(RGB 110 110 110 gray43
		 grey43)
(RGB 112 112 112 gray44
		 grey44)
(RGB 115 115 115 gray45
		 grey45)
(RGB 117 117 117 gray46
		 grey46)
(RGB 120 120 120 gray47
		 grey47)
(RGB 122 122 122 gray48
		 grey48)
(RGB 125 125 125 gray49
		 grey49)
(RGB 127 127 127 gray50
		 grey50)
(RGB 130 130 130 gray51
		 grey51)
(RGB 133 133 133 gray52
		 grey52)
(RGB 135 135 135 gray53
		 grey53)
(RGB 138 138 138 gray54
		 grey54)
(RGB 140 140 140 gray55
		 grey55)
(RGB 143 143 143 gray56
		 grey56)
(RGB 145 145 145 gray57
		 grey57)
(RGB 148 148 148 gray58
		 grey58)
(RGB 150 150 150 gray59
		 grey59)
(RGB 153 153 153 gray60
		 grey60)
(RGB 156 156 156 gray61
		 grey61)
(RGB 158 158 158 gray62
		 grey62)
(RGB 161 161 161 gray63
		 grey63)
(RGB 163 163 163 gray64
		 grey64)
(RGB 166 166 166 gray65
		 grey65)
(RGB 168 168 168 gray66
		 grey66)
(RGB 171 171 171 gray67
		 grey67)
(RGB 173 173 173 gray68
		 grey68)
(RGB 176 176 176 gray69
		 grey69)
(RGB 179 179 179 gray70
		 grey70)
(RGB 181 181 181 gray71
		 grey71)
(RGB 184 184 184 gray72
		 grey72)
(RGB 186 186 186 gray73
		 grey73)
(RGB 189 189 189 gray74
		 grey74)
(RGB 191 191 191 gray75
		 grey75)
(RGB 194 194 194 gray76
		 grey76)
(RGB 196 196 196 gray77
		 grey77)
(RGB 199 199 199 gray78
		 grey78)
(RGB 201 201 201 gray79
		 grey79)
(RGB 204 204 204 gray80
		 grey80)
(RGB 207 207 207 gray81
		 grey81)
(RGB 209 209 209 gray82
		 grey82)
(RGB 212 212 212 gray83
		 grey83)
(RGB 214 214 214 gray84
		 grey84)
(RGB 217 217 217 gray85
		 grey85)
(RGB 219 219 219 gray86
		 grey86)
(RGB 222 222 222 gray87
		 grey87)
(RGB 224 224 224 gray88
		 grey88)
(RGB 227 227 227 gray89
		 grey89)
(RGB 229 229 229 gray90
		 grey90)
(RGB 232 232 232 gray91
		 grey91)
(RGB 235 235 235 gray92
		 grey92)
(RGB 237 237 237 gray93
		 grey93)
(RGB 240 240 240 gray94
		 grey94)
(RGB 242 242 242 gray95
		 grey95)
(RGB 245 245 245 gray96
		 grey96)
(RGB 247 247 247 gray97
		 grey97)
(RGB 250 250 250 gray98
		 grey98)
(RGB 252 252 252 gray99
		 grey99)
(RGB 255 255 255 gray100
		 grey100)

(RGB 169 169 169 dark-grey
		 darkgrey
		 dark-gray
		 darkgray)
(RGB   0   0 139 dark-blue
		 darkblue)
(RGB   0 139 139 dark-cyan
		 darkcyan)
(RGB 139   0 139 dark-magenta
		 darkmagenta)
(RGB 139   0   0 dark-red
		 darkred)
(RGB 144 238 144 light-green
		 lightgreen)

;;; rs-colors-x11.lisp ends here
