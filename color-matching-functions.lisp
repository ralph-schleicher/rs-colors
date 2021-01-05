;;; color-matching-functions.lisp --- color matching functions.

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

(defconst cie-1931-standard-observer
  '((380 0.001368    3.9E-5     0.00645    )
    (385 0.002236    6.4E-5     0.01055    )
    (390 0.004243    0.00012    0.02005    )
    (395 0.00765     0.000217   0.03621    )
    (400 0.01431     0.000396   0.06785    )
    (405 0.02319     0.00064    0.1102     )
    (410 0.04351     0.00121    0.2074     )
    (415 0.07763     0.00218    0.3713     )
    (420 0.13438     0.004      0.6456     )
    (425 0.21477     0.0073     1.03905    )
    (430 0.2839      0.0116     1.3856     )
    (435 0.3285      0.01684    1.62296    )
    (440 0.34828     0.023      1.74706    )
    (445 0.34806     0.0298     1.7826     )
    (450 0.3362      0.038      1.77211    )
    (451 0.3332235   0.03984569 1.768377   )
    (452 0.3300701   0.04176509 1.764171   )
    (453 0.3266549   0.04376163 1.759031   )
    (454 0.3228932   0.04583879 1.752495   )
    (455 0.3187      0.048      1.7441     )
    (456 0.3140107   0.05024722 1.733474   )
    (457 0.3088406   0.05257638 1.720598   )
    (458 0.3032252   0.05498194 1.705546   )
    (459 0.2971998   0.05745833 1.68839    )
    (460 0.2908      0.06       1.6692     )
    (461 0.2840345   0.06260464 1.647912   )
    (462 0.2768059   0.06528299 1.62391    )
    (463 0.26899     0.06804901 1.596443   )
    (464 0.2604627   0.07091669 1.564757   )
    (465 0.2511      0.0739     1.5281     )
    (466 0.2408413   0.07701245 1.48607    )
    (467 0.2298805   0.08026559 1.439663   )
    (468 0.2184751   0.08367051 1.390224   )
    (469 0.2068824   0.08723829 1.339102   )
    (470 0.19536     0.09098    1.28764    )
    (471 0.1841196   0.09490605 1.236942   )
    (472 0.1731902   0.0990241  1.187136   )
    (473 0.1625549   0.1033411  1.138104   )
    (474 0.1521971   0.1078641  1.089731   )
    (475 0.1421      0.1126     1.0419     )
    (476 0.1322525   0.1175502  0.9945442  )
    (477 0.1226658   0.1226939  0.9477929  )
    (478 0.1133568   0.1280044  0.9018245  )
    (479 0.1043426   0.1334553  0.8568173  )
    (480 0.09564     0.13902    0.81295    )
    (481 0.08727035  0.1446855  0.7703949  )
    (482 0.07927288  0.1504923  0.7293002  )
    (483 0.07169123  0.1564943  0.689808   )
    (484 0.06456905  0.1627456  0.6520606  )
    (485 0.05795     0.1693     0.6162     )
    (486 0.05186237  0.1762077  0.5823285  )
    (487 0.04627304  0.183503   0.5503884  )
    (488 0.04113352  0.1912165  0.520282   )
    (489 0.03639533  0.1993786  0.4919118  )
    (490 0.03201     0.20802    0.46518    )
    (491 0.0279372   0.217164   0.439994   )
    (492 0.02416929  0.2268059  0.4162809  )
    (493 0.02070678  0.2369338  0.3939728  )
    (494 0.01755018  0.2475357  0.3730018  )
    (495 0.0147      0.2586     0.3533     )
    (496 0.01215545  0.2701351  0.3348039  )
    (497 0.009910513 0.2822317  0.3174674  )
    (498 0.007957846 0.2950007  0.301249   )
    (499 0.006290119 0.3085531  0.2861071  )
    (500 0.0049      0.323      0.272      )
    (501 0.003784501 0.3384105  0.2588604  )
    (502 0.002958014 0.3546861  0.2465173  )
    (503 0.002439277 0.3716864  0.234774   )
    (504 0.002247026 0.3892712  0.2234338  )
    (505 0.0024      0.4073     0.2123     )
    (506 0.002919343 0.425663   0.2012187  )
    (507 0.003835831 0.444372   0.1902075  )
    (508 0.005182647 0.4634696  0.179327   )
    (509 0.006992976 0.4829981  0.1686377  )
    (510 0.0093      0.503      0.1582     )
    (511 0.01213413  0.5234905  0.1480729  )
    (512 0.01551466  0.5443762  0.1383086  )
    (513 0.01945813  0.5655367  0.1289579  )
    (514 0.02398107  0.5868515  0.1200715  )
    (515 0.0291      0.6082     0.1117     )
    (516 0.03482391  0.629452   0.1038854  )
    (517 0.0411316   0.6504376  0.09663478 )
    (518 0.04799433  0.6709772  0.08994645 )
    (519 0.05538338  0.6908912  0.08381874 )
    (520 0.06327     0.71       0.07825    )
    (521 0.07162897  0.7281721  0.07322669 )
    (522 0.08044911  0.7454688  0.06868786 )
    (523 0.08972277  0.7619994  0.06456068 )
    (524 0.09944229  0.7778733  0.06077233 )
    (525 0.1096      0.7932     0.05725    )
    (526 0.1201787   0.8080612  0.0539295  )
    (527 0.131123    0.8224282  0.05078123 )
    (528 0.1423679   0.8362446  0.0477842  )
    (529 0.1538486   0.849454   0.04491745 )
    (530 0.1655      0.862      0.04216    )
    (531 0.1772709   0.8738426  0.03949537 )
    (532 0.1891644   0.8850072  0.03692506 )
    (533 0.2011976   0.8955355  0.03445507 )
    (534 0.2133871   0.9054692  0.03209138 )
    (535 0.22575     0.91485    0.02984    )
    (536 0.2383002   0.9237123  0.02770549 )
    (537 0.2510405   0.9320609  0.02568676 )
    (538 0.2639706   0.9398934  0.02378127 )
    (539 0.2770905   0.9472073  0.02198653 )
    (540 0.2904      0.954      0.0203     )
    (541 0.3038983   0.9602711  0.01871915 )
    (542 0.3175817   0.9660276  0.01724136 )
    (543 0.331446    0.9712785  0.01586399 )
    (544 0.3454869   0.976033   0.01458442 )
    (545 0.3597      0.9803     0.0134     )
    (546 0.374083    0.9840911  0.0123076  )
    (547 0.3886418   0.9874277  0.01130206 )
    (548 0.403384    0.9903337  0.01037772 )
    (549 0.4183175   0.9928332  0.009528924)
    (550 0.43345     0.99495    0.00875    )
    (551 0.4487867   0.9967031  0.008035648)
    (552 0.4643227   0.998091   0.007381992)
    (553 0.4800502   0.9991074  0.006785511)
    (554 0.4959619   0.9997459  0.006242687)
    (555 0.51205     1.0        0.00575    )
    (556 0.5283041   0.9998611  0.005303807)
    (557 0.5447015   0.9993116  0.00489997 )
    (558 0.561217    0.9983315  0.00453423 )
    (559 0.577825    0.9969009  0.004202327)
    (560 0.5945      0.995      0.0039     )
    (561 0.6112199   0.9926157  0.003623526)
    (562 0.6279755   0.9897624  0.003371328)
    (563 0.6447613   0.9864612  0.003142367)
    (564 0.6615714   0.9827333  0.002935604)
    (565 0.6784      0.9786     0.00275    )
    (566 0.6952373   0.9740777  0.002584491)
    (567 0.7120567   0.9691637  0.002437918)
    (568 0.7288274   0.963851   0.0023091  )
    (569 0.7455188   0.9581322  0.002196855)
    (570 0.7621      0.952      0.0021     )
    (571 0.7785421   0.9454513  0.002017311)
    (572 0.7948233   0.9384994  0.001947399)
    (573 0.8109234   0.9311619  0.001888831)
    (574 0.8268223   0.9234563  0.001840175)
    (575 0.8425      0.9154     0.0018     )
    (580 0.9163      0.87       0.00165    )
    (585 0.9786      0.8163     0.0014     )
    (590 1.0263      0.757      0.0011     )
    (595 1.0567      0.6949     0.001      )
    (600 1.0622      0.631      0.0008     )
    (605 1.0456      0.5668     0.0006     )
    (610 1.0026      0.503      0.00034    )
    (615 0.9384      0.4412     0.00024    )
    (620 0.85445     0.381      0.00019    )
    (625 0.7514      0.321      0.0001     )
    (630 0.6424      0.265      5.0E-5     )
    (635 0.5419      0.217      3.0E-5     )
    (640 0.4479      0.175      2.0E-5     )
    (645 0.3608      0.1382     1.0E-5     )
    (650 0.2835      0.107      0.0        )
    (655 0.2187      0.0816     0.0        )
    (660 0.1649      0.061      0.0        )
    (665 0.1212      0.04458    0.0        )
    (670 0.0874      0.032      0.0        )
    (675 0.0636      0.0232     0.0        )
    (680 0.04677     0.017      0.0        )
    (685 0.0329      0.01192    0.0        )
    (690 0.0227      0.00821    0.0        )
    (695 0.01584     0.005723   0.0        )
    (700 0.011359    0.004102   0.0        )
    (705 0.008111    0.002929   0.0        )
    (710 0.00579     0.002091   0.0        )
    (715 0.004109    0.001484   0.0        )
    (720 0.002899    0.001047   0.0        )
    (725 0.002049    0.00074    0.0        )
    (730 0.00144     0.00052    0.0        )
    (735 0.001       0.000361   0.0        )
    (740 0.00069     0.000249   0.0        )
    (745 0.000476    0.000172   0.0        )
    (750 0.000332    0.00012    0.0        )
    (755 0.000235    8.5E-5     0.0        )
    (760 0.000166    6.0E-5     0.0        )
    (765 0.000117    4.2E-5     0.0        )
    (770 8.3E-5      3.0E-5     0.0        )
    (775 5.9E-5      2.1E-5     0.0        )
    (780 4.2E-5      1.5E-5     0.0        ))
  "CIE 1931 standard observer.

Value are the color matching functions x(λ), y(λ), and z(λ).
The wavelength λ ranges from 380 nm to 780 nm in steps of 5 nm.

See the ‘*color-matching-functions*’ parameter for more details.")

(defconst cie-1964-standard-observer
  '((380 0.00016     1.7E-5    0.000705    )
    (385 0.000662    7.2E-5    0.002928    )
    (390 0.002362    0.000253  0.010482    )
    (395 0.007242    0.000769  0.032344    )
    (400 0.01911     0.002004  0.086011    )
    (405 0.0434      0.004509  0.19712     )
    (410 0.084736    0.008756  0.389366    )
    (415 0.140638    0.014456  0.65676     )
    (420 0.204492    0.021391  0.972542    )
    (425 0.264737    0.029497  1.2825      )
    (430 0.314679    0.038676  1.55348     )
    (435 0.357719    0.049602  1.7985      )
    (440 0.383734    0.062077  1.96728     )
    (445 0.386726    0.074704  2.0273      )
    (450 0.370702    0.089456  1.9948      )
    (455 0.342957    0.106256  1.9007      )
    (460 0.302273    0.128201  1.74537     )
    (465 0.254085    0.152761  1.5549      )
    (466 0.2432475   0.1586186 1.512042    )
    (467 0.2319496   0.1648612 1.467117    )
    (468 0.2202236   0.1714178 1.419887    )
    (469 0.2081022   0.1782176 1.370114    )
    (470 0.195618    0.18519   1.31756     )
    (471 0.1828311   0.1922623 1.26219     )
    (472 0.1699124   0.1993548 1.204785    )
    (473 0.1570602   0.2063862 1.146329    )
    (474 0.1444729   0.2132751 1.087806    )
    (475 0.132349    0.21994   1.0302      )
    (476 0.120846    0.2263518 0.9743491   )
    (477 0.1099575   0.2326904 0.9205078   )
    (478 0.09963629  0.239188  0.8687844   )
    (479 0.08983521  0.2460768 0.8192873   )
    (480 0.080507    0.253589  0.772125    )
    (481 0.07161855  0.2618691 0.7273653   )
    (482 0.06319327  0.2707107 0.6849138   )
    (483 0.05526873  0.27982   0.6446357   )
    (484 0.04788245  0.2889027 0.6063961   )
    (485 0.041072    0.297665  0.57006     )
    (486 0.03486852  0.3059148 0.5355169   )
    (487 0.0292776   0.3138684 0.5027536   )
    (488 0.02429842  0.3218438 0.4717812   )
    (489 0.01993016  0.3301593 0.442611    )
    (490 0.016172    0.339133  0.415254    )
    (491 0.01301216  0.3490061 0.3896918   )
    (492 0.01039492  0.3597114 0.3657875   )
    (493 0.008253606 0.3711048 0.3433745   )
    (494 0.006521527 0.3830421 0.3222862   )
    (495 0.005132    0.395379  0.302356    )
    (496 0.004040958 0.4079935 0.2834515   )
    (497 0.003294809 0.420852  0.2655762   )
    (498 0.002962582 0.4339431 0.2487677   )
    (499 0.003113303 0.4472552 0.2330638   )
    (500 0.003816    0.460777  0.218502    )
    (501 0.005119052 0.4744987 0.2050809   )
    (502 0.006988242 0.4884183 0.1926422   )
    (503 0.009368707 0.5025352 0.1809889   )
    (504 0.01220558  0.5168493 0.1699236   )
    (505 0.015444    0.53136   0.159249    )
    (506 0.01904291  0.5460662 0.1488256   )
    (507 0.02301648  0.5609631 0.1387446   )
    (508 0.0273927   0.576045  0.1291547   )
    (509 0.03219954  0.5913062 0.1202049   )
    (510 0.037465    0.606741  0.112044    )
    (511 0.04321471  0.6223385 0.1047725   )
    (512 0.04946494  0.6380665 0.09829797  )
    (513 0.05622962  0.6538877 0.09247956  )
    (514 0.06352266  0.6697647 0.0871765   )
    (515 0.071358    0.68566   0.082248    )
    (516 0.0797383   0.7015176 0.07757258  )
    (517 0.08862118  0.7172061 0.07310594  )
    (518 0.09795302  0.7325755 0.06882312  )
    (519 0.1076802   0.7474758 0.06469914  )
    (520 0.117749    0.761757  0.060709    )
    (521 0.1281188   0.7753066 0.056837    )
    (522 0.1388003   0.7881629 0.05310449  )
    (523 0.1498174   0.8004016 0.04954208  )
    (524 0.1611937   0.8120987 0.04618038  )
    (525 0.172953    0.82333   0.04305     )
    (526 0.1851043   0.8341717 0.04016966  )
    (527 0.1975981   0.8447021 0.03751042  )
    (528 0.21037     0.8549997 0.03503145  )
    (529 0.2233557   0.8651431 0.03269192  )
    (530 0.236491    0.875211  0.030451    )
    (531 0.249728    0.8852551 0.02827815  )
    (532 0.2630845   0.8952196 0.02618405  )
    (533 0.2765947   0.905022  0.02418967  )
    (534 0.2902928   0.9145796 0.022316    )
    (535 0.304213    0.92381   0.020584    )
    (536 0.318377    0.9326291 0.0190058   )
    (537 0.3327561   0.9409474 0.01755808  )
    (538 0.3473091   0.9486739 0.01620865  )
    (539 0.3619948   0.9557177 0.01492535  )
    (540 0.376772    0.961988  0.013676    )
    (541 0.3916074   0.9674229 0.01243726  )
    (542 0.4064996   0.9720774 0.01122119  )
    (543 0.4214554   0.9760357 0.01004869  )
    (544 0.4364813   0.9793818 0.008940661 )
    (545 0.451584    0.9822    0.007918    )
    (546 0.4667809   0.9845779 0.006994978 )
    (547 0.4821324   0.9866174 0.006159347 )
    (548 0.4977096   0.9884241 0.005392226 )
    (549 0.5135838   0.9901034 0.004674737 )
    (550 0.529826    0.991761  0.003988    )
    (551 0.5464835   0.9934678 0.003319025 )
    (552 0.5635077   0.9951573 0.002678381 )
    (553 0.5808257   0.9967282 0.002082523 )
    (554 0.5983651   0.9980795 0.001547911 )
    (555 0.616053    0.99911   0.001091    )
    (556 0.6338253   0.9997301 0.0007238649)
    (557 0.651651    0.9998957 0.0004410432)
    (558 0.6695077   0.9995742 0.0002326891)
    (559 0.6873729   0.9987332 8.895665E-5 )
    (560 0.705224    0.99734   0.0         )
    (565 0.793832    0.98238   0.0         )
    (570 0.878655    0.955552  0.0         )
    (575 0.951162    0.915175  0.0         )
    (580 1.01416     0.868934  0.0         )
    (585 1.0743      0.825623  0.0         )
    (590 1.11852     0.777405  0.0         )
    (595 1.1343      0.720353  0.0         )
    (600 1.12399     0.658341  0.0         )
    (605 1.0891      0.593878  0.0         )
    (610 1.03048     0.527963  0.0         )
    (615 0.95074     0.461834  0.0         )
    (620 0.856297    0.398057  0.0         )
    (625 0.75493     0.339554  0.0         )
    (630 0.647467    0.283493  0.0         )
    (635 0.53511     0.228254  0.0         )
    (640 0.431567    0.179828  0.0         )
    (645 0.34369     0.140211  0.0         )
    (650 0.268329    0.107633  0.0         )
    (655 0.2043      0.081187  0.0         )
    (660 0.152568    0.060281  0.0         )
    (665 0.11221     0.044096  0.0         )
    (670 0.081261    0.0318    0.0         )
    (675 0.05793     0.022602  0.0         )
    (680 0.040851    0.015905  0.0         )
    (685 0.028623    0.01113   0.0         )
    (690 0.019941    0.007749  0.0         )
    (695 0.013842    0.005375  0.0         )
    (700 0.009577    0.003718  0.0         )
    (705 0.006605    0.002565  0.0         )
    (710 0.004553    0.001768  0.0         )
    (715 0.003145    0.001222  0.0         )
    (720 0.002175    0.000846  0.0         )
    (725 0.001506    0.000586  0.0         )
    (730 0.001045    0.000407  0.0         )
    (735 0.000727    0.000284  0.0         )
    (740 0.000508    0.000199  0.0         )
    (745 0.000356    0.00014   0.0         )
    (750 0.000251    9.8E-5    0.0         )
    (755 0.000178    7.0E-5    0.0         )
    (760 0.000126    5.0E-5    0.0         )
    (765 9.0E-5      3.6E-5    0.0         )
    (770 6.5E-5      2.5E-5    0.0         )
    (775 4.6E-5      1.8E-5    0.0         )
    (780 3.3E-5      1.3E-5    0.0         ))
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

(defconst cie-1931-second-radiation-constant 1.435L-2
  "Second radiation constant.

Value used by the CIE when defining the CIE 1931 color space.")

(defvar *second-radiation-constant* codata-2018-second-radiation-constant
  "Second radiation constant.")
(proclaim '(type (real (0)) *second-radiation-constant*))

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

;;; color-matching-functions.lisp ends here
