;;; rs-colors-ral.lisp --- RAL Classic color names.

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

;;; Commentary:

;; sRGB color values extracted from color patches provided by
;; <https://www.ral-farben.de>, menu item "Anwendung & Hilfe",
;; "Alle RAL Farbnamen".

;;; Code:

(in-package :common-lisp-user)

(defpackage :rs-colors-ral
  (:nicknames :ral-color)
  (:use :common-lisp
	:rs-colors
	:rs-colors-internal)
  (:documentation "RAL Classic color names."))

(in-package :rs-colors-ral)

(defmacro ral-classic (number name &rest aliases)
  `(define-color-names (,name ,@aliases)
     (make-srgb-color-from-number ,number :byte-size 8)
     ,(format nil "RAL Classic color ‘#~6,'0,X’." number)))

(ral-classic #XCDBA88 ral-1000 gruenbeige greenbeige beigevert beigeverdoso beigeverdastro groenbeige)
(ral-classic #XD0B084 ral-1001 beige)
(ral-classic #XD2AA6D ral-1002 sandgelb sandyellow jaunesable amarilloarena giallosabbia zandgeel)
(ral-classic #XF9A800 ral-1003 signalgelb signalyellow jaunedesecurite amarillosenales giallosegnale signaalgeel)
(ral-classic #XE49E00 ral-1004 goldgelb goldenyellow jauneor amarillooro giallooro goudgeel)
(ral-classic #XCB8E00 ral-1005 honiggelb honeyyellow jaunemiel amarillomiel giallomiele honinggeel)
(ral-classic #XE29000 ral-1006 maisgelb maizeyellow jaunemais amarillomaiz giallopolenta maisgeel)
(ral-classic #XE88C00 ral-1007 narzissengelb daffodilyellow jaunenarcisse amarillonarciso giallonarciso narcissengeel)
(ral-classic #XAF804F ral-1011 braunbeige brownbeige beigebrun beigepardo beigemarrone bruinbeige)
(ral-classic #XDDAF27 ral-1012 zitronengelb lemonyellow jaunecitron amarillolimon giallolimone citroengeel)
(ral-classic #XE3D9C6 ral-1013 perlweiss oysterwhite blancperle blancoperla biancoperla parelwit)
(ral-classic #XDDC49A ral-1014 elfenbein ivory ivoire marfil avorio ivoorkleurig)
(ral-classic #XE6D2B5 ral-1015 hellelfenbein lightivory ivoireclair marfilclaro avoriochiaro lichtivoorkleurig)
(ral-classic #XF1DD38 ral-1016 schwefelgelb sulfuryellow jaunesoufre amarilloazufre giallozolfo zwavelgeel)
(ral-classic #XF6A950 ral-1017 safrangelb saffronyellow jaunesafran amarilloazafran giallozafferano saffraangeel)
(ral-classic #XFACA30 ral-1018 zinkgelb zincyellow jaunezinc amarillodezinc giallozinco zinkgeel)
(ral-classic #XA48F7A ral-1019 graubeige greybeige beigegris beigeagrisado beigegrigiastro grijsbeige)
(ral-classic #XA08F65 ral-1020 olivgelb oliveyellow jauneolive amarillooliva gialloolivastro olijfgeel)
(ral-classic #XF6B600 ral-1021 rapsgelb colzayellow jaunecolza amarillocolza giallonavone koolzaadgeel)
(ral-classic #XF7B500 ral-1023 verkehrsgelb trafficyellow jaunesignalisation amarillotrafico giallotraffico verkeersgeel)
(ral-classic #XBA8F4C ral-1024 ockergelb ochreyellow jauneocre amarilloocre gialloocra okergeel)
(ral-classic #XFFFF00 ral-1026 leuchtgelb luminousyellow jaunebrillant amarillobrillante giallobrillante briljantgeel)
(ral-classic #XA77F0E ral-1027 currygelb curry jaunecurry amarillocurry giallocurry kerriegeel)
(ral-classic #XFF9B00 ral-1028 melonengelb melonyellow jaunemelon amarillomelon giallomelone meloengeel)
(ral-classic #XE2A300 ral-1032 ginstergelb broomyellow jaunegenet amarilloretama gialloscopa bremgeel)
(ral-classic #XF99A1C ral-1033 dahliengelb dahliayellow jaunedahlia amarillodalia giallodahlien dahliageel)
(ral-classic #XEB9C52 ral-1034 pastellgelb pastelyellow jaunepastel amarillopastel giallopastello pastelgeel)
(ral-classic #X908370 ral-1035 perlbeige pearlbeige beigenacre beigeperlado beigeperlato parelmoergrijs)
(ral-classic #X80643F ral-1036 perlgold pearlgold ornacre oroperlado oroperlato parelmoergoud)
(ral-classic #XF09200 ral-1037 sonnengelb sunyellow jaunesoleil amarillosol giallosole zonnegeel)
(ral-classic #XDA6E00 ral-2000 gelborange yelloworange orangejaune amarillonaranja aranciogiallastro geeloranje)
(ral-classic #XBA481B ral-2001 rotorange redorange orangerouge rojoanaranjado aranciorossastro roodoranje)
(ral-classic #XBF3922 ral-2002 blutorange vermilion orangesang naranjasanguineo aranciosanguigno vermiljoen)
(ral-classic #XF67828 ral-2003 pastellorange pastelorange orangepastel naranjapalido aranciopastello pasteloranje)
(ral-classic #XE25303 ral-2004 reinorange pureorange orangepur naranjapuro aranciopuro zuiveroranje)
(ral-classic #XFF4D06 ral-2005 leuchtorange luminousorange orangebrillant naranjabrillante aranciobrillante briljantoranje)
(ral-classic #XFFB200 ral-2007 leuchthell-orange luminousbrightorange orangeclairbrillant naranjaclarobrillante aranciochiarobrillante briljantlichtoranje)
(ral-classic #XED6B21 ral-2008 hellrotorange brightredorange orangerougeclair rojoclaroanaranjado rossoaranciochiaro lichtroodoranje)
(ral-classic #XDE5307 ral-2009 verkehrsorange trafficorange orangesignalisation naranjatrafico aranciotraffico verkeersoranje)
(ral-classic #XD05D28 ral-2010 signalorange orangedesecurite naranjasenales aranciosegnale signaaloranje)
(ral-classic #XE26E0E ral-2011 tieforange deeporange orangefonce naranjaintenso arancioprofondo dieporanje)
(ral-classic #XD5654D ral-2012 lachsorange salmonorange orangesaumon naranjasalmon aranciosalmone zalmoranje)
(ral-classic #X923E25 ral-2013 perlorange pearlorange orangenacre naranjaperlado arancioperlato parelmoeroranje)
(ral-classic #XA72920 ral-3000 feuerrot flamered rougefeu rojovivo rossofuoco vuurrood)
(ral-classic #X9B2423 ral-3001 signalrot signalred rougedesecurite rojosenales rossosegnale signaalrood)
(ral-classic #X9B2321 ral-3002 karminrot carminered rougecarmin rojocarmin rossocarminio karmijnrood)
(ral-classic #X861A22 ral-3003 rubinrot rubyred rougerubis rojorubi rossorubino robijnrood)
(ral-classic #X6B1C23 ral-3004 purpurrot purplered rougepourpre rojopurpura rossoporpora purperrood)
(ral-classic #X59191F ral-3005 weinrot winered rougevin rojovino rossovino wijnrood)
(ral-classic #X3E2022 ral-3007 schwarzrot blackred rougenoir rojonegruzco rossonerastro zwartrood)
(ral-classic #X6D342D ral-3009 oxidrot oxidered rougeoxyde rojooxido rossoossido oxyderood)
(ral-classic #X792423 ral-3011 braunrot brownred rougebrun rojopardo rossomarrone bruinrood)
(ral-classic #XC6846D ral-3012 beigerot beigered rougebeige rojobeige rossobeige beigerood)
(ral-classic #X972E25 ral-3013 tomatenrot tomatored rougetomate rojotomate rossopomodoro tomaatrood)
(ral-classic #XCB7375 ral-3014 altrosa antiquepink vieuxrose rojoviejo rosaantico oudroze)
(ral-classic #XD8A0A6 ral-3015 hellrosa lightpink roseclair rosaclaro rosachiaro lichtroze)
(ral-classic #XA63D2F ral-3016 korallenrot coralred rougecorail rojocoral rossocorallo koraalrood)
(ral-classic #XCB555D ral-3017 rose rosa rosato bleekrood)
(ral-classic #XC73F4A ral-3018 erdbeerrot strawberryred rougefraise rojofresa rossofragola aardbeirood)
(ral-classic #XBB1E10 ral-3020 verkehrsrot trafficred rougesignalisation rojotrafico rossotraffico verkeersrood)
(ral-classic #XCF6955 ral-3022 lachsrot salmonpink rougesaumon rojosalmon rossosalmone zalmrood)
(ral-classic #XFF2D21 ral-3024 leuchtrot luminousred rougebrillant rojobrillante rossobrillante briljantrood)
(ral-classic #XFF2A1B ral-3026 leuchthellrot luminousbrightred rougeclairbrillant rojoclarobrillante rossochiarobrillante briljantlichtrood)
(ral-classic #XAB273C ral-3027 himbeerrot raspberryred rougeframboise rojoframbuesa rossolampone framboosrood)
(ral-classic #XCC2C24 ral-3028 reinrot purered rougepu rojopuro rossopuro zuiverrood)
(ral-classic #XA63437 ral-3031 orientrot orientred rougeoriental rojooriente rossooriente orientrood)
(ral-classic #X701D23 ral-3032 perlrubinrot pearlrubyred rougerubisnacre rojorubiperlado rossorubinoperlato parelmoerdonkerrood)
(ral-classic #XA53A2D ral-3033 perlrosa pearlpink rosenacre rosaperlado rosaperlato parelmoerlichtrood)
(ral-classic #X816183 ral-4001 rotlila redlilac lilasrouge rojolila lillarossastro roodlila)
(ral-classic #X8D3C4B ral-4002 rotviolett redviolet violetrouge rojovioleta violarossastro roodpaars)
(ral-classic #XC4618C ral-4003 erikaviolett heatherviolet violetbruyere violetaerica violaerica heidepaars)
(ral-classic #X651E38 ral-4004 bordeauxviolett claretviolet violetbordeaux burdeos violabordeaux bordeuaxpaars)
(ral-classic #X76689A ral-4005 blaulila bluelilac lilasbleu lilaazulado lillabluastro blauwlila)
(ral-classic #X903373 ral-4006 verkehrspurpur trafficpurple pourpresignalisation purpuratrafico porporatraffico verkeerspurper)
(ral-classic #X47243C ral-4007 purpurviolett purpleviolet violetpourpre violetapurpura porporavioletto purperviolet)
(ral-classic #X844C82 ral-4008 signalviolett signalviolet violetdesecurite violetasenales violettosegnale signaalviolet)
(ral-classic #X9D8692 ral-4009 pastellviolett pastelviolet violetpastel violetapastel violettopastello)
(ral-classic #XBC4077 ral-4010 telemagenta magentatele)
(ral-classic #X6E6387 ral-4011 perlviolett pearlviolet violetnacre violetaperlado violettoperlato parelmoerdonkerviolet)
(ral-classic #X6B6B7F ral-4012 perlbrombeer pearlblackberry murenacre moradoperlado moraperlato parelmoerlichtviolet)
(ral-classic #X314F6F ral-5000 violettblau violetblue bleuviolet azulvioleta bluviolaceo paarsblauw)
(ral-classic #X0F4C64 ral-5001 gruenblau greenblue bleuvert azulverdoso bluverdastro groenblauw)
(ral-classic #X00387B ral-5002 ultramarinblau ultramarineblue bleuoutremer azulultramar bluoltremare ultramarijnblauw)
(ral-classic #X1F3855 ral-5003 saphirblau sapphireblue bleusaphir azulzafiro bluzaffiro saffierblauw)
(ral-classic #X191E28 ral-5004 schwarzblau blackblue bleunoir azulnegruzco blunerastro zwartblauw)
(ral-classic #X005387 ral-5005 signalblau signalblue bleudesecurite azulsenales blusegnale signaalblauw)
(ral-classic #X376B8C ral-5007 brillantblau brilliantblue bleubrillant azulbrillante blubrillante briljantblauw)
(ral-classic #X2B3A44 ral-5008 graublau greyblue bleugris azulgrisaceo blugrigiastro grijsblauw)
(ral-classic #X225F78 ral-5009 azurblau azureblue bleuazur azulazur bluazzurro azuurblauw)
(ral-classic #X004F7C ral-5010 enzianblau gentianblue bleugentiane azulgenciana blugenziana gentiaanblauw)
(ral-classic #X1A2B3C ral-5011 stahlblau steelblue bleuacier azulacero bluacciaio staalblauw)
(ral-classic #X0089B6 ral-5012 lichtblau lightblue bleuclair azulluminoso bluluce lichtblauw)
(ral-classic #X193153 ral-5013 kobaltblau cobaltblue bleucobalt azulcobalto blucobalto kobaltblauw)
(ral-classic #X637D96 ral-5014 taubenblau pigeonblue bleupigeon azulcolombino blucolomba duifblauw)
(ral-classic #X007CB0 ral-5015 himmelblau skyblue bleuciel azulceleste blucielo hemelsblauw)
(ral-classic #X005B8C ral-5017 verkehrsblau trafficblue bleusignalisation azultrafico blutraffico verkeersblauw)
(ral-classic #X058B8C ral-5018 tuerkisblau turquoiseblue bleuturquoise azulturquesa bluturchese turkooisblauw)
(ral-classic #X005E83 ral-5019 capriblau capriblue bleucapri azulcapri blucapri capriblauw)
(ral-classic #X00414B ral-5020 ozeanblau oceanblue bleuocean azuloceano bluoceano oceaanblauw)
(ral-classic #X007577 ral-5021 wasserblau waterblue bleudeau azulagua bluacqua waterblauw)
(ral-classic #X222D5A ral-5022 nachtblau nightblue bleunocturne azulnoche blunotte nachtblauw)
(ral-classic #X42698C ral-5023 fernblau distantblue bleudistant azullejania bludistante verblauw)
(ral-classic #X6093AC ral-5024 pastellblau pastelblue bleupastel azulpastel blupastello pastelblauw)
(ral-classic #X21697C ral-5025 perlenzian pearlgentianblue gentianenacre gencianperlado blugenzianaperlato parelmoerblauw)
(ral-classic #X0F3052 ral-5026 perlnachtblau pearlnightblue bleunuitnacre azulnocheperlado blunotteperlato parelmoernachtblauw)
(ral-classic #X3C7460 ral-6000 patinagruen patinagreen vertpatine verdepatina patinagroen)
(ral-classic #X366735 ral-6001 smaragdgruen emeraldgreen vertemeraude verdeesmeralda verdesmeraldo smaragdgroen)
(ral-classic #X325928 ral-6002 laubgruen leafgreen vertfeuillage verdehoja verdefoglia loofgroen)
(ral-classic #X50533C ral-6003 olivgruen olivegreen vertolive verdeoliva olijfgroen)
(ral-classic #X024442 ral-6004 blaugruen bluegreen vertbleu verdeazulado verdebluastro blauwgroen)
(ral-classic #X114232 ral-6005 moosgruen mossgreen vertmousse verdemusgo verdemuschio mosgroen)
(ral-classic #X3C392E ral-6006 grauoliv greyolive olivegris olivagrisaceo olivagrigiastro grijsolijfgroen)
(ral-classic #X2C3222 ral-6007 flaschengruen bottlegreen vertbouteille verdebotella verdebottiglia flessengroen)
(ral-classic #X37342A ral-6008 braungruen browngreen vertbrun verdeparduzco verdebrunastro bruingroen)
(ral-classic #X27352A ral-6009 tannengruen firgreen vertsapin verdeabeto verdeabete dennengroen)
(ral-classic #X4D6F39 ral-6010 grasgruen grassgreen vertherbe verdehierba verdeerba grasgroen)
(ral-classic #X6C7C59 ral-6011 resedagruen resedagreen vertreseda verdereseda resedagroen)
(ral-classic #X303D3A ral-6012 schwarzgruen blackgreen vertnoir verdenegruzco verdenerastro zwartgroen)
(ral-classic #X7D765A ral-6013 schilfgruen reedgreen vertjonc verdecana verdecanna rietgroen)
(ral-classic #X474135 ral-6014 gelboliv yellowolive olivejaune olivaamarillo olivagiallastro geelolijfgroen)
(ral-classic #X3D3D36 ral-6015 schwarzoliv blackolive olivenoir olivanegruzco olivanerastro zwartolijfgroen)
(ral-classic #X00694C ral-6016 tuerkisgruen turquoisegreen vertturquoise verdeturquesa verdeturchese turkooisgroen)
(ral-classic #X587F40 ral-6017 maigruen maygreen vertmai verdemayo verdemaggio meigroen)
(ral-classic #X61993B ral-6018 gelbgruen yellowgreen vertjaune verdeamarillento verdegiallastro geelgroen)
(ral-classic #XB9CEAC ral-6019 weissgruen pastelgreen vertblanc verdeblanquecino verdebiancastro witgroen)
(ral-classic #X37422F ral-6020 chromoxidgruen chromegreen vertoxydechromique verdecromo chroomoxydegroen)
(ral-classic #X8A9977 ral-6021 blassgruen palegreen vertpale verdepalido verdepallido bleekgroen)
(ral-classic #X3A3327 ral-6022 braunoliv olivedrab olivebrun olivaparduzco olivabrunastro bruinolijfgroen)
(ral-classic #X008351 ral-6024 verkehrsgruen trafficgreen vertsignalisation verdetrafico verdetraffico verkeersgroen)
(ral-classic #X5E6E3B ral-6025 farngruen ferngreen vertfougere verdehelecho verdefelce varengroen)
(ral-classic #X005F4E ral-6026 opalgruen opalgreen vertopale verdeopalo verdeopale opaalgroen)
(ral-classic #X7EBAB5 ral-6027 lichtgruen lightgreen vertclair verdeluminoso verdechiaro lichtgroen)
(ral-classic #X315442 ral-6028 kieferngruen pinegreen vertpin verdepino pijnboomgroen)
(ral-classic #X006F3D ral-6029 minzgruen mintgreen vertmenthe verdementa mintgroen)
(ral-classic #X237F52 ral-6032 signalgruen signalgreen vertdesecurite verdesenales verdesegnale signaalgroen)
(ral-classic #X46877F ral-6033 minttuerkis mintturquoise turquoisementhe turquesamenta turchesementa)
(ral-classic #X7AACAC ral-6034 pastelltuerkis pastelturquoise turquoisepastel turquesapastel turchesepastello)
(ral-classic #X194D25 ral-6035 perlgruen pearlgreen vertnacre verdeperlado verdeperlato parelmoerdonkergroen)
(ral-classic #X04574B ral-6036 perlopalgruen pearlopalgreen vertopalnacre verdeopaloperlado verdeopaloperlato parelmoerlichtgroen)
(ral-classic #X008B29 ral-6037 reingruen puregreen vertpur verdepuro zuivergroen)
(ral-classic #X00B51A ral-6038 leuchtgruen luminousgreen vertbrillant verdebrillante briljantgroen)
(ral-classic #X7A888E ral-7000 fehgrau squirrelgrey grispetitgris grisardilla grigiovaio pelsgrijs)
(ral-classic #X8C969D ral-7001 silbergrau silvergrey grisargent grisplata grigioargento zilvergrijs)
(ral-classic #X817863 ral-7002 olivgrau olivegrey grisolive grisoliva grigioolivastro olijfgrijs)
(ral-classic #X7A7669 ral-7003 moosgrau mossgrey grismousse grismusgo grigiomuschio mosgrijs)
(ral-classic #X9B9B9B ral-7004 signalgrau signalgrey grisdesecurite grissenales grigiosegnale signaalgrijs)
(ral-classic #X6C6E6B ral-7005 mausgrau mousegrey grissouris grisraton grigiotopo muisgrijs)
(ral-classic #X766A5E ral-7006 beigegrau beigegrey grisbeige grigiobeige beigegrijs)
(ral-classic #X745E3D ral-7008 khakigrau khakigrey griskaki griscaqui grigiokaki kakigrijs)
(ral-classic #X5D6058 ral-7009 gruengrau greengrey grisvert grisverdoso grigioverdastro groengrijs)
(ral-classic #X585C56 ral-7010 zeltgrau tarpaulingrey gristente grislona grigiotenda zeildoekgrijs)
(ral-classic #X52595D ral-7011 eisengrau irongrey grisfer grishierro grigioferro ijzergrijs)
(ral-classic #X575D5E ral-7012 basaltgrau basaltgrey grisbasalte grisbasalto grigiobasalto bazaltgrijs)
(ral-classic #X575044 ral-7013 braungrau browngrey grisbrun grisparduzco grigiobrunastro bruingrijs)
(ral-classic #X4F5358 ral-7015 schiefergrau slategrey grisardoise grispizarra grigioardesia leigrijs)
(ral-classic #X383E42 ral-7016 anthrazitgrau anthracitegrey grisanthracite grisantracita grigioantracite antracietgrijs)
(ral-classic #X2F3234 ral-7021 schwarzgrau blackgrey grisnoir grisnegruzco grigionerastro zwartgrijs)
(ral-classic #X4C4A44 ral-7022 umbragrau umbragrey gristerredombre grissombra grigioombra ombergrijs)
(ral-classic #X808076 ral-7023 betongrau concretegrey grisbeton grishormigon grigiocalcestruzzo betongrijs)
(ral-classic #X45494E ral-7024 graphitgrau graphitegrey grisgraphite grisgrafita grigiografite grafietgrijs)
(ral-classic #X374345 ral-7026 granitgrau granitegrey grisgranit grisgranito grigiogranito granietgrijs)
(ral-classic #X928E85 ral-7030 steingrau stonegrey grispierre grispiedra grigiopietra steengrijs)
(ral-classic #X5B686D ral-7031 blaugrau bluegrey grisbleu grisazulado grigiobluastro blauwgrijs)
(ral-classic #XB5B0A1 ral-7032 kieselgrau pebblegrey grissilex grisguijarro grigioghiaia kiezelgrijs)
(ral-classic #X7F8274 ral-7033 zementgrau cementgrey grisciment griscemento grigiocemento cementgrijs)
(ral-classic #X92886F ral-7034 gelbgrau yellowgrey grisjaune grisamarillento grigiogiallastro geelgrijs)
(ral-classic #XC5C7C4 ral-7035 lichtgrau lightgrey grisclair grisluminoso grigioluce lichtgrijs)
(ral-classic #X979392 ral-7036 platingrau platinumgrey grisplatine grisplatino grigioplatino platinagrijs)
(ral-classic #X7A7B7A ral-7037 staubgrau dustygrey grispoussiere grispolvo grigiopolvere stofgrijs)
(ral-classic #XB0B0A9 ral-7038 achatgrau agategrey grisagate grisagata grigioagata agaatgrijs)
(ral-classic #X6B665E ral-7039 quarzgrau quartzgrey grisquartz griscuarzo grigioquarzo kwartsgrijs)
(ral-classic #X989EA1 ral-7040 fenstergrau windowgrey grisfenetre grisventana grigiofinestra venstergrijs)
(ral-classic #X8E9291 ral-7042 verkehrsgrau-a trafficgrey-a grissignalisation-a gristrafico-a grigiotraffico-a verkeersgrijs-a)
(ral-classic #X4F5250 ral-7043 verkehrsgrau-b trafficgrey-b grissignalisation-b gristrafico-b grigiotraffico-b verkeersgrijs-b)
(ral-classic #XB7B3A8 ral-7044 seidengrau silkgrey grissoie grisseda grigioseta zijdegrijs)
(ral-classic #X8D9295 ral-7045 telegrau-1 telegrey-1 telegris-1 gristele-1 grigiotele-1 telegrijs-1)
(ral-classic #X7F868A ral-7046 telegrau-2 telegrey-2 telegris-2 gristele-2 grigiotele-2 telegrijs-2)
(ral-classic #XC8C8C7 ral-7047 telegrau-4 telegrey-4 telegris-4 gristele-4 grigiotele-4 telegrijs-4)
(ral-classic #X817B73 ral-7048 perlmausgrau pearlmousegrey grissourisnacre grismusgoperlado grigiotopoperlato parelmoermuisgrijs)
(ral-classic #X89693E ral-8000 gruenbraun greenbrown brunvert pardoverdoso marroneverdastro groenbruin)
(ral-classic #X9D622B ral-8001 ockerbraun ochrebrown brunterredesienne pardoocre marroneocra okerbruin)
(ral-classic #X794D3E ral-8002 signalbraun signalbrown brundesecurite marronsenales marronesegnale signaalbruin)
(ral-classic #X7E4B26 ral-8003 lehmbraun claybrown brunargile pardoarcilla marronefango leembruin)
(ral-classic #X8D4931 ral-8004 kupferbraun copperbrown bruncuivre pardocobre marronerame koperbruin)
(ral-classic #X70452A ral-8007 rehbraun fawnbrown brunfauve pardocorzo marronecapriolo reebruin)
(ral-classic #X724A25 ral-8008 olivbraun olivebrown brunolive pardooliva marroneoliva olijfbruin)
(ral-classic #X5A3826 ral-8011 nussbraun nutbrown brunnoisette pardonuez marronenoce notenbruin)
(ral-classic #X66332B ral-8012 rotbraun redbrown brunrouge pardorojo marronerossiccio roodbruin)
(ral-classic #X4A3526 ral-8014 sepiabraun sepiabrown brunsepia sepia marroneseppia sepiabruin)
(ral-classic #X5E2F26 ral-8015 kastanienbraun chestnutbrown marron castano marronecastagna kastanjebruin)
(ral-classic #X4C2B20 ral-8016 mahagonibraun mahoganybrown brunacajou caoba marronemogano mahoniebruin)
(ral-classic #X442F29 ral-8017 schokoladenbraun chocolatebrown brunchocolat chocolate marronecioccolata chocoladebruin)
(ral-classic #X3D3635 ral-8019 graubraun greybrown brungris pardogrisaceo marronegrigiastro grijsbruin)
(ral-classic #X1A1718 ral-8022 schwarzbraun blackbrown brunnoir pardonegruzco marronenerastro zwartbruin)
(ral-classic #XA45729 ral-8023 orangebraun orangebrown brunorange pardoanaranjado marronearancio oranjebruin)
(ral-classic #X795038 ral-8024 beigebraun beigebrown brunbeige pardobeige marronebeige beigebruin)
(ral-classic #X755847 ral-8025 blassbraun palebrown brunpale pardopalido marronepallido bleekbruin)
(ral-classic #X513A2A ral-8028 terrabraun terrabrown brunterre marrontierra marroneterra terrabruin)
(ral-classic #X7F4031 ral-8029 perlkupfer pearlcopper cuivrenacre cobreperlado rameperlato parelmoerkoper)
(ral-classic #XE9E0D2 ral-9001 cremeweiss cream blanccreme blancocrema biancocrema cremewit)
(ral-classic #XD7D5CB ral-9002 grauweiss greywhite blancgris blancogrisaceo biancogrigiastro grijswit)
(ral-classic #XECECE7 ral-9003 signalweiss signalwhite blancdesecurite blancosenales biancosegnale signaalwit)
(ral-classic #X2B2B2C ral-9004 signalschwarz signalblack noirdesecurite negrosenales nerosegnale signaalzwart)
(ral-classic #X0E0E10 ral-9005 tiefschwarz jetblack noirfonce negrointenso nerointenso gitzwart)
(ral-classic #XA1A1A0 ral-9006 weissaluminium whitealuminium aluminiumblanc aluminioblanco alluminiobrillante blankaluminiumkleurig)
(ral-classic #X878581 ral-9007 graualuminium greyaluminium aluminiumgris aluminiogris alluminiogrigiastro grijsaluminiumkleurig)
(ral-classic #XF1ECE1 ral-9010 reinweiss purewhite blancpur blancopuro biancopuro zuiverwit)
(ral-classic #X27292B ral-9011 graphitschwarz graphiteblack noirgraphite negrografito nerografite grafietzwart)
(ral-classic #XF1F0EA ral-9016 verkehrsweiss trafficwhite blancsignalisation blancotrafico biancotraffico verkeerswit)
(ral-classic #X2A292A ral-9017 verkehrsschwarz trafficblack noirsignalisation negrotrafico nerotraffico verkeerszwart)
(ral-classic #XC8CBC4 ral-9018 papyrusweiss papyruswhite blancpapyrus blancopapiro biancopapiro papyruswit)
(ral-classic #X858583 ral-9022 perlhellgrau pearllightgrey grisclairnacre grisclaroperlado grigiochiaroperlato parelmoerlichtgrijs)
(ral-classic #X797B7A ral-9023 perldunkelgrau pearldarkgrey grisfoncenacre grisoscuroperlado grigioscuroperlato parelmoerdonkergrijs)

;;; rs-colors-ral.lisp ends here
