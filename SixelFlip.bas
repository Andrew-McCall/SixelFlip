   10 REM ###################
   20 REM ##  Sixel Flip   ##
   30 REM ## Andrew McCall ##
   40 REM ###################
   50 REM -------------------------------------------------
   60 REM Copyright (C) 2023
   70 REM ------------------------------------------------
   80 REM Andrew McCall
   90 REM AndrewDavidMcCall@hotmail.com
  100 REM This can not be copied and/or distributed
  110 REM without the express permission of Andrew McCall
  120 REM -------------------------------------------------
  130 MODE 7
  140 CLS
  150 VDU 23,1,0;0;0;0; : : REM Hide the cursor
  160
  170 REM  TEXT COLOUR CODES : RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE
  180 TR$=CHR$(129) : TG$=CHR$(130) : TY$=CHR$(131) : TB$=CHR$(132)
  190 TM$=CHR$(133) : TC$=CHR$(134) : TW$=CHR$(135)
  200
  210 REM  GRAPHIC COLOUR CODES : RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE
  220 R$=CHR$(145) : G$=CHR$(146) : Y$=CHR$(147) : B$=CHR$(148)
  230 M$=CHR$(149) : C$=CHR$(150) : W$=CHR$(151)
  240
  250 REM  FLASHING, BACKGROUND COLOUR E.G 'R$BACK$' Makes background red
  260 FLASH$=CHR$(136) : UNFLASH$=CHR$(137) : BACK$=CHR$(157)
  270
  280 DIM Cards$(10,8)
  290
  300 REM Card Cover
  310 Cover1$ = W$+CHR$(249)+CHR$(175)+CHR$(175)+CHR$(175)+CHR$(175)+CHR$(246)
  320 Cover2$ = W$+CHR$(255)+CHR$(224)+CHR$(230)+CHR$(185)+CHR$(176)+CHR$(255)
  330 Cover3$ = W$+CHR$(255)+CHR$(162)+CHR$(230)+CHR$(185)+CHR$(161)+CHR$(255)
  340 Cover4$ = W$+CHR$(187)+CHR$(252)+CHR$(252)+CHR$(252)+CHR$(252)+CHR$(231)
  350
  360 REM Card One, Frame One, Puc-Mam
  370 Cards$(0,0) = Y$+CHR$(160)+CHR$(224)+CHR$(252)+CHR$(252)+CHR$(176)+CHR$(160)
  380 Cards$(0,1) = Y$+CHR$(160)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(167)+CHR$(160)
  390 Cards$(0,2) = Y$+CHR$(160)+CHR$(255)+CHR$(255)+CHR$(245)+CHR$(160)+CHR$(160)
  400 Cards$(0,3) = Y$+CHR$(160)+CHR$(171)+CHR$(255)+CHR$(255)+CHR$(189)+CHR$(160)
  410
  420 REM Card One, Frame Two, Puc-Mam
  430 Cards$(0,4) = Y$+CHR$(160)+CHR$(224)+CHR$(252)+CHR$(252)+CHR$(176)+CHR$(160)
  440 Cards$(0,5) = Y$+CHR$(160)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  450 Cards$(0,6) = Y$+CHR$(160)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  460 Cards$(0,7) = Y$+CHR$(160)+CHR$(171)+CHR$(255)+CHR$(255)+CHR$(167)+CHR$(160)
  470
  480 REM Card Two, Frame One, Rainbow
  490 Cards$(1,0) = Y$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  500 Cards$(1,1) = G$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  510 Cards$(1,2) = R$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  520 Cards$(1,3) = B$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  530
  540 REM Card Two, Frame Two, Rainbow
  550 Cards$(1,4) = B$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  560 Cards$(1,5) = R$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  570 Cards$(1,6) = G$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  580 Cards$(1,7) = Y$+CHR$(160)+CHR$(185)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
  590
  600 REM Card Three, Frame One, Race Flag
  610 Cards$(2,0) = W$+CHR$(255)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)
  620 Cards$(2,1) = W$+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)
  630 Cards$(2,2) = W$+CHR$(255)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)
  640 Cards$(2,3) = W$+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)
  650
  660 REM Card Three, Frame Two, Race Flag
  670 Cards$(2,4) = W$+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)
  680 Cards$(2,5) = W$+CHR$(255)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)
  690 Cards$(2,6) = W$+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)
  700 Cards$(2,7) = W$+CHR$(255)+CHR$(255)+CHR$(160)+CHR$(255)+CHR$(160)+CHR$(255)
  710
  720 REM Card Four, Frame One, ANDY
  730 Cards$(3,0) = C$+CHR$(160)+CHR$(232)+CHR$(249)+CHR$(234)+CHR$(228)+CHR$(181)
  740 Cards$(3,1) = C$+CHR$(160)+CHR$(162)+CHR$(162)+CHR$(162)+CHR$(160)+CHR$(161)
  750 Cards$(3,2) = R$+CHR$(160)+CHR$(232)+CHR$(228)+CHR$(232)+CHR$(232)+CHR$(160)
  760 Cards$(3,3) = R$+CHR$(160)+CHR$(170)+CHR$(166)+CHR$(160)+CHR$(165)+CHR$(160)
  770
  780 REM Card Four, Frame Two, ANDY
  790 Cards$(3,4) = C$+CHR$(160)+CHR$(232)+CHR$(228)+CHR$(232)+CHR$(232)+CHR$(160)
  800 Cards$(3,5) = C$+CHR$(160)+CHR$(170)+CHR$(166)+CHR$(160)+CHR$(165)+CHR$(160)
  810 Cards$(3,6) = R$+CHR$(160)+CHR$(232)+CHR$(249)+CHR$(234)+CHR$(228)+CHR$(181)
  820 Cards$(3,7) = R$+CHR$(160)+CHR$(162)+CHR$(162)+CHR$(162)+CHR$(160)+CHR$(161)
  830
  840 REM Card Five, Frame One
  850 Cards$(4,0) = TM$+" This "
  860 Cards$(4,1) = TM$+"  is  "
  870 Cards$(4,2) = TM$+" text "
  880 Cards$(4,3) = TM$+" ! !  "
  890
  900 REM Card Five, Frame Two
  910 Cards$(4,4) = TY$+" This  "
  920 Cards$(4,5) = TY$+"  is   "
  930 Cards$(4,6) = TY$+" text  "
  940 Cards$(4,7) = TY$+"  ! !  "
  950
  960 REM Card Six, Frame One, Cletic
  970 Cards$(5,0) = B$+CHR$(184)+CHR$(232)+CHR$(187)+CHR$(180)+CHR$(228)+CHR$(160)
  980 Cards$(5,1) = B$+CHR$(184)+CHR$(190)+CHR$(228)+CHR$(173)+CHR$(244)+CHR$(160)
  990 Cards$(5,2) = B$+CHR$(171)+CHR$(236)+CHR$(169)+CHR$(190)+CHR$(166)+CHR$(160)
 1000 Cards$(5,3) = B$+CHR$(169)+CHR$(170)+CHR$(246)+CHR$(165)+CHR$(166)+CHR$(160)
 1010
 1020 REM Card Six, Frame Two, Cletic
 1030 Cards$(5,0) = G$+CHR$(184)+CHR$(232)+CHR$(187)+CHR$(180)+CHR$(228)+CHR$(160)
 1040 Cards$(5,1) = G$+CHR$(184)+CHR$(190)+CHR$(228)+CHR$(173)+CHR$(244)+CHR$(160)
 1050 Cards$(5,2) = G$+CHR$(171)+CHR$(236)+CHR$(169)+CHR$(190)+CHR$(166)+CHR$(160)
 1060 Cards$(5,3) = G$+CHR$(169)+CHR$(170)+CHR$(246)+CHR$(165)+CHR$(166)+CHR$(160)
 1070
 1080 REM Card Seven, Frame One, Clubs and Spades
 1090 Cards$(6,0) = B$+CHR$(160)+CHR$(224)+CHR$(252)+CHR$(176)+CHR$(160)+CHR$(160)
 1100 Cards$(6,1) = B$+CHR$(224)+CHR$(246)+CHR$(255)+CHR$(249)+CHR$(176)+CHR$(160)
 1110 Cards$(6,2) = B$+CHR$(162)+CHR$(167)+CHR$(255)+CHR$(171)+CHR$(161)+CHR$(160)
 1120 Cards$(6,3) = B$+CHR$(160)+CHR$(166)+CHR$(163)+CHR$(169)+CHR$(160)+CHR$(160)
 1130
 1140 REM Card Seven, Frame Two, Clubs and Spades
 1150 Cards$(6,4) = B$+CHR$(160)+CHR$(248)+CHR$(255)+CHR$(244)+CHR$(160)+CHR$(160)
 1160 Cards$(6,5) = B$+CHR$(248)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(244)+CHR$(160)
 1170 Cards$(6,6) = B$+CHR$(239)+CHR$(191)+CHR$(255)+CHR$(239)+CHR$(191)+CHR$(160)
 1180 Cards$(6,7) = B$+CHR$(160)+CHR$(168)+CHR$(175)+CHR$(164)+CHR$(160)+CHR$(160)
 1190
 1200 REM Card Eight, Frame One, Heart and Diamond
 1210 Cards$(7,0) = R$+CHR$(160)+CHR$(160)+CHR$(254)+CHR$(253)+CHR$(160)+CHR$(160)
 1220 Cards$(7,1) = R$+CHR$(160)+CHR$(250)+CHR$(255)+CHR$(255)+CHR$(245)+CHR$(160)
 1230 Cards$(7,2) = R$+CHR$(160)+CHR$(235)+CHR$(255)+CHR$(255)+CHR$(183)+CHR$(160)
 1240 Cards$(7,3) = R$+CHR$(160)+CHR$(160)+CHR$(239)+CHR$(191)+CHR$(160)+CHR$(160)
 1250
 1260 REM Card Eight, Frame Two
 1270 Cards$(7,4) = R$+CHR$(160)+CHR$(224)+CHR$(176)+CHR$(224)+CHR$(176)+CHR$(160)
 1280 Cards$(7,5) = R$+CHR$(160)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(160)
 1290 Cards$(7,6) = R$+CHR$(160)+CHR$(171)+CHR$(255)+CHR$(255)+CHR$(167)+CHR$(160)
 1300 Cards$(7,7) = R$+CHR$(160)+CHR$(160)+CHR$(162)+CHR$(161)+CHR$(160)+CHR$(160)
 1310
 1320 REM Card Nine, Frame One, Smiley
 1330 Cards$(8,0) = Y$+CHR$(224)+CHR$(190)+CHR$(239)+CHR$(191)+CHR$(237)+CHR$(176)
 1340 Cards$(8,1) = Y$+CHR$(255)+CHR$(245)+CHR$(250)+CHR$(245)+CHR$(250)+CHR$(255)
 1350 Cards$(8,2) = Y$+CHR$(255)+CHR$(163)+CHR$(163)+CHR$(163)+CHR$(163)+CHR$(255)
 1360 Cards$(8,3) = Y$+CHR$(162)+CHR$(237)+CHR$(252)+CHR$(252)+CHR$(190)+CHR$(161)
 1370
 1380 REM Card Nine, Frame Two, Smiley
 1390 Cards$(8,4) = Y$+CHR$(224)+CHR$(190)+CHR$(255)+CHR$(255)+CHR$(237)+CHR$(176)
 1400 Cards$(8,5) = Y$+CHR$(255)+CHR$(255)+CHR$(254)+CHR$(253)+CHR$(255)+CHR$(255)
 1410 Cards$(8,6) = Y$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1420 Cards$(8,7) = Y$+CHR$(162)+CHR$(237)+CHR$(252)+CHR$(252)+CHR$(190)+CHR$(161)
 1430
 1440 REM Card Ten, Frame One,PlaceHolder
 1450 Cards$(9,0) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1460 Cards$(9,1) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1470 Cards$(9,2) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1480 Cards$(9,3) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1490
 1500 REM Card Ten, Frame Two
 1510 Cards$(9,4) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1520 Cards$(9,5) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1530 Cards$(9,6) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1540 Cards$(9,7) = M$+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)+CHR$(255)
 1550
 1560 DIM CardActive(20)
 1570 DIM CardValue(20)
 1580 Player1%=0
 1590 Player2%=0
 1600 CurrentPlayer%=1
 1610 FirstChoice%=-1
 1620
 1630 PROC_Init
 1640 Selected% = 0
 1650 PROC_DrawSelect(Selected%)
 1660
 1670
 1680 REPEAT
 1690   PROC_DrawActiveCards
 1700   IN% = INKEY(60)
 1710   OldSelected% = Selected%
 1720   IF IN% <> -1 THEN
 1730     IF IN%=114 AND (Player1% + Player2%) = 10 THEN
 1740       PROC_Init
 1750     ELSE
 1760       IF IN%=32 THEN
 1770         IF CardActive(Selected%) <> TRUE THEN
 1780
 1790           CardActive(Selected%) = TRUE
 1800           Card% = CardValue(Selected%)
 1810           PROC_DrawCard(Selected%, Cards$(Card%,0),Cards$(Card%,1),Cards$(Card%,2),Cards$(Card%,3))
 1820
 1830           IF FirstChoice% = -1 THEN
 1840
 1850             FirstChoice% = Selected%
 1860           ELSE
 1870             IF CardValue(FirstChoice%) == CardValue(Selected%) THEN
 1880               PROC_SoundEffect_Correct
 1890               IF CurrentPlayer% = 1 THEN
 1900                 Player1% = Player1% + 1
 1910               ELSE
 1920                 Player2% = Player2% + 1
 1930               ENDIF
 1940               IF Player2% + Player1% = 10 THEN
 1950                 PRINT TAB(12,3) "Press R to Reset!"
 1960               ENDIF
 1970             ELSE
 1980               PROC_TogglePlayer
 1990               PROC_SoundEffect
 2000               WAIT 80
 2010               CardActive(Selected%) = FALSE
 2020               CardActive(FirstChoice%) = FALSE
 2030               PROC_DrawCard(Selected%,Cover1$,Cover2$,Cover3$,Cover4$)
 2040               PROC_DrawCard(FirstChoice%,Cover1$,Cover2$,Cover3$,Cover4$)
 2050
 2060             ENDIF
 2070             FirstChoice% = -1
 2080           ENDIF
 2090           PROC_Draw_Scores
 2100         ENDIF
 2110       ELSE
 2120         IF IN%=136 OR IN%=97 THEN
 2130           Selected% = (Selected% + 19) MOD 20
 2140         ENDIF
 2150         IF IN%=137 OR IN%=100 THEN
 2160           Selected% = (Selected% + 1) MOD 20
 2170         ENDIF
 2180         IF IN%=138 OR IN%=115 THEN
 2190           Selected% = (Selected% + 25) MOD 20
 2200         ENDIF
 2210         IF IN%=139 OR IN%=119 THEN
 2220           Selected% = (Selected% + 15) MOD 20
 2230         ENDIF
 2240       ENDIF
 2250       IF Selected% <> OldSelected% THEN
 2260         PROC_UnDrawSelect(OldSelected%)
 2270         PROC_DrawSelect(Selected%)
 2280       ENDIF
 2290     ENDIF
 2300   ENDIF
 2310 UNTIL 1=0
 2320 END
 2330 DEF PROC_TogglePlayer
 2340 IF CurrentPlayer% = 1 THEN
 2350   CurrentPlayer% = 2
 2360 ELSE
 2370   CurrentPlayer% = 1
 2380 ENDIF
 2390 ENDPROC
 2400 DEF PROC_Draw_Scores
 2410 PRINT TAB(0, 24) TW$"Player 1: "STR$(Player1%);
 2420 PRINT TAB(27,24) TW$"Player 2: "STR$(Player2%);
 2430 IF CurrentPlayer% = 1 THEN
 2440   PRINT TAB(0,24) TG$;
 2450 ELSE
 2460   PRINT TAB(27, 24) TG$;
 2470 ENDIF
 2480 ENDPROC
 2490 DEF PROC_Init
 2500 CLS
 2510 Player1%=0
 2520 Player2%=0
 2530 PROC_SoundEffect_Correct
 2540 PRINT TAB(5,1) TG$CHR$(141)"Sixel-Flip";CHR$(140);"by Andrew McCall"
 2550 PRINT TAB(5,2) TG$CHR$(141)"Sixel-Flip";CHR$(140);"by Andrew McCall"
 2560 PROC_Draw_Scores
 2570 FOR I%=0 TO 19
 2580   PROC_DrawCard(I%,Cover1$,Cover2$,Cover3$,Cover4$)
 2590 NEXT
 2600 FOR I%=3 TO 23
 2610   PRINT TAB(8, I%) W$;
 2620   PRINT TAB(16, I%) W$;
 2630   PRINT TAB(24, I%) W$;
 2640   PRINT TAB(32, I%) W$;
 2650 NEXT
 2660 PROC_RandomiseCards
 2670 ENDPROC
 2680
 2690 DEF PROC_DrawCard(N%, Row0$, Row1$, Row2$, Row3$)
 2700 X% = FN_CardNumberToX(N%)
 2710 Y% = FN_CardNumberToY(N%)
 2720 PRINT TAB(X%, Y%)    Row0$
 2730 PRINT TAB(X%, Y%+1)  Row1$
 2740 PRINT TAB(X%, Y%+2)  Row2$
 2750 PRINT TAB(X%, Y%+3)  Row3$
 2760 ENDPROC
 2770
 2780 DEF FN_CardNumberToX(N%)
 2790 =(8*(N%MOD5))
 2800
 2810 DEF FN_CardNumberToY(N%)
 2820 =4+(5*(N%DIV5))
 2830
 2840 DEF PROC_DrawSelect(N%)
 2850 X% = FN_CardNumberToX(N%)
 2860 Y% = FN_CardNumberToY(N%)
 2870 C$ = CHR$(RND(5) + 144)
 2880 PRINT TAB(X%, Y%-1) C$CHR$(185)CHR$(185)CHR$(185)CHR$(185)CHR$(185)CHR$(185);
 2890 PRINT TAB(X%, Y%+4) C$CHR$(185)CHR$(185)CHR$(185)CHR$(185)CHR$(185)CHR$(185);
 2900 ENDPROC
 2910 DEF PROC_UnDrawSelect(N%)
 2920 X% = FN_CardNumberToX(N%)
 2930 Y% = FN_CardNumberToY(N%)
 2940 PRINT TAB(X%, Y%-1) CHR$(160)CHR$(160)CHR$(160)CHR$(160)CHR$(160)CHR$(160)CHR$(160);
 2950 PRINT TAB(X%, Y%+4) CHR$(160)CHR$(160)CHR$(160)CHR$(160)CHR$(160)CHR$(160)CHR$(160);
 2960 ENDPROC
 2970 DEF PROC_DrawActiveCards
 2980 PRIVATE Frame%
 2990 PRIVATE LastTime%
 3000 IF TIME - LastTime% > 80 THEN
 3010   LastTime% = TIME
 3020   IF Frame% = 0 THEN
 3030     Frame% = 4
 3040   ELSE
 3050     Frame% = 0
 3060   ENDIF
 3070   FOR I%=0 TO 20
 3080     IF CardActive(I%)=TRUE THEN
 3090       Card% = CardValue(I%)
 3100       PROC_DrawCard(I%, Cards$(Card%,0+Frame%),Cards$(Card%,1+Frame%),Cards$(Card%,2+Frame%),Cards$(Card%,3+Frame%))
 3110     ENDIF
 3120   NEXT
 3130
 3140 ENDIF
 3150 ENDPROC
 3160 DEF PROC_SoundEffect
 3170 SOUND 0, -2, 100, 2
 3180 WAIT 1
 3190 SOUND 1, -2, 160, 1
 3200 SOUND 2, -2, 170, 1
 3210 ENDPROC
 3220 DEF PROC_SoundEffect_Correct
 3230 SOUND 1, -2, 188, 1
 3240 SOUND 2, -2, 196, 1
 3250 SOUND 3, -2, 178, 1
 3260 WAIT 4
 3270 SOUND 1, -2, 240, 2
 3280 SOUND 2, -2, 230, 2
 3290 SOUND 3, -2, 220, 2
 3300 SOUND 1, -2, 200, 1
 3310 SOUND 2, -2, 220, 1
 3320 SOUND 3, -2, 210, 1
 3330
 3340 ENDPROC
 3350 DEF PROC_RandomiseCards
 3360
 3370 FOR I%=0 TO 20
 3380   CardValue(I%) = I%MOD10
 3390   CardActive(I%) = FALSE
 3400 NEXT
 3410 FOR I%=0 TO 21
 3420   A% = RND(20)-1
 3430   B% = RND(20)-1
 3440   IF A% = B% THEN I% = I% -1
 3450   Temp% = CardValue(B%)
 3460   CardValue(B%) = CardValue(A%)
 3470   CardValue(A%) = Temp%
 3480 NEXT
 3490 ENDPROC
