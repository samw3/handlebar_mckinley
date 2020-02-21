DECLARE SUB PSprite (cspr%, mode%)
DECLARE SUB DrawT (cspr%)
DECLARE SUB Save ()
DECLARE SUB Load ()
DECLARE SUB BigPset (x%, y%, c%)
DECLARE SUB DColorBox (cc%)
DECLARE SUB Init ()
DECLARE SUB BigPreset (x%, y%)
DECLARE SUB DScreen (s%)
DECLARE SUB CursorPlot (x%, y%)

DEFINT A-Z

TYPE sprtype
  image AS INTEGER  '  Which Image
  mask AS INTEGER '    Which Mask
  x AS INTEGER
  y AS INTEGER
  xhot AS INTEGER
  yhot AS INTEGER
END TYPE

DIM SHARED SprImg(67, 119, 1) AS INTEGER
COMMON SHARED NextKey$, ClipX, ClipY

' Init
cspr = 0: bkg = 8: pal = 0
Init
COLOR bkg, pal
PALETTE 0, 0
PALETTE 1, 7
PALETTE 2, 8
PALETTE 3, 15

' Begin main program
DO
  DO
    CursorPlot cx, cy
    IF NextKey$ = "" THEN
      k$ = INKEY$
    ELSE
      k$ = NextKey$
      NextKey$ = ""
    END IF
  LOOP WHILE k$ = ""
  GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
  GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
  SELECT CASE k$
    CASE "b"
      IF bkg < 15 THEN
        bkg = bkg + 1
      ELSE
        bkg = 0
      END IF
      COLOR bkg, pal
    CASE "B"
      IF bkg > 0 THEN
        bkg = bkg - 1
      ELSE
        bkg = 15
      END IF
      COLOR bkg, pal
    CASE "P"
      PSprite cspr, 1
    CASE "`"
      BigPset cx, cy, 0
      DrawT cspr
    CASE "1"
      BigPset cx, cy, 1
      DrawT cspr
    CASE "2"
      BigPset cx, cy, 2
      DrawT cspr
    CASE "3"
      BigPset cx, cy, 3
      DrawT cspr
    CASE "4"
      BigPset cx, cy, -1
      DrawT cspr
    CASE "0"
      PSET (154 + cx, 2 + cy), 0
      BigPreset cx, cy
      DrawT cspr
    CASE ")"
      FOR x = 0 TO 15
        FOR y = 0 TO 15
          PSET (154 + x, 2 + y), 0
          BigPreset x, y
        NEXT y
      NEXT x
      DrawT cspr
    CASE "$"
      FOR x = 0 TO 15
        FOR y = 0 TO 15
          BigPset x, y, -1
        NEXT y
      NEXT x
      DrawT cspr
    CASE "~"
      LINE (2, 2)-(129, 129), 0, BF
      LINE (134, 2)-(149, 17), 0, BF
      DrawT cspr
    CASE "!"
      LINE (2, 2)-(129, 129), 1, BF
      LINE (134, 2)-(149, 17), 1, BF
      DrawT cspr
    CASE "@"
      LINE (2, 2)-(129, 129), 2, BF
      LINE (134, 2)-(149, 17), 2, BF
      DrawT cspr
    CASE "#"
      LINE (2, 2)-(129, 129), 3, BF
      LINE (134, 2)-(149, 17), 3, BF
      DrawT cspr
    CASE "p"
      IF pal = 0 THEN pal = 1 ELSE pal = 0
      COLOR bkg, pal
    CASE "o"
      IF mon = 0 THEN mon = 1 ELSE mon = 0
      SCREEN 1, mon
      COLOR bkg, pal
      DScreen cspr
      DrawT cspr
    CASE "+"
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      IF cspr < 119 THEN cspr = cspr + 1 ELSE cspr = 0
      DScreen cspr
    CASE "-"
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      IF cspr > 0 THEN cspr = cspr - 1 ELSE cspr = 119
      DScreen cspr
    CASE "L"
      Load
      DScreen cspr
    CASE "S"
      Save
      DScreen cspr
    CASE "q"
      PUT (150, 100), SprImg(0, cspr, 0), PSET
    CASE "w"
      PUT (166, 100), SprImg(0, cspr, 0), PSET
    CASE "e"
      PUT (182, 100), SprImg(0, cspr, 0), PSET
    CASE "a"
      PUT (150, 116), SprImg(0, cspr, 0), PSET
    CASE "s"
      PUT (166, 116), SprImg(0, cspr, 0), PSET
    CASE "d"
      PUT (182, 116), SprImg(0, cspr, 0), PSET
    CASE "z"
      PUT (150, 132), SprImg(0, cspr, 0), PSET
    CASE "x"
      PUT (166, 132), SprImg(0, cspr, 0), PSET
    CASE "c"
      PUT (182, 132), SprImg(0, cspr, 0), PSET
    CASE "h"
      PUT (182, 2), SprImg(0, cspr, 0), PSET
      PUT (202, 2), SprImg(0, cspr, 1), PSET
      FOR x = 0 TO 15
        FOR y = 0 TO 15
          c1 = POINT(182 + x, 2 + y)
          c2 = POINT(202 + x, 2 + y)
          PSET (149 - x, y + 2), c1
          PSET (169 - x, y + 2), c2
        NEXT y
      NEXT x
      LINE (182, 2)-(222, 17), 0, BF
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      DScreen cspr
    CASE "v"
      PUT (182, 2), SprImg(0, cspr, 0), PSET
      PUT (202, 2), SprImg(0, cspr, 1), PSET
      FOR x = 0 TO 15
        FOR y = 0 TO 15
          c1 = POINT(182 + x, 2 + y)
          c2 = POINT(202 + x, 2 + y)
          PSET (134 + x, 17 - y), c1
          PSET (154 + x, 17 - y), c2
        NEXT y
      NEXT x
      LINE (182, 2)-(222, 17), 0, BF
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      DScreen cspr
    CASE "i"
      LINE (134, 0)-(150, 19), 0, BF
      LINE (154, 2)-(169, 17), 3, BF
      PUT (134, 1), SprImg(0, cspr, 0), PSET
      PUT (154, 1), SprImg(0, cspr, 1), PSET
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      LINE (134, 0)-(170, 19), 0, BF
      DScreen cspr
    CASE "m"
      LINE (134, 0)-(150, 19), 0, BF
      LINE (154, 2)-(169, 17), 3, BF
      PUT (134, 3), SprImg(0, cspr, 0), PSET
      PUT (154, 3), SprImg(0, cspr, 1), PSET
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      LINE (134, 0)-(170, 19), 0, BF
      DScreen cspr
    CASE "j"
      LINE (134, 0)-(150, 19), 0, BF
      LINE (154, 2)-(169, 17), 3, BF
      PUT (133, 2), SprImg(0, cspr, 0), PSET
      PUT (153, 2), SprImg(0, cspr, 1), PSET
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      LINE (134, 0)-(170, 19), 0, BF
      DScreen cspr
    CASE "k"
      LINE (134, 0)-(150, 19), 0, BF
      LINE (154, 2)-(169, 17), 3, BF
      PUT (135, 2), SprImg(0, cspr, 0), PSET
      PUT (155, 2), SprImg(0, cspr, 1), PSET
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      LINE (134, 0)-(170, 19), 0, BF
      DScreen cspr
    CASE "f"
      'Flood Fill
      LOCATE 21, 1, 0: PRINT "What color?"
      DO
        zz$ = INKEY$
        zz = INSTR("`1234", zz$) <> 0
      LOOP UNTIL zz <> 0
      zz = zz - 1
      LOCATE 21, 1, 0: PRINT "           "
     
      IF zz <> 4 THEN
        PAINT (x + 134, y + 2), zz
        PAINT (x + 154, y + 2), 0
      ELSE
        PAINT (x + 134, y + 2), 0
        PAINT (x + 154, y + 2), 3
      END IF
    CASE CHR$(9)
      ' Swap
      PUT (182, 2), SprImg(0, cspr, 0), PSET
      PUT (202, 2), SprImg(0, cspr, 1), PSET
      GET (134, 21)-(149, 37), SprImg(0, cspr, 0)
      GET (154, 21)-(169, 37), SprImg(0, cspr, 1)
      PUT (134, 2), SprImg(0, cspr, 0), PSET
      PUT (154, 2), SprImg(0, cspr, 1), PSET
      GET (182, 2)-(197, 17), SprImg(0, cspr, 0)
      GET (202, 2)-(217, 17), SprImg(0, cspr, 1)
      PUT (134, 21), SprImg(0, cspr, 0), PSET
      PUT (154, 21), SprImg(0, cspr, 1), PSET
      GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
      GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
      LINE (182, 2)-(222, 17), 0, BF
      DScreen cspr
  END SELECT
  IF ASC(LEFT$(k$, 1)) = 0 THEN
    k = ASC(RIGHT$(k$, 1))
    BigPreset cx, cy
    SELECT CASE k
    CASE 72
      ' UP
      IF cy = 0 THEN cy = 15 ELSE cy = cy - 1
    CASE 80
      ' Down
      IF cy = 15 THEN cy = 0 ELSE cy = cy + 1
    CASE 75
      ' Left
      IF cx = 0 THEN cx = 15 ELSE cx = cx - 1
    CASE 77
      ' Right
      IF cx = 15 THEN cx = 0 ELSE cx = cx + 1
    CASE 82
      ' Put
      GET (134, 21)-(149, 37), SprImg(0, cspr, 0)
      GET (154, 21)-(169, 37), SprImg(0, cspr, 1)
      PUT (134, 2), SprImg(0, cspr, 0), PSET
      PUT (154, 2), SprImg(0, cspr, 1), PSET
      DScreen cspr
    CASE 83
      ' Get
      PUT (134, 21), SprImg(0, cspr, 0), PSET
      PUT (154, 21), SprImg(0, cspr, 1), PSET
    CASE 71
      cspr = 0
      DScreen cspr
    CASE 79
      cspr = cspr + 10
      DScreen cspr
    CASE 59
      ClipX = cx
      ClipY = cy
      LOCATE 24, 1
      PRINT "X ="; ClipX; " Y ="; ClipY; "         ";
    END SELECT
  END IF
  IF k$ = CHR$(27) THEN
    CLS
    INPUT "Do you want to quit:", yn$
    IF UCASE$(yn$) = "Y" THEN
      done = -1
    ELSE
      CLS
      DScreen cspr
    END IF
  END IF
LOOP UNTIL done

SUB BigPreset (x, y)
  c2 = POINT(x + 154, y + 2)
  IF c2 = 0 THEN
    LINE (x * 8 + 2, y * 8 + 2)-(x * 8 + 9, y * 8 + 9), POINT(x + 134, y + 2), BF
  ELSE
    LINE (x * 8 + 2, y * 8 + 2)-(x * 8 + 9, y * 8 + 9), 0, BF
    LINE (x * 8 + 3, y * 8 + 3)-(x * 8 + 8, y * 8 + 8), 3
    LINE (x * 8 + 8, y * 8 + 3)-(x * 8 + 3, y * 8 + 8), 3
  END IF
END SUB

SUB BigPset (x, y, c)
  IF x > 15 THEN x = 15
  IF y > 15 THEN y = 15
  IF c >= 0 THEN
    LINE (x * 8 + 2, y * 8 + 2)-(x * 8 + 9, y * 8 + 9), c, BF
    PSET (x + 134, y + 2), c
    PSET (x + 154, y + 2), 0
  ELSE
    LINE (x * 8 + 2, y * 8 + 2)-(x * 8 + 9, y * 8 + 9), 0, BF
    LINE (x * 8 + 3, y * 8 + 3)-(x * 8 + 8, y * 8 + 8), 3
    LINE (x * 8 + 8, y * 8 + 3)-(x * 8 + 3, y * 8 + 8), 3
    PSET (x + 134, y + 2), 0
    PSET (x + 154, y + 2), 3
  END IF
END SUB

SUB CursorPlot (x, y)
  STATIC toggle, cnt
  cnt = cnt + 1
  IF cnt < 20 THEN EXIT SUB
  cnt = 0
  IF toggle = 0 THEN toggle = 3 ELSE toggle = 0
  IF x > 15 THEN x = 15
  IF y > 15 THEN y = 15
  LINE (x * 8 + 4, y * 8 + 4)-(x * 8 + 7, y * 8 + 7), toggle, BF
END SUB

SUB DrawT (cspr)
  GET (134, 2)-(149, 17), SprImg(0, cspr, 0)
  GET (154, 2)-(169, 17), SprImg(0, cspr, 1)
  PUT (150, 50), SprImg(0, cspr, 0), PSET
  PUT (150, 66), SprImg(0, cspr, 0), PSET
  PUT (150, 82), SprImg(0, cspr, 0), PSET
  PUT (166, 50), SprImg(0, cspr, 0), PSET
  PUT (166, 66), SprImg(0, cspr, 0), PSET
  PUT (166, 82), SprImg(0, cspr, 0), PSET
  PUT (182, 50), SprImg(0, cspr, 0), PSET
  PUT (182, 66), SprImg(0, cspr, 0), PSET
  PUT (182, 82), SprImg(0, cspr, 0), PSET
  LINE (32, 148)-(48, 168), 0, BF
  LINE (50, 148)-(68, 168), 1, BF
  LINE (70, 148)-(88, 168), 2, BF
  LINE (90, 148)-(108, 168), 3, BF
  PUT (32, 150), SprImg(0, cspr, 0), PSET
  PUT (52, 150), SprImg(0, cspr, 1), AND
  PUT (52, 150), SprImg(0, cspr, 0), OR
  PUT (72, 150), SprImg(0, cspr, 1), AND
  PUT (72, 150), SprImg(0, cspr, 0), OR
  PUT (92, 150), SprImg(0, cspr, 1), AND
  PUT (92, 150), SprImg(0, cspr, 0), OR
END SUB

SUB DScreen (s)
  LINE (0, 0)-(131, 131), 3, B
  LINE (132, 0)-(151, 19), 3, B
  LINE (1, 1)-(130, 130), 0, BF
  LINE (133, 1)-(150, 18), 0, BF
  PUT (134, 2), SprImg(0, s, 0)
  PUT (154, 2), SprImg(0, s, 1), PSET
  DrawT s
  LOCATE 20, 1, 0
  PRINT RIGHT$("0" + MID$(HEX$(s), 1), 2)
  LINE (1, 1)-(130, 130), 0, BF
  FOR x = 0 TO 15
    FOR y = 0 TO 15
      BigPreset x, y
      NextKey$ = INKEY$
      IF LEFT$(NextKey$, 1) = CHR$(0) THEN
        k = ASC(RIGHT$(NextKey$, 1))
        SELECT CASE k
          CASE 79, 82, 83
            EXIT SUB
        END SELECT
      END IF
      IF NextKey$ <> "" THEN IF INSTR("!@#~$+-hvijkmLS" + CHR$(27), NextKey$) THEN EXIT SUB
    NEXT y
  NEXT x
END SUB

SUB Init
  SHARED cspr
  SCREEN 1, 0
  COLOR 8, 0
PALETTE 0, 0
PALETTE 1, 7
PALETTE 2, 8
PALETTE 3, 15
 
  LINE (16, 0)-(31, 15), 3, BF
  FOR i = 0 TO 119
    GET (0, 0)-(15, 15), SprImg(0, i, 0)
    GET (16, 0)-(31, 15), SprImg(0, i, 1)
  NEXT i
  DScreen (s)
END SUB

SUB Load
  CLS
  INPUT "Filename:", file$
  DEF SEG = VARSEG(SprImg(0, 0, 0))
  BLOAD file$, VARPTR(SprImg(0, 0, 0))
  CLS
END SUB

SUB PSprite (cspr, mode)
  c$ = "€€∞∞±±  {}"
  l = 2
  e$ = CHR$(27)
  IF mode = 1 THEN
    LPRINT e$ + "3" + CHR$(11) + e$ + CHR$(15) + e$ + "S0"
    c$ = "€ ∞ ±    x"
    l = 1
  END IF
  FOR y = 0 TO 15
    FOR x = 0 TO 15
      c2 = POINT(x + 154, y + 2)
      IF c2 = 0 THEN
        LPRINT MID$(c$, POINT(x + 134, y + 2) * 2 + 1, 1);
      ELSE
        LPRINT RIGHT$(c$, 1);
      END IF
    NEXT x
    LPRINT
  NEXT y
  IF mode = 1 THEN LPRINT e$ + "@"
  LPRINT
END SUB

SUB PSprite2
  c$ = "€€∞∞±±  {}"
  l = 2
  e$ = CHR$(27)
  IF mode = 1 THEN
    LPRINT e$ + "3" + CHR$(11) + e$ + CHR$(15) + e$ + "S0"
    c$ = "€ ∞ ±    x"
    l = 1
  END IF
  FOR y = 0 TO 15
    FOR x = 0 TO 15
      c2 = POINT(x + 154, y + 2)
      IF c2 = 0 THEN
        LPRINT MID$(c$, POINT(x + 134, y + 2) * 2 + 1, 1);
      ELSE
        LPRINT RIGHT$(c$, 1);
      END IF
    NEXT x
    LPRINT
  NEXT y
  IF mode = 1 THEN LPRINT e$ + "@"
  LPRINT
END SUB

SUB Save
  CLS
  INPUT "Filename:", file$
  PRINT VARSEG(SprImg(0, 0, 0)), VARPTR(SprImg(0, 0, 0))
  DEF SEG = VARSEG(SprImg(0, 0, 0))
  BSAVE file$, VARPTR(SprImg(0, 0, 0)), 32640&: REM 32640& * 2
  CLS
END SUB

