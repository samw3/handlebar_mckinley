DECLARE FUNCTION PickBrk% (st%)
DECLARE FUNCTION PickBrk% (st%)
DECLARE FUNCTION GetPos% (x%, y%, i%, f%)
DECLARE SUB DStatus (n%)
DECLARE FUNCTION PutSwitch% (x%, y%, w%, s%)
DECLARE FUNCTION GetBrk% (x%, y%, w%, s%)
DECLARE FUNCTION GetItem% (x%, y%, w%, s%)
DECLARE SUB PutBrk (x%, y%, w%, s%, i%)
DECLARE SUB PutItem (x%, y%, w%, s%, i%)
DECLARE SUB boop ()
DECLARE SUB ReplaceBrick (x%, y%)
DECLARE SUB DCursor ()
DECLARE FUNCTION GetClick% ()
DECLARE SUB DoTool (tool%)
DECLARE SUB DScreen ()
DECLARE SUB SaveMap ()
DECLARE SUB LoadMap ()
DECLARE SUB DMap (w%, s%)
DECLARE SUB PutSprite (x%, y%, i%)
DECLARE SUB Dec (x%, b%, t%)
DECLARE SUB Inc (x%, b%, t%)
DECLARE SUB delay (x%)
DECLARE SUB Sprite (n%, x%, y%, i%)
DECLARE SUB DSpr (n%)
DECLARE SUB ClrSpr (n%)
DECLARE SUB Load ()
DECLARE SUB Init ()

DEFINT A-Z

TYPE SprType
  image AS INTEGER  '  Which Image
  x AS INTEGER
  y AS INTEGER
  xhot AS INTEGER
  yhot AS INTEGER
  SprOn AS INTEGER
END TYPE

CONST MaxSpr = 31
CONST True = -1
CONST False = 0
DIM SHARED SprImg(67, 119, 1) AS INTEGER
DIM SHARED SprBkg(67, MaxSpr) AS INTEGER
DIM SHARED Spr(MaxSpr) AS SprType
DIM SHARED Map(19, 10, 5, 9)
DIM SHARED MapData(9, 1, 5, 9)
DIM SHARED msg$(30)
COMMON SHARED mb, mx, my, w, s, gpx, gpy

' Init
Init
Load

mb = 0: mx = 0: my = 0: w = 0: s = 0

DScreen
DO
  tool = GetClick
  DoTool (tool)
LOOP

SUB boop
  SOUND 50, 1
END SUB

SUB ClrSpr (n)
  IF Spr(n).SprOn = 0 THEN EXIT SUB
  PUT (Spr(n).x, Spr(n).y), SprBkg(0, n), PSET
  Spr(n).SprOn = 0
END SUB

SUB DCursor
  PUT (mx * 16, my * 16 + 9), SprImg(0, mb + 64, 0), PSET
  delay (1000)
  ReplaceBrick mx, my
  delay (100)
END SUB

SUB Dec (x, b, t)
  IF x = b THEN x = t ELSE x = x - 1
END SUB

SUB delay (x)
  FOR i = 0 TO x
  NEXT i
END SUB

SUB DMap (w, s)
  ClrSpr 0
  sx = 0
  FOR x = 0 TO 19
    sy = 9
    FOR y = 0 TO 10
      ReplaceBrick x, y
    NEXT y
    sx = sx + 16
  NEXT x
END SUB

SUB DoTool (tool)
  SELECT CASE tool
  CASE 1
    mx = mx - 1: IF mx < 0 THEN mx = 19
    my = my + 1: IF my > 10 THEN my = 0
  CASE 2
    my = my + 1: IF my > 10 THEN my = 0
  CASE 3
    mx = mx + 1: IF mx > 19 THEN mx = 0
    my = my + 1: IF my > 10 THEN my = 0
  CASE 4
    mx = mx - 1: IF mx < 0 THEN mx = 19
  CASE 5
  CASE 6
    mx = mx + 1: IF mx > 19 THEN mx = 0
  CASE 7
    mx = mx - 1: IF mx < 0 THEN mx = 19
    my = my - 1: IF my < 0 THEN my = 10
  CASE 8
    my = my - 1: IF my < 0 THEN my = 10
  CASE 9
    mx = mx + 1: IF mx > 19 THEN mx = 0
    my = my - 1: IF my < 0 THEN my = 10
  CASE 10: REM 0
    ' Remove Item
    g = GetItem(mx, my, w, s)
    IF g = 15 OR g = 14 THEN
      ' Remove Switch
      coor = mx * 16 + my
      FOR i = 0 TO 10
        IF (MapData(i, 0, w, s) MOD 512) = coor THEN EXIT FOR
      NEXT i
      IF i <> 10 THEN MapData(i, 0, w, s) = 0 ELSE BEEP
    END IF
    PutItem mx, my, w, s, 0
  CASE 11: REM .
    boop
  CASE 12: REM +
    mb = mb + 1: IF mb > 32 THEN mb = -32
  CASE 13: REM -
    mb = mb - 1: IF mb < -32 THEN mb = 32
  CASE 14: REM " "
    z = True
    IF mb = -15 OR mb = -14 THEN
      'Put a Switch
      z = PutSwitch(mx, my, w, s)
      IF z = False THEN boop
      DStatus -1
    END IF
    IF z THEN
      IF mb < 0 THEN
        PutItem mx, my, w, s, ABS(mb)
      ELSE
        PutBrk mx, my, w, s, mb
        PutItem mx, my, w, s, 0
      END IF
    END IF
  CASE 15: REM L
    LoadMap
    DScreen
  CASE 16: REM S
    SaveMap
    DScreen
  CASE 17: REM q
    w = w - 1
    IF w < 0 THEN w = 5
    DScreen
  CASE 18: REM w
    w = w + 1
    IF w > 5 THEN w = 0
    DScreen
  CASE 19: REM a
    s = s - 1
    IF s < 0 THEN s = 9
    DScreen
  CASE 20: REM s
    s = s + 1
    IF s > 9 THEN s = 0
    DScreen
  CASE 21: REM R
    Load
  CASE 22: REM r
    ' Draw Rectangle
    ' no switches allowed
    IF mb = -14 OR mb = -15 THEN
      boop
    ELSE
      ok = GetPos(mx, my, mb, 0)
      IF ok THEN
        FOR i = mx TO gpx
          FOR j = my TO gpy
            IF mb < 0 THEN
              PutItem i, j, w, s, ABS(mb)
            ELSE
              PutBrk i, j, w, s, mb
            END IF
            ReplaceBrick i, j
          NEXT j
        NEXT i
        mx = gpx: my = gpy
      ELSE
        boop
      END IF
    END IF
  CASE 23: REM z
    zM = GetBrk(mx, my, w, s)
    IF zM = 0 THEN
      mb = GetItem(mx, my, w, s)
    ELSE
      mb = zM
    END IF
  CASE 24: REM esc
    CLS
    INPUT "Are you sure you want to QUIT?!"; yn$
    IF LCASE$(yn$) = "y" THEN CLS : END
    CLS
    DScreen
  END SELECT
END SUB

SUB DScreen
  DStatus -1
  DMap w, s
END SUB

SUB DSpr (n)
  x = Spr(n).x
  y = Spr(n).y
  i = Spr(n).image
  hx = Spr(n).xhot
  hy = Spr(n).yhot
  GET (x, y)-(x + 15, y + 15), SprBkg(0, n)
  PUT (x, y), SprImg(0, i, 1), AND
  PUT (x, y), SprImg(0, i, 0), OR
END SUB

SUB DStatus (n)
  IF n = -1 THEN
    LOCATE 1, 1: PRINT "World:"; w + 1; "  Stage:"; s + 1; "                    "
  ELSE
    LOCATE 1, 1, 0: PRINT "Pick(" + CHR$(27) + "/" + CHR$(26) + "):" + msg$(n)
  END IF
END SUB

FUNCTION GetBrk (x, y, w, s)
  GetBrk = Map(x, y, w, s) MOD 256
END FUNCTION

FUNCTION GetClick
  DO
    DO
      DCursor
      k$ = INKEY$
    LOOP UNTIL k$ <> ""
    k = INSTR("1234567890.+- LSqwasRrz" + CHR$(27), k$)
    IF k = 0 THEN boop
  LOOP UNTIL k <> 0
  GetClick = k
END FUNCTION

FUNCTION GetItem (x, y, w, s)
  GetItem = Map(x, y, w, s) \ 256
END FUNCTION

FUNCTION GetPos (ix, iy, i, f)
  x = ix: y = iy
  ox = x: oy = y
  Done = 0
  LockX = (f AND 1) = 1
  LockY = (f AND 2) = 2
  DO
    DO
      DO
        PUT (ox * 16, oy * 16 + 9), SprImg(0, i + 64, 0), PSET
        delay (1000)
        ReplaceBrick ox, oy
        delay (100)
        PUT (x * 16, y * 16 + 9), SprImg(0, i + 64, 0), PSET
        delay (500)
        ReplaceBrick x, y
        delay (50)
        k$ = INKEY$
      LOOP UNTIL k$ <> ""
      k = INSTR("123456789 " + CHR$(27), k$)
      IF k = 0 THEN boop
    LOOP UNTIL k <> 0
    IF NOT LockY THEN
      SELECT CASE k
        CASE 1
          y = y + 1: IF y > 10 THEN y = 0
        CASE 2
          y = y + 1: IF y > 10 THEN y = 0
        CASE 3
          y = y + 1: IF y > 10 THEN y = 0
        CASE 7
          y = y - 1: IF y < 0 THEN y = 10
        CASE 8
          y = y - 1: IF y < 0 THEN y = 10
        CASE 9
          y = y - 1: IF y < 0 THEN y = 10
      END SELECT
    END IF
    IF NOT LockX THEN
      SELECT CASE k
        CASE 1
          x = x - 1: IF x < 0 THEN x = 19
        CASE 3
          x = x + 1: IF x > 19 THEN x = 0
        CASE 4
          x = x - 1: IF x < 0 THEN x = 19
        CASE 6
          x = x + 1: IF x > 19 THEN x = 0
        CASE 7
          x = x - 1: IF x < 0 THEN x = 19
        CASE 9
          x = x + 1: IF x > 19 THEN x = 0
      END SELECT
    END IF
    SELECT CASE k
      CASE 1
      CASE 2
      CASE 3
      CASE 4
      CASE 5
      CASE 6
      CASE 7
      CASE 8
      CASE 9
      CASE 10
        Done = -2
      CASE ELSE
        Done = -1
    END SELECT
  LOOP UNTIL Done < 0
  gpx = x: gpy = y
  GetPos = Done + 1
END FUNCTION

SUB Inc (x, b, t)
  IF x = t THEN x = b ELSE x = x + 1
END SUB

SUB Init
  SHARED cspr
  SCREEN 1, 0
  COLOR 0, 1
  PALETTE 0, 0
  PALETTE 1, 7
  PALETTE 2, 8
  PALETTE 3, 15
END SUB

SUB Load
  file$ = "macmap.spr"
  DEF SEG = VARSEG(SprImg(0, 0, 0))
  BLOAD file$, VARPTR(SprImg(0, 0, 0))
END SUB

SUB LoadMap
  CLS
  INPUT "Enter Filename:", file$
  DEF SEG = VARSEG(Map(0, 0, 0, 0))
  BLOAD file$ + ".map", VARPTR(Map(0, 0, 0, 0))
  DEF SEG = VARSEG(MapData(0, 0, 0, 0))
  BLOAD file$ + ".dat", VARPTR(MapData(0, 0, 0, 0))
  FOR x = 0 TO 9
    PRINT (MapData(x, 0, 1, 2) MOD 512) \ 16; " ";
    PRINT (MapData(x, 0, 1, 2) MOD 512) MOD 16; " ";
    PRINT (MapData(x, 0, 1, 2) \ 512) - 1
  NEXT x
  INPUT x
  CLS
END SUB

FUNCTION PickBrk (st)
  b = st
  LINE (135, 76)-STEP(48, 48), 0, BF
  LOCATE 11, 19: PRINT "Pick"
  Done = False: Abort = False
  DO
    DO
      PUT (151, 92), SprImg(0, b + 64, 0), PSET
      DO
        k$ = INKEY$
      LOOP UNTIL k$ <> ""
      k = INSTR("64 " + CHR$(27), k$)
      IF k = 0 THEN boop
    LOOP UNTIL k <> 0
    SELECT CASE k
      CASE 1: REM 6
        b = b + 1: IF b > 32 THEN b = -32
      CASE 2: REM 4
        b = b - 1: IF b < -32 THEN b = 32
      CASE 3: REM " "
        Done = True
      CASE 4: REM ESC
        Done = True
        Abort = True
    END SELECT
  LOOP UNTIL Done
  IF Abort THEN PickBrk = 255 ELSE PickBrk = b
  DScreen
END FUNCTION

SUB PutBrk (x, y, w, s, i)
  Map(x, y, w, s) = ((Map(x, y, w, s) \ 256) * 256) + (i MOD 256)
END SUB

SUB PutItem (x, y, w, s, i)
  Map(x, y, w, s) = (Map(x, y, w, s) MOD 256) + (i MOD 256) * 256
END SUB

SUB PutSprite (x, y, i)
'  PRINT x, y, i
  PUT (x, y), SprImg(0, i, 0), PSET
END SUB

' MapData(index,0,w,s)=d
' MapData(index,1,w,s)=z
' ----------------
' d B0-B3  - Y Coor (0-15)
'   B4-B8  - X Coor (0-31)
'   B9-B15 - Switch Type (See Below) (0=free)
'
' w        - World Number (0-5)
' s        - Stage Number (0-9)
'
' z B0-B14 - Switch Data (also See Below)
'   B15    - Switch On or Off
FUNCTION PutSwitch (x, y, w, s)
  ' Initialize
  m = -1
  '=-=-=-=-=-=-=-=-=-=- Switch Types -=-=-=-=-=-=-=-=-
  '                     012345678901234567890123456789
  m = m + 1: msg$(m) = "Warp Link to Circular        "
  m = m + 1: msg$(m) = "Rotate Cannons 90 deg. Left  "
  m = m + 1: msg$(m) = "Rotate Cannons 90 deg. Right "
  m = m + 1: msg$(m) = "Rotate Cannons 180 deg.      "
  m = m + 1: msg$(m) = "Rotate All Big Cannons       "
  m = m + 1: msg$(m) = "Swap all Teeter-Totters      "
  m = m + 1: msg$(m) = "Swap Brick on X Axis         "
  m = m + 1: msg$(m) = "Swap Brick on Y Axis         "
  m = m + 1: msg$(m) = "Swap All of one kind of Brick"
  m = m + 1: msg$(m) = "Reset all Switches           "
  m = m + 1: msg$(m) = "Shoot All Big Cannons        "
  m = m + 1: msg$(m) = "Shoot One Big Cannon         "
  m = m + 1: msg$(m) = "Shoot All Small Cannons      "
  m = m + 1: msg$(m) = "Swap Brick once on X Axis    "
  m = m + 1: msg$(m) = "Swap Brick once on Y Axis    "
  m = m + 1: msg$(m) = "Rotate U/D Big Cannons       "
  m = m + 1: msg$(m) = "Rotate L/R Big Cannons       "
  '=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  n = m
  '-=-=-=-=-=-=-=-=-=-= Second Values =-=-=-=-=-=-=-=-
  m = m + 1: msg$(m) = "Pick the Brick to Swap From  "
  m = m + 1: msg$(m) = "Pick the Brick to Swap To    "
  m = m + 1: msg$(m) = "Pick the Cannon to Shoot     "
  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 
 
  ' New switch or edit an old one
  edit = False
  coor = x * 16 + y
  FOR i = 0 TO 9
    IF ((MapData(i, 0, w, s) MOD 512) = coor) AND (MapData(i, 0, w, s) \ 512 <> 0) THEN
      edit = True: EXIT FOR
    END IF
  NEXT i
  IF NOT edit THEN
    ' Check if there is enough Switches left in the level to make a new one
    enough = False
    FOR i = 0 TO 9
      IF MapData(i, 0, w, s) = 0 THEN enough = True: EXIT FOR
    NEXT i
    IF NOT enough THEN PutSwitch = False: EXIT FUNCTION
    PutSwitch = True
  END IF
  ' Yes! Let's continue
  IF edit THEN
    m = (MapData(i, 0, w, s) \ 512) - 1
  ELSE
    m = 0
  END IF
  DStatus m
  DO
    DO: k$ = INKEY$: LOOP WHILE k$ = ""
    IF k$ = "6" THEN
      m = m + 1
      IF m > n THEN m = 0
    END IF
    IF k$ = "4" THEN
      m = m - 1
      IF m < 0 THEN m = n
    END IF
    ' ESC Quit Proc
    IF k$ = CHR$(27) THEN PutSwitch = False: EXIT FUNCTION
    IF k$ = " " THEN EXIT DO
    DStatus m
  LOOP
  MapData(i, 0, w, s) = coor + (m + 1) * 512
  ' Get Secondary Values
  SELECT CASE m
    CASE 6
      'Swap on X Axis
      ' i B0-B7 XCoor; B8-B14 Brk to swap
      ok = GetPos(x, y, -16, 2)
      b1 = PickBrk(0)
      IF b1 = 255 THEN PutSwitch = False: EXIT FUNCTION
      z = b1 * 256 + gpx
      MapData(i, 1, w, s) = z
    CASE 7
      'Swap on Y Axis
      ' i B0-B7 YCoor; B8-B14 Brk to swap
      ok = GetPos(x, y, -16, 1)
      b1 = PickBrk(0)
      IF b1 = 255 THEN PutSwitch = False: EXIT FUNCTION
      z = b1 * 256 + gpy
      MapData(i, 1, w, s) = z
    CASE 8
      'Swap All between 2 bricks
      ' i B0-B7 Brk to Swap from; B8-B14 Brk to Swap to
      b1 = 0
      b2 = 0
      DStatus (n + 1)
      b1 = PickBrk(b1)
      IF b1 = 255 THEN PutSwitch = False: EXIT FUNCTION
      DStatus (n + 2)
      b2 = PickBrk(0)
      IF b2 = 255 THEN PutSwitch = False: EXIT FUNCTION
      z = b2 * 256 + b1
      MapData(i, 1, w, s) = z
    CASE 11
      ' Shoot One Cannon
      DO
        ok = GetPos(x, y, -16, 0)
        IF NOT ok THEN PutSwitch = False: EXIT FUNCTION
        g = GetBrk(gpx, gpy, w, s)
        IF g = 3 OR g = 20 OR g = 21 OR g = 22 THEN EXIT DO
        boop
      LOOP
      z = gpx * 16 + gpy
      MapData(i, 1, w, s) = z
    CASE 13
      'Swap on X Axis
      ' i B0-B7 XCoor; B8-B14 Brk to swap
      ok = GetPos(x, y, -16, 2)
      b1 = PickBrk(0)
      IF b1 = 255 THEN PutSwitch = False: EXIT FUNCTION
      z = b1 * 256 + gpx
      MapData(i, 1, w, s) = z
    CASE 14
      'Swap on Y Axis
      ' i B0-B7 YCoor; B8-B14 Brk to swap
      ok = GetPos(x, y, -16, 1)
      b1 = PickBrk(0)
      IF b1 = 255 THEN PutSwitch = False: EXIT FUNCTION
      z = b1 * 256 + gpy
      MapData(i, 1, w, s) = z
  END SELECT
END FUNCTION

SUB ReplaceBrick (x, y)
  PUT (x * 16, y * 16 + 9), SprImg(0, GetBrk(x, y, w, s) + 64, 0), PSET
  zz = GetItem(x, y, w, s)
  IF zz <> 0 THEN
    PUT (x * 16, y * 16 + 9), SprImg(0, 64 - zz, 1), AND
    PUT (x * 16, y * 16 + 9), SprImg(0, 64 - zz, 0), XOR
  END IF
END SUB

SUB SaveMap
  CLS
  INPUT "Enter Filename:", file$
  DEF SEG = VARSEG(Map(0, 0, 0, 0))
  BSAVE file$ + ".map", VARPTR(Map(0, 0, 0, 0)), 26400
  DEF SEG = VARSEG(MapData(0, 0, 0, 0))
  BSAVE file$ + ".dat", VARPTR(MapData(0, 0, 0, 0)), 2400
END SUB

SUB Sprite (n, x, y, i)
  IF Spr(n).SprOn THEN PUT (Spr(n).x, Spr(n).y), SprBkg(0, n), PSET
  GET (x, y)-STEP(15, 15), SprBkg(0, n)
  PUT (x, y), SprImg(0, i, 1), AND
  PUT (x, y), SprImg(0, i, 0), XOR
  Spr(n).image = i
  Spr(n).x = x
  Spr(n).y = y
  Spr(n).SprOn = -1
END SUB

