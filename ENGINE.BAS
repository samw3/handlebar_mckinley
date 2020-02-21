DECLARE SUB SObjRoll (x%, y%, f%, zd&)
DECLARE SUB SwapAllBrk (b1%, b2%)
DECLARE SUB InObj (n%)
DECLARE SUB OutObj (n%)
DECLARE SUB DoSwitch (x%, y%)
DECLARE SUB ShootSmallCannon (x%, y%)
DECLARE SUB DoSObjLandOn (x%, y%, o%)
DECLARE FUNCTION CheckIfWin% ()
DECLARE SUB DoSObjWarp (x%, y%, zz%)
DECLARE SUB PushSObj ()
DECLARE SUB PushSteelie ()
DECLARE SUB TurnObjAround (i%)
DECLARE SUB Splash (sp%, x%, y%)
DECLARE SUB CalculateWaitTime ()
DECLARE SUB UAllObj ()
DECLARE SUB DoAnim (Seq%, w%)
DECLARE SUB UAnim ()
DECLARE FUNCTION Hex4% (v$)
DECLARE FUNCTION Hex2% (v$)
DECLARE FUNCTION Hex1% (v$)
DECLARE SUB SteelieRoll (x%, y%, f%, zd&)
DECLARE SUB Roll (f%)
DECLARE SUB Slip (f%)
DECLARE SUB DoLandOn (zd&)
DECLARE SUB Jump (f%, zd&)
DECLARE SUB walk (f%)
DECLARE SUB Push (f%, zd&)
DECLARE SUB Stand (f%)
DECLARE FUNCTION ChkAheadBrk% ()
DECLARE SUB DoWarp ()
DECLARE SUB ChkCannon ()
DECLARE SUB Inc (x%, b%, t%)
DECLARE FUNCTION GetBrickType% (x%, y%)
DECLARE SUB DoSound (snd%)
DECLARE SUB Delay (x&)
DECLARE SUB DoPlayer ()
DECLARE SUB DoEnemy ()
DECLARE FUNCTION ChkKey% ()
DECLARE SUB Uplayer ()
DECLARE FUNCTION GetItem% (x%, y%)
DECLARE SUB InitLevel ()
DECLARE SUB Intro ()
DECLARE SUB boop ()
DECLARE SUB ReplaceBrick (x%, y%)
DECLARE FUNCTION GetClick% ()
DECLARE SUB LoadMap (file$)
DECLARE SUB Init ()
DECLARE SUB PutBrick (x%, y%, b%)
DECLARE FUNCTION GetBrick% (x%, y%)
DECLARE SUB PutItem (x%, y%, i%)
DECLARE SUB ClrObj (n%)
DECLARE SUB Objite (n%, x%, y%, i%)
DECLARE SUB SetObj (n%, x%, y%, i%)
DECLARE SUB ClrAllObj ()
DECLARE SUB DAllObj ()
DECLARE SUB DObj (n%)
DECLARE SUB LoadImg (file$)

DEFINT A-Z

COMMON SHARED w, s, SoundOn, WaitTime&, RollDir, GameOver
CLEAR , , 16384

TYPE ObjType
  Frame AS INTEGER  '  Which Image
  mx AS INTEGER     '  Map Coors
  my AS INTEGER
  sx AS INTEGER     '  Screen Coors
  sy AS INTEGER
  ox AS INTEGER     '  Old Screen Coors
  oy AS INTEGER
  oFrame AS INTEGER '  Old Anim Frame
  ObjOn AS INTEGER
  Update AS INTEGER ' Update Flag
  f AS INTEGER      '  Facing.. 1up,2dn,3lt,4rt
  t AS INTEGER      '  Obj Type.. 0Player,1Steelie,2Weight,3Switch
  evt AS INTEGER    '  Switch event number.
  item AS INTEGER   '  Holding a Map Item 0=None
END TYPE

TYPE PlayerT
  'PowerUP?
  k AS INTEGER
  'Will he move automatically next DoPlayer?
  WillMove AS INTEGER
  Standing AS INTEGER
  Dead AS INTEGER
  Holding AS INTEGER
END TYPE

TYPE LevelT
  'Player Starting Coors
  psx AS INTEGER
  psy AS INTEGER
  'Exit Coors
  ex AS INTEGER
  ey AS INTEGER
  'Exit Objite Frame
  ef AS INTEGER
  'Level is Complete
  Won AS INTEGER
  'Warp type 0Link 1Circular
  wt AS INTEGER
  'Hole Info
  MaxHole AS INTEGER
  HoleCnt AS INTEGER
END TYPE

TYPE AnimType
  Seq AS INTEGER
  inUse AS INTEGER
  Delay AS INTEGER
  p AS INTEGER
  o AS INTEGER
  f AS INTEGER
END TYPE

CONST MaxObj = 20
CONST MaxAnim = 10
CONST MaxSeq = 49
CONST True = -1
CONST Flase = 0

DIM SHARED ObjImg(67, 119, 1) AS INTEGER
DIM SHARED ObjBkg(67, MaxObj) AS INTEGER
DIM SHARED Obj(MaxObj) AS ObjType
DIM SHARED Map(19, 10, 5, 9)
DIM SHARED MapData(9, 1, 5, 9)
DIM SHARED Warp(9, 1)
DIM SHARED Anim(MaxAnim) AS AnimType
DIM SHARED Seq$(MaxSeq)
DIM SHARED Player AS PlayerT
DIM SHARED Level AS LevelT


CONST debug = -1

' Init Game
Init
LoadImg "mac.spr"

SoundOn = False
SoundOn = True

w = 0: s = 0

' Read Sequence Data
RESTORE AnimData
FOR i = 0 TO MaxSeq
  READ Seq$(i)
NEXT i

CalculateWaitTime

' Main Game Routine
IF NOT debug THEN Intro

GameOver = False

DO
  LoadMap "mac"
  IF debug THEN
    CLS
    INPUT "World"; w: w = w - 1
    INPUT "Stage"; s: s = s - 1
  END IF
  CLS
  IF NOT debug THEN
    LOCATE 10, 15: PRINT "World "; MID$(STR$(w + 1), 2, 1); "-"; MID$(STR$(s + 1), 2)
    SLEEP 3
  END IF
  ' Setup and Draw Level
  InitLevel

  Player.Dead = False
  DO
    'Checks things like cannons
    DoEnemy

    'Work with Player
    DoPlayer

    UAnim
  LOOP UNTIL Player.Dead OR Level.Won
  ClrAllObj
  IF Level.Won THEN
    s = s + 1
    IF s = 10 THEN
      s = 0
      w = w + 1
    END IF
  END IF
LOOP UNTIL GameOver
SYSTEM

'Animation Sequence Data
'  D are in 4-digit hex
'  # are in 2-digit hex
'  S are 1-digit hex
'
'IN OP  Desc
'-- --  ----
'u  S   Moves # Pixels up
'd  S   Moves # Pixels down
'l  S   Moves # Pixels left
'r  S   Moves # Pixels right
'f  #   Displays Anim Frame #
'w  S   Wait S tenths of a second
'U  S   Moves # Tiles up
'D  S   Moves # Tiles down
'L  S   Moves # Tiles left
'R  S   Moves # Tiles right
'o  S   Select Object Number # 0Player,1Effect,2Effect,3Mover
's      Set the Current Object down
'p      Pick Up Current Obect off the map
'c  #   Call Anim Routine #
'x  #   Do sfx #
'v  S   Set Visibility 0Hide 1Show
't? S S Transfer Properties from S to S
' x     Xcoor
' y     Ycoor
' p     Position
' f     Anim frame
'a? S D Assign S's Property to D
' x     Xcoor
' y     Ycoor
       
AnimData:
' Walk Anims
DATA "o0f03u4x00w7 f04u4x00w7 f05u4x00w7 f04u4x00w7U1"
DATA "o0f00d4x00w7 f01d4x00w7 f02d4x00w7 f01d4x00w7D1"
DATA "o0f09l4x00w7 f0al4x00w7 f0bl4x00w7 f0al4x00w7L1"
DATA "o0f06r4x00w7 f07r4x00w7 f08r4x00w7 f07r4x00w7R1"
' Roll Anims
DATA "o0f61u4x00w4 f62u4x00w4 f63u4x00w6 f64u4x00w6 U1"
DATA "o0f65d4x00w4 f66d4x00w4 f67d4x00w6 f68d4x00w6 D1"
DATA "o0f6dl4x00w4 f6el4x00w4 f6fl4x00w6 f70l4x00w6 L1"
DATA "o0f69r4x00w4 f6ar4x00w4 f6br4x00w6 f6cr4x00w6 R1"
' Slip Anims
DATA "o0f26u4x0dw5 f27u4w5 f26u4w5 f27u4w5 U1"
DATA "o0f24d4x0dw5 f25d4w5 f24d4w5 f25d4w5 D1"
DATA "o0f22l4x0dw5 f23l4w5 f22l4w5 f23l4w5 L1"
DATA "o0f1er4x0dw5 f1fr4w5 f1er4w5 f1fr4w5 R1"
' Jump Anims
DATA "o1v0tp01u2f2bv1 o0u4f2cw2 o1u3o0f19u7w2 o1u2o0u6w2 o1u3o0u5w2 o1u2o0u4w2 o1u3o0u4w2 U1 o1u2o0u2w2 o1u3o0u1w2 o1u3w2 o1u2o0u1w2 o1u3o0d1w2 o1v0o0d1w2 U1"
DATA "o1v0tp01u2f2bv1 o0u1f18w2 o1d3o0f2du1w2 o1d2w2 o1d3o0d1w2 o1d2o0d1w2 o1d3o0d2w2 D1 o1d2o0d4w2 o1d3o0d4w2 o1d3o0d5w2 o1d2o0d6w2 o1u3o0d7w2 o1v0o0d4w2 D1"
DATA "o1v0tp01u2f2bv1 o1l2o0f1bw2f20l2u4w2 o1l3o0l3u4w2 o1l2o0l2u4w2 o1l3o0l3u2w2 o1l2o0l2u2w2 o1l3o0f21l3u1w2 L1 o1l2o0l2d1w2 o1l3o0f22l3d2w2 o1l2o0l2d2w2 o1l3o0l3d4w2 o1l2o0l2d4w2 o1v0o0l3d4w1 f1bl2w1 L1"
DATA "o1v0tp01u2f2bv1 o1r2o0f1aw2f1cr2u4w2 o1r3o0r3u4w2 o1r2o0r2u4w2 o1r3o0r3u2w2 o1r2o0r2u2w2 o1r3o0f1dr3u1w2 R1 o1r2o0r2d1w2 o1r3o0f1er3d2w2 o1r2o0r2d2w2 o1r3o0r3d4w2 o1r2o0r2d4w2 o1v0o0r3d4w1 f1ar2w1 R1"
' Push Against
DATA "o0f19w8"
DATA "o0f18w8"
DATA "o0f1bw8"
DATA "o0f1aw8"
' Stand
DATA "o0f04"
DATA "o0f01"
DATA "o0f0a"
DATA "o0f07"
' Run Anims
DATA "o0f03u4x00w4 f04u4x00w2 f05u4x00w1 f04u4x00w1U1"
DATA "o0f00d4x00w4 f01d4x00w2 f02d4x00w1 f01d4x00w1D1"
DATA "o0f09l4x00w4 f0al4x00w2 f0bl4x00w1 f0al4x00w1L1"
DATA "o0f06r4x00w4 f07r4x00w2 f08r4x00w1 f07r4x00w1R1"
' Warp
DATA "x10tp01tf01o1v1o0f28w2 f29w2 f2aw2 o1v0 o0f2aw2 o0f29w2 f28w2 v0w8"
DATA "x11tp01o0f28v1w2 f29w2 f2aw2 o1v1w2 f29w2 f28w2 tf10o1v0"
' Splash
DATA "x0e o1v1 f77w8 f76w8 f75w8 f74w8 f77w8 f76wa f75wc f74wf v0"
' Drink Ketchup
DATA "o0f2ftp01u8 o1v1f72w8x02wfwfwfwfwf v0tp10 o0f2dx09wfwf"
' Push
DATA "o0f19x01u4w6f58x01u4w6f19x01u4w6f58x01u4w6U1"
DATA "o0f18x01d4w6f57x01d4w6f18x01d4w6f57x01d4w6D1"
DATA "o0f1bx01l4w6f5ax01l4w6f1bx01l4w6f5ax01l4w6L1"
DATA "o0f1ax01r4w6f59x01r4w6f1ax01r4w6f59x01r4w6R1"
' Obj Move
DATA "o3v1u4w6u4w6u4w8u4wav0"
DATA "o3v1d4w6d4w6d4w8d4wav0"
DATA "o3v1l4w6l4w6l4w8l4wav0"
DATA "o3v1r4w6r4w6r4w8r4wav0"
' Obj Bounce
DATA "x04o2tp32u2f2bv1 o3v1u4w2 o2u3o3u7w2 o2u2 o3u6w2 o2u3 o3u5w2 o2u2 o3u4w2 o2u3 o3u4w2 U1 o2u2 o3u2w2 o2u3 o3u1w2 o2u3w2u2 o3u1w2 o2u3 o3d1w2v0 o2v0 o3v1d1w2 U1"
DATA "x04o2tp32u2f2bv1 o3v1u1w2 o2d3o3u1w2 o2d2w2 o2d3 o3d1w2 o2d2 o3d1w2 o2d3 o3d2w2 D1 o2d2 o3d4w2 o2d3 o3d4w2 o2d3 o3d5w2 o2d2 o3d6w2 o2u3 o3d7w2v0 o2v0 o3v1d4w2 D1"
DATA "x04o2tp32u2f2bv1l2 o3v1w2l2u4w2 o2l3 o3l3u4w2 o2l2 o3l2u4w2 o2l3 o3l3u2w2 o2l2 o3l2u2w2 o2l3 o3l3u1w2 L1 o2l2 o3l2d1w2 o2l3 o3l3d2w2 o2l2 o3l2d2w2 o2l3 o3l3d4w2 o2l2 o3l2d4w2v0 o2v0 o3v1l3d4w1l2w1 L1"
DATA "x04o2tp32u2f2bv1r2 o3v1w2r2u4w2 o2r3 o3r3u4w2 o2r2 o3r2u4w2 o2r3 o3r3u2w2 o2r2 o3r2u2w2 o2r3 o3r3u1w2 R1 o2r2 o3r2d1w2 o2r3 o3r3d2w2 o2r2 o3r2d2w2 o2r3 o3r3d4w2 o2r2 o3r2d4w2v0 o2v0 o3v1r3d4w1r2w1 R1"
' Obj Warp
DATA "x10tp32tf32o2v1o3f28v1w2f29w2f2aw2o2v0o3f2aw2f29w2f28w2v0w2"
DATA "x11tp32o3f28v1w2f29w2f2aw2o2v1w2f29w2f28w2tf23o2v0o3v0"
' Cannon Shoot
DATA "o4f36u4w2u4w2u4w2u4w2"
DATA "o4f36d4w2d4w2d4w2d4w2"
DATA "o4f36l4w2l4w2l4w2l4w2"
DATA "o4f36r4w2r4w2r4w2r4w2"
' Pick Up
' Drop
' Hold & Walk
' Hold & Jump

SUB boop
  SOUND 300, 1
END SUB

SUB CalculateWaitTime
  ' x = VAL(MID$(TIME$, 7, 2))
  ' y& = 0
  ' z = VAL(MID$(TIME$, 7, 2))
  ' WHILE z = x
  '  z = VAL(MID$(TIME$, 7, 2))
  ' WEND
  ' IF (z < x) THEN x = -1
  ' WHILE z = x + 1
  '   WaitTime& = WaitTime& + 1
  '   z = VAL(MID$(TIME$, 7, 2))
  ' WEND
  ' WaitTime& = WaitTime& * 130
  WaitTime& = 200
END SUB

FUNCTION CheckIfWin
  zz = (Level.MaxHole = Level.HoleCnt)
  IF zz THEN
    ' Show the Exit
    DoSObjWarp Level.ex, Level.ey, Level.ef
  END IF
END FUNCTION

'Check The Brick Ahead of the Player 1UP 2DN 3LT 4RT
'
FUNCTION ChkAheadBrk
  SELECT CASE Obj(0).f
    CASE 1
      ChkAheadBrk = GetBrickType(Obj(0).mx, Obj(0).my - 1)
    CASE 2
      ChkAheadBrk = GetBrickType(Obj(0).mx, Obj(0).my + 1)
    CASE 3
      ChkAheadBrk = GetBrickType(Obj(0).mx - 1, Obj(0).my)
    CASE 4
      ChkAheadBrk = GetBrickType(Obj(0).mx + 1, Obj(0).my)
  END SELECT
END FUNCTION

SUB ChkCannon
  FOR n = 0 TO 19
    b = GetBrickType(n, Obj(0).my)
    IF b = 18 THEN ShootSmallCannon n, Obj(0).my
  NEXT n
  FOR i = 0 TO 10
    b = GetBrickType(Obj(0).mx, i)
    IF b = 18 THEN ShootSmallCannon Obj(0).mx, i
  NEXT i
END SUB

FUNCTION ChkKey STATIC
  k = 0
  k$ = INKEY$
  IF debug THEN IF k$ = CHR$(27) THEN END
  IF debug THEN IF k$ = "w" THEN Roll Obj(0).f: DoLandOn zd&
  IF k$ = "" THEN ChkKey = k: EXIT FUNCTION
  IF ASC(k$) = 0 THEN
    sc = ASC(RIGHT$(k$, 1))
    SELECT CASE sc
      CASE 72
        k = 1
      CASE 80
        k = 2
      CASE 75
        k = 3
      CASE 77
        k = 4
    END SELECT
  ELSE
    SELECT CASE k$
      CASE " "
        k = 5
      CASE "s"
        SoundOn = NOT SoundOn
      CASE "-"
        WaitTime& = WaitTime& + 1000
      CASE "+"
        WaitTime& = WaitTime& - 1000
      CASE CHR$(27)
        OutObj eek
        CLS
        LOCATE 10, 15: PRINT "[Q]uit"
        LOCATE 11, 15: PRINT "[R]etry"
        LOCATE 12, 15: PRINT "[C]ontinue"
        DO: d$ = INKEY$: LOOP UNTIL d$ = ""
        DO
          DO: k$ = INKEY$: LOOP UNTIL k$ <> ""
          k = INSTR("QRC", UCASE$(k$))
        LOOP UNTIL k <> 0
        SELECT CASE k
          CASE 1
            Player.Dead = True
            GameOver = True
          CASE 2
            Player.Dead = True
          CASE 3
            FOR x = 0 TO 19
              sy = 9
              FOR y = 0 TO 10
                ReplaceBrick x, y
              NEXT y
              sx = sx + 16
            NEXT x
        END SELECT
        IF eek <> -1 THEN InObj eek
        k = 0
    END SELECT
  END IF
  ChkKey = k
  'Cleak the rest of the Key Buffer
  DO: d$ = INKEY$: LOOP UNTIL d$ = ""
END FUNCTION

SUB ClrAllObj
  FOR n = 0 TO MaxObj
    IF Obj(n).ObjOn THEN PUT (Obj(n).ox, Obj(n).oy + 9), ObjBkg(0, n), PSET: Obj(n).ObjOn = False
  NEXT n
END SUB

SUB ClrObj (n)
  IF Obj(n).ObjOn = 0 THEN EXIT SUB
  'Clear Objs
  FOR z = 0 TO n
    IF Obj(z).ObjOn THEN PUT (Obj(z).ox, Obj(z).oy + 9), ObjBkg(0, z), PSET
  NEXT z
  'Turn Obj off
  Obj(n).ObjOn = 0
  ' Re-Draw Objs
  IF n > 0 THEN
    FOR z = n - 1 TO 0 STEP -1
      IF Obj(z).ObjOn THEN
        x = Obj(z).sx
        y = Obj(z).sy
        i = Obj(z).Frame
        Obj(z).ox = x
        Obj(z).oy = y
        Obj(z).oFrame = i
        GET (x, y + 9)-(x + 15, y + 24), ObjBkg(0, z)
        PUT (x, y + 9), ObjImg(0, i, 1), AND
        PUT (x, y + 9), ObjImg(0, i, 0), OR
      END IF
    NEXT z
  END IF
END SUB

SUB DAllObj
  FOR n = MaxObj TO 0 STEP -1
    x = Obj(n).sx
    y = Obj(n).sy
    i = Obj(n).Frame
    GET (x, y + 9)-(x + 15, y + 24), ObjBkg(0, n)
    PUT (x, y + 9), ObjImg(0, i, 1), AND
    PUT (x, y + 9), ObjImg(0, i, 0), OR
    Obj(n).ObjOn = -1
  NEXT n
END SUB

SUB Dec (x, b, t)
  IF x = b THEN x = t ELSE x = x - 1
END SUB

SUB Delay (x&)
  FOR i& = 0 TO WaitTime&
  NEXT i&
END SUB

SUB DoAnim (Seq, w)
  DO
    IF Anim(i).inUse = False THEN EXIT DO
    i = i + 1
  LOOP
  Anim(i).inUse = True
  Anim(i).Seq = Seq - 1
  Anim(i).p = 1
  Anim(i).Delay = 0
  IF w THEN
    DO
      UAnim
    LOOP UNTIL Anim(i).inUse = False
  END IF
END SUB

SUB DObj (n)
  'Clear Objs
  FOR z = 0 TO n
    IF Obj(z).ObjOn THEN PUT (Obj(z).ox, Obj(z).oy + 9), ObjBkg(0, z), PSET
  NEXT z
  'Turn Obj on
  Obj(n).ObjOn = True
  ' Re-Draw Objs
  FOR z = n TO 0 STEP -1
    IF Obj(z).ObjOn THEN
      x = Obj(z).sx
      y = Obj(z).sy
      i = Obj(z).Frame
      Obj(z).ox = x
      Obj(z).oy = y
      Obj(z).oFrame = i
      GET (x, y + 9)-(x + 15, y + 24), ObjBkg(0, z)
      PUT (x, y + 9), ObjImg(0, i, 1), AND
      PUT (x, y + 9), ObjImg(0, i, 0), OR
    END IF
  NEXT z
END SUB

SUB DoEnemy
END SUB

SUB DoLandOn (zd&)
  'Check Current Brick
  ChkCannon
  b = GetBrickType(Obj(0).mx, Obj(0).my)
  IF Player.Holding THEN
  ELSE
    SELECT CASE b
      CASE 1, 15
        'Wall or CrackBrk ... Fall Back
        TurnObjAround 0
        Roll Obj(0).f
        IF GetBrickType(Obj(0).mx, Obj(0).my) = 1 THEN TurnObjAround 0
        DoLandOn zd&
      CASE 3
        'Warp ... Warp
        'Check the Cannons
        ChkCannon
        'WARP!
        DoWarp
      CASE 4
        ' Ice
        Slip Obj(0).f
        DoLandOn zd&
        ' Test
      CASE 2, 5
        ChkCannon
        Jump Obj(0).f, zd&
        DoLandOn zd&
      CASE 9
        ' Ketchup
        PutItem Obj(0).mx, Obj(0).my, 0
        ClrObj 0
        ReplaceBrick Obj(0).mx, Obj(0).my
        DObj 0
        DoAnim 32, True
        DoLandOn zd&
      CASE 7
        ' Beat The Level
        Obj(0).Frame = 45: UAllObj
        DoSound 8
        Level.Won = True
        SLEEP 2
      CASE 8
        ' Weight
        Inc RollDir, 1, 4
        Obj(0).f = RollDir
        Roll Obj(0).f
        DoLandOn zd&
      CASE 10
        ' Steelie
        Obj(0).f = INT(RND * 4) + 1
        Roll Obj(0).f
        DoLandOn zd&
      CASE 13
        'Tree ... Squish it
        DoSound 12
        PutBrick Obj(0).mx, Obj(0).my, 16
        ClrObj 0
        ReplaceBrick Obj(0).mx, Obj(0).my
        DObj 0
      CASE 14
        'Water ... Sploosh
        Splash 0, Obj(0).mx, Obj(0).my
        Player.Dead = True
      END SELECT
  END IF
END SUB

SUB DoPlayer
  k = ChkKey
  IF k = 0 THEN
    IF Player.WillMove THEN
      k = Obj(0).f
    ELSE
      IF NOT Player.Standing THEN
        Stand Obj(0).f
        Player.Standing = True
      END IF
      EXIT SUB
    END IF
  END IF
  IF k = Obj(0).f THEN
    IF Player.Holding THEN
    ELSE
      zd& = 50: ' Delay Length
      'Try to move
      Player.Standing = False
      b = ChkAheadBrk
      'Do the Action
      SELECT CASE b
        CASE 0
          'Ground ... Walk Up
          walk k
          ChkCannon
        CASE 1
          'Wall ... Try to Push It
          Push k, zd&
        CASE 2
          'Rubber ... ???
        CASE 3
          'Warp ... Warp
          walk k
          'Check the Cannons
          ChkCannon
          'WARP!
          DoWarp
        CASE 4
          ' Ice ... Walk then xfer to DoLandOn
          Slip k
          ChkCannon
          DoLandOn zd&
        CASE 5
          'Trampoline
          DoAnim k + 24, True
          'Check Cannons
          ChkCannon
          Jump k, zd&
          b = GetBrickType(Obj(0).mx, Obj(0).my): IF b = 1 OR b = 6 OR b = 8 OR b = 15 THEN REM TurnPlayerAround
          DoLandOn zd&
        CASE 7
          'Exit..Beat the Level
          walk k
          ChkCannon
          DoLandOn zd&
        CASE 8
          'Weight  ... Push it!
          PushSObj
        CASE 9
          'Ketchup ... Drink It!
          walk k
          ChkCannon
          DoLandOn zd&
        CASE 10
          'Steelie ... Push it!
          PushSObj
      END SELECT
    END IF
    Player.WillMove = False
  ELSE
    IF k = 5 THEN
      ' Try to Flip Switch
      xi = 0: yi = 0
      SELECT CASE Obj(0).f
        CASE 1
          yi = -1
        CASE 2
          yi = 1
        CASE 3
          xi = -1
        CASE 4
          xi = 1
      END SELECT
      b = GetBrickType(Obj(0).mx + xi, Obj(0).my + yi)
      IF b = 6 THEN
        ' It's a Switch
        DoSwitch Obj(0).mx + xi, Obj(0).my + yi
      END IF
    ELSE
      'Turn Around
      zd& = 50
      SELECT CASE Obj(0).f
        CASE 1 TO 2
          SELECT CASE k
            CASE 1
              Obj(0).Frame = 10: Uplayer: Delay zd&
              Obj(0).Frame = 4: Uplayer: Delay zd&
            CASE 2
              Obj(0).Frame = 7: Uplayer: Delay zd&
              Obj(0).Frame = 1: Uplayer: Delay zd&
            CASE 3
              Obj(0).Frame = 10: Uplayer: Delay zd&
            CASE 4
              Obj(0).Frame = 7: Uplayer: Delay zd&
          END SELECT
        CASE 3 TO 4
          SELECT CASE k
            CASE 1
              Obj(0).Frame = 4: Uplayer: Delay zd&
            CASE 2
              Obj(0).Frame = 1: Uplayer: Delay zd&
            CASE 3
              Obj(0).Frame = 1: Uplayer: Delay zd&
              Obj(0).Frame = 10: Uplayer: Delay zd&
            CASE 4
              Obj(0).Frame = 4: Uplayer: Delay zd&
              Obj(0).Frame = 7: Uplayer: Delay zd&
          END SELECT
      END SELECT
      Obj(0).f = k
      Player.WillMove = True
    END IF
  END IF
END SUB

SUB DoSObjLandOn (x, y, o)
  zz = GetItem(x, y): PutItem x, y, 0
  IF o <> 0 THEN PutItem x, y, o
  b = GetBrickType(x, y): ReplaceBrick x, y
  SELECT CASE b
    CASE 1
      'Wall ... Roll Back
      Obj(3).Frame = 64 - zz
      Obj(3).mx = x
      Obj(3).my = y
      Obj(3).sx = Obj(3).mx * 16
      Obj(3).sy = Obj(3).my * 16
      TurnObjAround 3
      DoAnim 36 + Obj(3).f, True
      SELECT CASE Obj(3).f
        CASE 1
          y = y - 1
        CASE 2
          y = y + 1
        CASE 3
          x = x - 1
        CASE 4
          x = x + 1
      END SELECT
      oo = GetItem(x, y)
      PutItem x, y, zz
      ClrObj 3
      ReplaceBrick x, y
      IF GetBrickType(x, y) = 1 THEN TurnObjAround 3
      DoLandOn zd&
      DoSObjLandOn x, y, oo
    CASE 8
      ' Weight
      Inc RollDir, 1, 4
      Obj(3).f = RollDir
      Obj(3).Frame = 64 - zz
      Obj(3).mx = x
      Obj(3).my = y
      Obj(3).sx = Obj(3).mx * 16
      Obj(3).sy = Obj(3).my * 16
      DoAnim 36 + Obj(3).f, True
      SELECT CASE Obj(3).f
        CASE 1
          y = y - 1
        CASE 2
          y = y + 1
        CASE 3
          x = x - 1
        CASE 4
          x = x + 1
      END SELECT
      oo = GetItem(x, y)
      PutItem x, y, zz
      ClrObj 3
      ReplaceBrick x, y
      IF GetBrickType(x, y) = 1 THEN TurnObjAround 3
      DoLandOn zd&
      DoSObjLandOn x, y, oo
    CASE 10
      ' Steelie
      Obj(3).f = INT(RND * 4) + 1
      Obj(3).Frame = 64 - zz
      Obj(3).mx = x
      Obj(3).my = y
      Obj(3).sx = Obj(3).mx * 16
      Obj(3).sy = Obj(3).my * 16
      DoAnim 36 + Obj(3).f, True
      SELECT CASE Obj(3).f
        CASE 1
          y = y - 1
        CASE 2
          y = y + 1
        CASE 3
          x = x - 1
        CASE 4
          x = x + 1
      END SELECT
      oo = GetItem(x, y)
      PutItem x, y, zz
      ClrObj 3
      ReplaceBrick x, y
      IF GetBrickType(x, y) = 1 THEN TurnObjAround 3
      DoLandOn zd&
      DoSObjLandOn x, y, oo
    CASE 3
      ' Warp ... Warp duh
      DoSObjWarp x, y, zz
      DoLandOn zd&
    CASE 5, 2
      ' Trampoline ... Bounce
      Obj(3).Frame = 64 - zz
      Obj(3).mx = x
      Obj(3).my = y
      Obj(3).sx = Obj(3).mx * 16
      Obj(3).sy = Obj(3).my * 16
      DoAnim 40 + Obj(3).f, True
      SELECT CASE Obj(3).f
        CASE 1
          y = y - 2
        CASE 2
          y = y + 2
        CASE 3
          x = x - 2
        CASE 4
          x = x + 2
      END SELECT
      oo = GetItem(x, y)
      PutItem x, y, zz
      ReplaceBrick x, y
      DoLandOn zd&
      DoSObjLandOn x, y, oo
    CASE 4
      ' Ice ... Slip
      Obj(3).Frame = 64 - zz
      Obj(3).mx = x
      Obj(3).my = y
      Obj(3).sx = Obj(3).mx * 16
      Obj(3).sy = Obj(3).my * 16
      DoSound 13
      DoAnim 36 + Obj(3).f, True
      SELECT CASE Obj(3).f
        CASE 1
          y = y - 1
        CASE 2
          y = y + 1
        CASE 3
          x = x - 1
        CASE 4
          x = x + 1
      END SELECT
      oo = GetItem(x, y)
      PutItem x, y, zz
      ClrObj 3
      ReplaceBrick x, y
      DoLandOn zd&
      DoSObjLandOn x, y, oo
    CASE 13
      'Tree ... Squish it
      DoSound 12
      PutItem x, y, zz
      PutBrick x, y, 16
      ClrObj 3
      ReplaceBrick x, y
    CASE 14
      Splash 3, x, y
    CASE 17
      ' Chk if Steelie
      IF zz = 1 THEN
        ' Fill in the hole
        oo = GetItem(x, y)
        PutBrick x, y, 6
        DoSound 1
        Level.HoleCnt = Level.HoleCnt + 1
        ClrObj 3
        ClrObj 2
        ReplaceBrick x, y
        dummy = CheckIfWin
        IF oo <> 0 THEN DoSObjLandOn x, y, oo
      ELSE
        'Just Sit There
        oo = GetItem(x, y)
        PutItem x, y, zz
        ClrObj 3
        ReplaceBrick x, y
        IF oo <> 0 THEN DoSObjLandOn x, y, oo
      END IF
    CASE ELSE
      oo = GetItem(x, y)
      PutItem x, y, zz
      ClrObj 3
      ReplaceBrick x, y
      IF oo <> 0 THEN DoSObjLandOn x, y, oo
  END SELECT
END SUB

SUB DoSObjWarp (x, y, zz)
  i = 0
  DO
    IF Warp(i, 0) = x AND Warp(i, 1) = y THEN EXIT DO
    i = i + 1
  LOOP UNTIL i = 9
  IF i = 9 THEN
    WarpIn = True
  END IF
  Obj(3).Frame = 64 - zz
  Obj(3).mx = x
  Obj(3).my = y
  Obj(3).sx = Obj(3).mx * 16
  Obj(3).sy = Obj(3).my * 16
  IF WarpIn = False THEN
    DoAnim 45, True
    'Check the type of warp
    IF Level.wt = 0 THEN
      even = (i MOD 2) - 1
      IF even THEN i = i + 1 ELSE i = i - 1
    ELSE
      Inc i, 0, 8
    END IF
    'Warp Coors
    wx = Warp(i, 0)
    wy = Warp(i, 1)
    'Warp Marble If There
    b = GetBrickType(wx, wy)
    IF b = 8 OR b = 10 THEN
      z2 = GetItem(wx, wy): PutItem wx, wy, 0: ReplaceBrick wx, wy
      DoSObjWarp (wx), (wy), (z2)
    END IF
    Obj(3).mx = wx
    Obj(3).my = wy
    Obj(3).sx = Obj(3).mx * 16
    Obj(3).sy = Obj(3).my * 16
    x = wx
    y = wy
  END IF
  'Reappear
  DoAnim 46, True
  PutItem x, y, zz
  ReplaceBrick x, y
END SUB

SUB DoSound (snd) STATIC
  IF NOT SoundOn THEN EXIT SUB
  SELECT CASE snd
    CASE 0
      ' Step
      s$ = "T255mso1l64c"
      p = 0
    CASE 1
      ' Thump
      s$ = "T255L64O1ep32dp32cp32"
      p = 1
    CASE 2
      ' Drink
      s$ = "T255L16O1aa#b a#b>c <b>cc# cc#d c#dd# dd#e a"
      p = 100
    CASE 3
      ' Pick Up
      s$ = "t255l64o1cp64cp64cp64cp64cp64cp64cp64cp64cp4l32>cc#dd#eff#"
      p = 100
    CASE 4
      ' Bounce
      s$ = "t255l32p16o1cc#dd#eff#gg#"
      p = 50
    CASE 5
      ' Drop
      s$ = "t225l32o2d#dc#c<ba#ap12l64o1cp64cp64cp64cp64c"
      p = 50
    CASE 6
      ' Chink (Change Switch)
      s$ = "t255l64o1co3co6d#o3ao1co3co4d#o3a"
      p = 50
    CASE 7
      ' Walk Down
      s$ = "t222l64o1d#p32d#p32d#p32d#p8dp32dp32dp32dp8c#p32c#p32c#p32c#p8cp32cp32cp32cp32"
      p = 100
    CASE 8
      ' End
      s$ = "t90 l16o2c l32d# l16e l24a# l16g l24f l16f# l24g l16>c l8<b >c"
      p = 150
    CASE 9
      ' Item
      s$ = "t190l16o3efg>l8c<g"
      p = 100
    CASE 10
      ' Bump
      s$ = "t255l32o1cc#dd#ecc#d"
      p = 1
    CASE 11
      ' Walk Up
      s$ = "t222l64o1cp32cp32cp32cp8c#p32c#p32c#p32c#p8dp32dp32dp32dp8d#p32d#p32d#p32d#p32"
      p = 100
    CASE 12
      ' Smash
      s$ = "t255l64o2cn0c#n0dn0c#n2cn0<dn0c#n2cn0dn0c#n2cn0"
      p = 1
    CASE 13
      ' Slip
      s$ = "t255l24o2c>c#<d>d#<e>f<f#>g<g#"
      p = 1
    CASE 14
      ' Splash
      s$ = "t255l64ms"
      FOR i = 0 TO 20
        s$ = s$ + "N" + STR$(INT(RND * 84))
      NEXT i
      p = 150
    CASE 15
      'Cannon Shoot
      s$ = "t255l64ms"
      FOR i = 0 TO 20
        s$ = s$ + "N" + STR$(INT(RND * 30))
      NEXT i
      p = 1
    CASE 16
      'Warp In
      s$ = "t255l64o3p60cegb>c#fg#>cegb"
    CASE 17
      'Warp Out
      s$ = "t255l64o5p60bgec<g#fc#<bgec"
  END SELECT
  IF PLAY(0) = 0 OR p > cp THEN
    SOUND 0, 0
    PLAY "MBX" + VARPTR$(s$)
    cp = p
  END IF
END SUB

SUB DoSteelieBrk (x, y, zd&)
  'Check Current Brick
  b = GetBrickType(x, y)
  SELECT CASE b
    CASE 1, 15
      'Wall or CrackBrk ... Roll Back
      SObjRoll x, y, Obj(0).f, zd&
      DoSteelieBrk x, y, zd&
    CASE 3
      'Warp ... Warp
      'Check the Cannons
      ChkCannon
      'WARP!
      DoWarp
    CASE 4
      ' Ice
      Slip Obj(0).f
      b = GetBrickType(Obj(0).mx, Obj(0).my): IF b = 1 OR b = 6 OR b = 8 OR b = 15 THEN REM TurnPlayerAround
      DoLandOn zd&
    CASE 2, 5
      'Rubber or Trampoline
      'Check Cannons
      ChkCannon
      Jump Obj(0).f, zd&
      b = GetBrickType(Obj(0).mx, Obj(0).my): IF b = 1 OR b = 6 OR b = 8 OR b = 15 THEN REM TurnPlayerAround
      DoLandOn zd&
    CASE 9
      ' Ketchup
      ClrObj 0
      PutItem Obj(0).mx, Obj(0).my, 0
      ReplaceBrick Obj(0).mx, Obj(0).my
      DObj 0
      Obj(0).Frame = 114: Uplayer
      Objite 1, Obj(0).sx, Obj(0).sy, 62: Delay zd&
      DoSound 2
      Objite 1, Obj(0).sx + 1, Obj(0).sy - 2, 47
      Delay zd& * 13
      ClrObj 1
      Delay zd&
      Obj(0).Frame = 45: Uplayer
      DoSound 9
      Delay zd& * 7
    CASE 13
      'Tree ... Squish it
      DoSound 12
      PutBrick Obj(0).mx, Obj(0).my, 16
      ClrObj 0
      ReplaceBrick Obj(0).mx, Obj(0).my
      DObj 0
    CASE 14
      'Water ... Sploosh
      Splash 0, Obj(0).mx, Obj(0).my
      Player.Dead = True
  END SELECT
END SUB

SUB DoSwitch (x, y)
  sw = GetItem(x, y)
  IF sw = 15 THEN sw = 14 ELSE sw = 15
  PutItem x, y, sw
  DoSound 6
  OutObj eek
  ReplaceBrick x, y
  IF eek <> -1 THEN InObj eek
  m = MapData(i, 0, w, s) \ 512
  coor = x * 16 + y
  FOR i = 0 TO 9
    IF ((MapData(i, 0, w, s) MOD 512) = coor) AND (MapData(i, 0, w, s) \ 512 <> 0) THEN
      EXIT FOR
    END IF
  NEXT i
  SELECT CASE i
    CASE 1
      ' Change Warp Type
      IF Level.wt = 0 THEN Level.wt = 1 ELSE Level.wt = 0
    CASE 2
      ' Rot. Sm Cannon 90 Lt
    CASE 3
      ' Rot. Sm Cannon 90 Rt
    CASE 4
      ' Rot. Sm Cannon 180
    CASE 5
      ' Rot. All Big Can.
    CASE 6
      ' Swap All Tee-Tots
    CASE 7
      ' Swap Brk on X
    CASE 8
      ' Swap Brk on Y
    CASE 9
      ' Swap one kind
      b2 = MapData(i, 0, w, s) \ 256
      b1 = MapData(i, 0, w, s) MOD 256
      SwapAllBrk b1, b2
    CASE 10
      ' Reset All Switches
    CASE 11
      ' Shoot all Big Can.
    CASE 12
      ' Shoot 1 Big Can.
    CASE 13
      ' Shoot all Sm Can.
    CASE 14
      ' Swap Brk once on X
    CASE 15
      ' Swap Brk once on Y
    CASE 16
      ' Rot. U/D Big Cannons
    CASE 17
      ' Rot. L/R Big Cannons
  END SELECT
END SUB

SUB DoWarp
  'WARP!
  i = 0
  DO
    IF Warp(i, 0) = Obj(0).mx AND Warp(i, 1) = Obj(0).my THEN EXIT DO
    i = i + 1
  LOOP UNTIL i = 9
  IF i = 9 THEN
    CLS
    PRINT "FATAL ERROR: NO WARP FOUND"
    END
  END IF
 
  'Disappear
  DoAnim 29, True
 
  'Check the type of warp
  IF Level.wt = 0 THEN
    even = (i MOD 2) - 1
    IF even THEN i = i + 1 ELSE i = i - 1
  ELSE
    Inc i, 0, 8
  END IF
 
  wx = Warp(i, 0)
  wy = Warp(i, 1)
  'Warp Marble If There
  b = GetBrickType(wx, wy)
  IF b = 8 OR b = 10 THEN
    zz = GetItem(wx, wy): PutItem wx, wy, 0: ReplaceBrick wx, wy
    DoSObjWarp wx, wy, zz
  END IF
  wx = Warp(i, 0)
  wy = Warp(i, 1)
  'Warp Coors
  Obj(0).mx = wx
  Obj(0).my = wy
  Obj(0).sx = Obj(0).mx * 16
  Obj(0).sy = Obj(0).my * 16
  'Reappear
  DoAnim 30, True
END SUB

FUNCTION GetBrick (x, y)
  GetBrick = Map(x, y, w, s) MOD 256
END FUNCTION

FUNCTION GetBrickType (x, y)
  b = GetBrick(x, y)
  SELECT CASE b
    CASE 4, 9, 14, 16
      t1 = 0: 'First Try...Ground
    CASE 0, 2, 3, 6, 20, 21, 22
      t1 = 1: '... Wall
    CASE 1
      t1 = 2: '... Rubber
    CASE 7
      t1 = 13: '.. Tree
    CASE 8
      t1 = 15: '.. Cracked Brick
    CASE 10
      t1 = 14: '.. Water
    CASE 11
      t1 = 3: '... Warp
    CASE 12
      t1 = 4: '... Ice
    CASE 13
      t1 = 5: '... Trampoline
    CASE 5
      t1 = 17: '.. Hole in the Ground
    CASE 31, 32, 17, 15
      t1 = 18: '.. Small Cannon
  END SELECT
  t2 = -1
  i = GetItem(x, y)
  SELECT CASE i
    CASE 15, 14
      t2 = 6: '...Switch
    CASE 11 TO 13
      t2 = 7: '...Exit
    CASE 3
      t2 = 8: '...weight
    CASE 2
      t2 = 9: '...Super Ketchup
    CASE 1
      t2 = 10: '..Steelie
    CASE 9
      t2 = 11: '..Teeter-Totter High Side 1
    CASE 4
      t2 = 16: '..Teeter-Totter High Side 2
    CASE 8, 5
      t2 = 12: '..High Ground
    CASE 7, 6
      t2 = 0: '...Ground
  END SELECT
  IF t2 = -1 THEN t = t1 ELSE t = t2
  GetBrickType = t
END FUNCTION

FUNCTION GetClick
  DO
    DO
      k$ = INKEY$
    LOOP UNTIL k$ <> ""
    k = INSTR("1234567890.+- LSqwasRrz" + CHR$(27), k$)
    IF k = 0 THEN boop
  LOOP UNTIL k <> 0
  GetClick = k
END FUNCTION

FUNCTION GetItem (x, y)
  GetItem = Map(x, y, w, s) \ 256
END FUNCTION

FUNCTION Hex1 (v$)
  HexA = INSTR("0123456789abcdef", LEFT$(v$, 1)) - 1
  Hex1 = HexA
  IF debug AND HexA = -1 THEN CLS : PRINT "ERROR: INVALID HEX1 "; LEFT$(v$, 1)
END FUNCTION

FUNCTION Hex2 (v$)
  HexA = INSTR("0123456789abcdef", LEFT$(v$, 1)) - 1
  HexB = INSTR("0123456789abcdef", RIGHT$(v$, 1)) - 1
  Hex2 = HexA * 16 + HexB
  IF debug AND (HexA = -1 OR HexB = -1) THEN CLS : PRINT "ERROR: INVALID Hex2:"; v$
END FUNCTION

FUNCTION Hex4 (v$)
  hx$ = "0123456789abcdef"
  HexA = INSTR(hx$, RIGHT$(v$, 1)) - 1
  HexB = INSTR(hx$, MID$(v$, 2, 1)) - 1
  HexC = INSTR(hx$, MID$(v$, 3, 1)) - 1
  HexD = INSTR(hx$, LEFT$(v$, 1)) - 1
  Hex4 = HexA * 4096 + HexB * 256 + HexC * 16 + HexD
  IF debug AND (HexA = -1 OR HexB = -1 OR HexC = -1 OR HexD = -1) THEN CLS : PRINT "ERROR: INVALID HEX4"
END FUNCTION

SUB Inc (x, b, t)
  IF x = t THEN x = b ELSE x = x + 1
END SUB

SUB Init
  SHARED cObj
  SCREEN 1, 0
  COLOR 0, 1
  PALETTE 0, 0
  PALETTE 1, 7
  PALETTE 2, 8
  PALETTE 3, 15
  RANDOMIZE TIMER
END SUB

SUB InitLevel
  CLS
  Level.MaxHole = 0
  sx = 0
  FOR x = 0 TO 19
    sy = 9
    FOR y = 0 TO 10
      b = GetBrick(x, y)
      i = GetItem(x, y)
      'Check for Special Bricks
      SELECT CASE b
        CASE 5
          Level.MaxHole = Level.MaxHole + 1
        CASE 11
          Warp(8, 0) = x
          Warp(8, 1) = y
        CASE 23 TO 30
          Warp(b - 23, 0) = x
          Warp(b - 23, 1) = y
          PutBrick x, y, 11
      END SELECT
      'Check for Special Items
      SELECT CASE i
        CASE 11 TO 13
          Level.ex = x
          Level.ey = y
          Level.ef = i
          PutItem x, y, 0
        CASE 16
          Level.psx = x
          Level.psy = y
          Warp(9, 0) = x
          Warp(9, 1) = y
          PutItem x, y, 0
      END SELECT
      ReplaceBrick x, y
    NEXT y
    sx = sx + 16
  NEXT x
  Level.HoleCnt = 0
  Level.Won = False
  ws = w * 10 + s
  SELECT CASE ws
    CASE 5
      Level.wt = 0
    CASE ELSE
      Level.wt = 1
  END SELECT
  ' Initialize Player
  Obj(0).mx = Level.psx
  Obj(0).my = Level.psy
  Obj(0).sx = Level.psx * 16
  Obj(0).sy = Level.psy * 16
  Player.k = False
  Obj(0).f = 2
  Obj(0).Frame = 1
  DObj 0
END SUB

SUB InObj (n)
  ' Re-Draw Objs
  FOR z = n TO 0 STEP -1
    IF Obj(z).ObjOn THEN
      x = Obj(z).sx
      y = Obj(z).sy
      i = Obj(z).Frame
      Obj(z).ox = x
      Obj(z).oy = y
      Obj(z).oFrame = i
      GET (x, y + 9)-(x + 15, y + 24), ObjBkg(0, z)
      PUT (x, y + 9), ObjImg(0, i, 1), AND
      PUT (x, y + 9), ObjImg(0, i, 0), OR
    END IF
  NEXT z
END SUB

SUB Intro
  x = 11: y = 9
  CLS
  LOCATE y + 0, x: PRINT "=-=-=-=-=-=-=-=-=-="
  LOCATE y + 2, x: PRINT " The Adventures of "
  LOCATE y + 3, x: PRINT "Handlebar  McKinley"
  LOCATE y + 5, x: PRINT "=-=-=-=-=-=-=-=-=-="
  LOCATE y + 13, x: PRINT "  (c)1991 SamWare  "
  LOCATE y + 14, x: PRINT "   Press any Key   ";
  SLEEP
END SUB

SUB Jump (f, zd&)
  DoSound 4
  DoAnim f + 12, True
END SUB

SUB LoadImg (file$)
  DEF SEG = VARSEG(ObjImg(0, 0, 0))
  BLOAD file$, VARPTR(ObjImg(0, 0, 0))
END SUB

SUB LoadMap (file$)
  DEF SEG = VARSEG(Map(0, 0, 0, 0))
  BLOAD file$ + ".map", VARPTR(Map(0, 0, 0, 0))
  DEF SEG = VARSEG(MapData(0, 0, 0, 0))
  BLOAD file$ + ".dat", VARPTR(MapData(0, 0, 0, 0))
END SUB

SUB Objite (n, x, y, i)
  IF Obj(n).ObjOn THEN PUT (Obj(n).ox, Obj(n).oy + 9), ObjBkg(0, n), PSET
  GET (x, y + 9)-STEP(15, 15), ObjBkg(0, n)
  PUT (x, y + 9), ObjImg(0, i, 1), AND
  PUT (x, y + 9), ObjImg(0, i, 0), XOR
  Obj(n).Frame = i
  Obj(n).sx = x
  Obj(n).sy = y
  Obj(n).ObjOn = -1
  Obj(n).ox = x
  Obj(n).oy = y
END SUB

SUB OutObj (n)
  ' Find Last Objs Which need to be updated
  zz = -1
  FOR n = 0 TO MaxObj
    IF Obj(n).ObjOn THEN
      IF Obj(n).sx <> Obj(n).ox OR Obj(n).sy <> Obj(n).oy OR Obj(n).Frame <> Obj(n).oFrame THEN
        zz = n
      END IF
    END IF
  NEXT n
  n = zz
  IF zz = -1 THEN EXIT SUB
  'Clear Objs
  FOR z = 0 TO n
    IF Obj(z).ObjOn THEN PUT (Obj(z).ox, Obj(z).oy + 9), ObjBkg(0, z), PSET
  NEXT z
END SUB

SUB Push (f, zd&)
  DoAnim f + 16, True
END SUB

SUB PushSObj
  xi = 0: yi = 0
  SELECT CASE Obj(0).f
    CASE 1
      yi = -1
    CASE 2
      yi = 1
    CASE 3
      xi = -1
    CASE 4
      xi = 1
  END SELECT
  zz = GetItem(Obj(0).mx + xi, Obj(0).my + yi)
  PutItem Obj(0).mx + xi, Obj(0).my + yi, 0
  ReplaceBrick Obj(0).mx + xi, Obj(0).my + yi
  Obj(3).Frame = 64 - zz
  Obj(3).mx = Obj(0).mx + xi
  Obj(3).my = Obj(0).my + yi
  Obj(3).sx = Obj(3).mx * 16
  Obj(3).sy = Obj(3).my * 16
  Obj(3).f = Obj(0).f
  b = GetBrickType(Obj(0).mx + (xi * 2), Obj(0).my + (yi * 2))
  SELECT CASE b
    CASE 1, 13, 15, 14, 6, 7, 8, 9, 10, 11, 16, 12
      'Wall ... Do Nothing
      PutItem Obj(0).mx + xi, Obj(0).my + yi, zz
      ReplaceBrick Obj(0).mx + xi, Obj(0).my + yi
    CASE 2
      'Rubber . Do The Flip
      TurnObjAround 3
      DoAnim 36 + Obj(3).f, False
      DoAnim 4 + Obj(0).f, True
      PutItem Obj(0).mx - xi, Obj(0).my - yi, zz
      ClrAllObj
      ReplaceBrick Obj(0).mx - xi, Obj(0).my - yi
      DObj 0
    CASE 3
      ' Warp
      DoAnim 36 + Obj(0).f, False
      DoAnim 32 + Obj(0).f, True
      oo = GetItem(x, y)
      PutItem Obj(0).mx + xi, Obj(0).my + yi, zz
      ReplaceBrick Obj(0).mx + xi, Obj(0).my + y1
      ix = Obj(0).mx + xi: iy = Obj(0).my + yi
      DoSObjLandOn ix, iy, oo
    CASE 0, 4, 5, 12, 17
      'Ground . Roll
      DoAnim 36 + Obj(0).f, False
      DoAnim 32 + Obj(0).f, True
      oo = GetItem(x, y)
      PutItem Obj(0).mx + xi, Obj(0).my + yi, zz
      ReplaceBrick Obj(0).mx + xi, Obj(0).my + yi
      ix = Obj(0).mx + xi: iy = Obj(0).my + yi
      DoSObjLandOn ix, iy, oo
      DoLandOn zd&
  END SELECT
END SUB

SUB PutBrick (x, y, b)
  Map(x, y, w, s) = ((Map(x, y, w, s) \ 256) * 256) + (b MOD 256)
END SUB

SUB PutItem (x, y, i)
  Map(x, y, w, s) = (Map(x, y, w, s) MOD 256) + (i MOD 256) * 256
END SUB

SUB PutObjite (x, y, i)
'  PRINT x, y, i
  PUT (x, y), ObjImg(0, i, 0), PSET
END SUB

SUB ReplaceBrick (x, y)
  PUT (x * 16, y * 16 + 9), ObjImg(0, GetBrick(x, y) + 64, 0), PSET
  zz = GetItem(x, y)
  IF zz > 0 THEN
    PUT (x * 16, y * 16 + 9), ObjImg(0, 64 - zz, 1), AND
    PUT (x * 16, y * 16 + 9), ObjImg(0, 64 - zz, 0), XOR
  END IF
END SUB

SUB Roll (f)
  TurnObjAround 0
  DoAnim f + 4, True
END SUB

SUB SetObj (n, x, y, i)
  Obj(n).Frame = i
  Obj(n).sx = x
  Obj(n).sy = y
END SUB

SUB ShootSmallCannon (x, y)
  b = GetBrick(x, y)
  SELECT CASE b
    CASE 31
      ' UP
      d = 1
      n = x
      FOR i = y - 1 TO 0 STEP -1
        b = GetBrickType(x, i)
        SELECT CASE b
          CASE 0, 14, 3, 4, 5, 17, 7, 9
            'can
            b = 0
          CASE 1, 13, 8, 11, 16, 12
            'Can't
            b = 1: EXIT FOR
          CASE 6
            'Switch
            b = 2: EXIT FOR
          CASE 15
            'Cracked Brk
            b = 3: EXIT FOR
          CASE 10
            'Steelie
            b = 4: EXIT FOR
        END SELECT
        IF b > 0 THEN EXIT FOR
      NEXT i
      IF Obj(0).my > i AND Obj(0).my < y THEN i = Obj(0).my: b = 5

      Obj(4).mx = x
      Obj(4).my = y
      Obj(4).sx = Obj(4).mx * 16
      Obj(4).sy = Obj(4).my * 16
      Obj(4).Frame = 54
      DObj 4
      DoSound 15
      FOR z = y TO i + 1 STEP -1
        DoAnim 47, True
      NEXT z
      ClrObj 4
    CASE 32
      ' Down
      d = 2
      n = x
      FOR i = y + 1 TO 10
        b = GetBrickType(x, i)
        SELECT CASE b
          CASE 0, 14, 3, 4, 5, 17, 7, 9
            'can
            b = 0
          CASE 1, 13, 8, 11, 16, 12
            'Can't
            b = 1: EXIT FOR
          CASE 6
            'Switch
            b = 2: EXIT FOR
          CASE 15
            'Cracked Brk
            b = 3: EXIT FOR
          CASE 10
            'Steelie
            b = 4: EXIT FOR
        END SELECT
        IF b > 0 THEN EXIT FOR
      NEXT i
      IF Obj(0).my < i AND Obj(0).my > y THEN i = Obj(0).my: b = 5

      Obj(4).mx = x
      Obj(4).my = y
      Obj(4).sx = Obj(4).mx * 16
      Obj(4).sy = Obj(4).my * 16
      Obj(4).Frame = 54
      DObj 4
      DoSound 15
      FOR z = y TO i - 1
        DoAnim 48, True
      NEXT z
      ClrObj 4
    CASE 17
      ' Left
      d = 3
      i = y
      FOR n = x - 1 TO 0 STEP -1
        b = GetBrickType(n, y)
        SELECT CASE b
          CASE 0, 14, 3, 4, 5, 17, 7, 9
            'can
            b = 0
          CASE 1, 13, 8, 11, 16, 12
            'Can't
            b = 1: EXIT FOR
          CASE 6
            'Switch
            b = 2: EXIT FOR
          CASE 15
            'Cracked Brk
            b = 3: EXIT FOR
          CASE 10
            'Steelie
            b = 4: EXIT FOR
        END SELECT
        IF b > 0 THEN EXIT FOR
      NEXT n
      IF Obj(0).mx > n AND Obj(0).mx < x THEN n = Obj(0).mx: b = 5
      Obj(4).mx = x
      Obj(4).my = y
      Obj(4).sx = Obj(4).mx * 16
      Obj(4).sy = Obj(4).my * 16
      Obj(4).Frame = 54
      DObj 4
      DoSound 15
      FOR z = x TO n + 1 STEP -1
        DoAnim 49, True
      NEXT z
      ClrObj 4
    CASE 15
      ' Right
      d = 4
      i = y
      FOR n = x + 1 TO 19
        b = GetBrickType(n, y)
        SELECT CASE b
          CASE 0, 14, 3, 4, 5, 17, 7, 9
            'can
            b = 0
          CASE 1, 13, 8, 11, 16, 12
            'Can't
            b = 1: EXIT FOR
          CASE 6
            'Switch
            b = 2: EXIT FOR
          CASE 15
            'Cracked Brk
            b = 3: EXIT FOR
          CASE 10
            'Steelie
            b = 4: EXIT FOR
        END SELECT
        IF b > 0 THEN EXIT FOR
      NEXT n
      IF Obj(0).mx < n AND Obj(0).mx > x THEN n = Obj(0).mx: b = 5
      Obj(4).mx = x
      Obj(4).my = y
      Obj(4).sx = Obj(4).mx * 16
      Obj(4).sy = Obj(4).my * 16
      Obj(4).Frame = 54
      DObj 4
      DoSound 15
      FOR z = x TO n - 1
        DoAnim 50, True
      NEXT z
      ClrObj 4
  END SELECT
  SELECT CASE b
    CASE 1
      DoSound 1
    CASE 2
      DoSwitch n, i
    CASE 3
      DoSound 1
      PutBrick n, i, &H13
      ReplaceBrick n, i
    CASE 5
      TurnObjAround 0
      Roll d
      Player.Dead = True
      Delay (100)
    CASE 4
      SObjRoll n, i, d, zd&
      DoSObjLandOn x, y, o
      IF Obj(0).mx = x OR Obj(0).my = y THEN
        Obj(0).f = INT(RND * 4) + 1
PRINT Obj(0).mx, Obj(0).my
        Roll Obj(0).f
PRINT Obj(0).mx, Obj(0).my
      END IF
    END SELECT
END SUB

SUB Slip (f)
  DoAnim f + 8, True
END SUB

SUB SObjRoll (x, y, f, zd&)
  DoSound 4
  zz = GetItem(x, y)
  'Erase Item From Block
  PutItem x, y, 0: OutObj n
  ReplaceBrick x, y: InObj n
  ' Perform Roll
  DoAnim 36 + f, True
  ix = x: iy = y
  SELECT CASE f
    CASE 1
      iy = iy - 1
    CASE 2
      iy = iy + 1
    CASE 3
      ix = ix - 1
    CASE 4
      ix = ix + 1
  END SELECT
  oo = GetItem(ix, iy)
  PutItem ix, iy, zz: OutObj n
  ReplaceBrick ix, iy: InObj n
  DoSObjLandOn ix, iy, oo
END SUB

SUB SortObj
  FOR i = 0 TO MaxObj - 1
    FOR j = i + 1 TO MaxObj
      IF Obj(i).sx > Obj(j).sx THEN SWAP Obj(i), Obj(j)
    NEXT j
  NEXT i
  FOR i = 0 TO MaxObj - 1
    FOR j = i + 1 TO MaxObj
      IF Obj(i).sy > Obj(j).sy THEN SWAP Obj(i), Obj(j)
    NEXT j
  NEXT i
END SUB

SUB Splash (sp, x, y)
  Obj(1).sx = x * 16
  Obj(1).sy = y * 16
  ClrObj sp
  ClrObj 1
  DoAnim 31, True
END SUB

' Show Stand Animation in a certain direction
'
SUB Stand (f)
  DoAnim f + 20, True
END SUB

SUB SteelieJump (x, y, f, d, zd&)
  DoSound 4
  sx = x * 16: sy = y * 16 + 9
  'Erase Item From Block
  PutItem x, y, 0: ClrAllObj
  ReplaceBrick x, y: DAllObj
  ' Perform Jump
END SUB

SUB SteelieWarp (x, y, zd&)
END SUB

SUB SwapAllBrk (b1, b2)
  OutObj eek
  FOR x = 0 TO 19
    FOR y = 0 TO 10
      IF GetBrick(x, y) = b1 THEN PutBrick x, y, b2: ReplaceBrick x, y
    NEXT y
  NEXT x
  IF eek <> -1 THEN InObj eek
END SUB

SUB TurnObjAround (i)
  Obj(i).f = Obj(i).f + (Obj(i).f MOD 2) * 2 - 1
END SUB

SUB UAllObj
  ' Find Last Objs Which need to be updated
  zz = -1
  FOR n = 0 TO MaxObj
    IF Obj(n).ObjOn THEN
      IF Obj(n).sx <> Obj(n).ox OR Obj(n).sy <> Obj(n).oy OR Obj(n).Frame <> Obj(n).oFrame THEN
        zz = n
      END IF
    END IF
  NEXT n
  IF zz = -1 THEN EXIT SUB
  n = zz
  'Clear Objs
  FOR z = 0 TO n
    IF Obj(z).ObjOn THEN PUT (Obj(z).ox, Obj(z).oy + 9), ObjBkg(0, z), PSET
  NEXT z
  ' Re-Draw Objs
  FOR z = n TO 0 STEP -1
    IF Obj(z).ObjOn THEN
      x = Obj(z).sx
      y = Obj(z).sy
      i = Obj(z).Frame
      Obj(z).ox = x
      Obj(z).oy = y
      Obj(z).oFrame = i
      GET (x, y + 9)-(x + 15, y + 24), ObjBkg(0, z)
      PUT (x, y + 9), ObjImg(0, i, 1), AND
      PUT (x, y + 9), ObjImg(0, i, 0), OR
    END IF
  NEXT z
END SUB

SUB UAnim STATIC
  FOR i = 0 TO MaxAnim
    IF Anim(i).inUse THEN
      IF Anim(i).Delay <> 0 THEN
        Anim(i).Delay = Anim(i).Delay - 1
        Delay 1
      ELSE
        IF Anim(i).p > LEN(Seq$(Anim(i).Seq)) THEN
          Anim(i).inUse = False
        ELSE
          Update = True
          'Remove whitespace
          DO
            in$ = MID$(Seq$(Anim(i).Seq), Anim(i).p, 1)
            IF in$ <> " " THEN EXIT DO
            Anim(i).p = Anim(i).p + 1
          LOOP
          v$ = MID$(Seq$(Anim(i).Seq), Anim(i).p + 1, 2)
          Anim(i).p = Anim(i).p + 2
          'Do Instruction
          SELECT CASE in$
            CASE "u"
              Obj(Anim(i).o).sy = Obj(Anim(i).o).sy - Hex1(v$)
            CASE "d"
              Obj(Anim(i).o).sy = Obj(Anim(i).o).sy + Hex1(v$)
            CASE "l"
              Obj(Anim(i).o).sx = Obj(Anim(i).o).sx - Hex1(v$)
            CASE "r"
              Obj(Anim(i).o).sx = Obj(Anim(i).o).sx + Hex1(v$)
            CASE "U"
              Obj(Anim(i).o).my = Obj(Anim(i).o).my - Hex1(v$)
            CASE "D"
              Obj(Anim(i).o).my = Obj(Anim(i).o).my + Hex1(v$)
            CASE "L"
              Obj(Anim(i).o).mx = Obj(Anim(i).o).mx - Hex1(v$)
            CASE "R"
              Obj(Anim(i).o).mx = Obj(Anim(i).o).mx + Hex1(v$)
            CASE "v"
              IF Hex1(v$) = 1 THEN
                DObj Anim(i).o
              ELSE
                ClrObj Anim(i).o
              END IF
            CASE "f"
              Anim(i).f = Hex2(v$)
              Obj(Anim(i).o).Frame = Hex2(v$)
              Anim(i).p = Anim(i).p + 1
            CASE "w"
              Anim(i).Delay = Hex1(v$)
              Update = False
            CASE "o"
              Anim(i).o = Hex1(v$)
            CASE "s"
              Obj(Anim(i).o).item = GetItem(Obj(Anim(i).o).mx, Obj(Anim(i).o).my)
              PutItem Obj(Anim(i).o).mx, Obj(Anim(i).o).my, 0
              Anim(i).p = Anim(i).p - 1
            CASE "p"
              PutItem Obj(Anim(i).o).mx, Obj(Anim(i).o).my, Obj(Anim(i).o).item
              Obj(Anim(i).o).item = 0
              Anim(i).p = Anim(i).p - 1
            CASE "c"
              Anim(i).Seq = Hex2(v$)
              Anim(i).p = Anim(i).p + 1
            CASE "x"
              DoSound (Hex2(v$))
              Anim(i).p = Anim(i).p + 1
              Update = False
            CASE "t"
              op$ = LEFT$(v$, 1)
              v1 = Hex1(MID$(Seq$(Anim(i).Seq), Anim(i).p, 1))
              v2 = Hex1(MID$(Seq$(Anim(i).Seq), Anim(i).p + 1, 1))
              SELECT CASE op$
                CASE "f"
                  Obj(v2).Frame = Obj(v1).Frame
                CASE "p"
                  Obj(v2).sx = Obj(v1).sx
                  Obj(v2).sy = Obj(v1).sy
                CASE "x"
                  Obj(v2).sx = Obj(v1).sx
                CASE "y"
                  Obj(v2).sy = Obj(v1).sy
                CASE ELSE
                  PRINT "ERROR: Syntax Error in Anim String "; MID$(Seq$(Anim(i).Seq), Anim(i).p, 4)
                  Anim(i).p = Anim(i).p + 4
              END SELECT
            CASE "a"
              op$ = v$
              v1 = Hex1(MID$(Seq$(Anim(i).Seq), Anim(i).p + 2, 1))
              v2 = Hex4(MID$(Seq$(Anim(i).Seq), Anim(i).p + 3, 4))
              SELECT CASE op$
                CASE "x"
                  Obj(v1).sx = v2
                CASE "y"
                  Obj(v1).sy = v2
                CASE ELSE
                  PRINT "ERROR: Syntax Error in Anim String "; MID$(Seq$(Anim(i).Seq), Anim(i).p, 7)
                  Anim(i).p = Anim(i).p + 7
              END SELECT
          END SELECT
        END IF
      END IF
    END IF
  NEXT i
  UAllObj
END SUB

SUB UObj (n)
  IF Obj(n).ObjOn = 0 THEN EXIT SUB
  'Clear Objs
  FOR z = 0 TO n
    IF Obj(z).ObjOn THEN PUT (Obj(z).ox, Obj(z).oy + 9), ObjBkg(0, z), PSET
  NEXT z
  ' Re-Draw Objs
  FOR z = n TO 0 STEP -1
    IF Obj(z).ObjOn THEN
      x = Obj(z).sx
      y = Obj(z).sy
      i = Obj(z).Frame
      Obj(z).ox = x
      Obj(z).oy = y
      Obj(z).oFrame = i
      GET (x, y + 9)-(x + 15, y + 24), ObjBkg(0, z)
      PUT (x, y + 9), ObjImg(0, i, 1), AND
      PUT (x, y + 9), ObjImg(0, i, 0), OR
    END IF
  NEXT z
END SUB

SUB Uplayer
  Objite 0, Obj(0).sx, Obj(0).sy, Obj(0).Frame
END SUB

SUB walk (f)
  DoAnim f, True
END SUB

