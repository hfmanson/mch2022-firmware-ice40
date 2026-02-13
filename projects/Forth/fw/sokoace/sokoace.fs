( SOKOBAN )

: DEF-CHAR 8 * DUP 8 + SWAP DO I FONT! LOOP ;

HEX
( Screen elements)
00 CONSTANT EMPTY
01 CONSTANT TARGET
02 CONSTANT SOKO
04 CONSTANT BOX
06 CONSTANT WALL

( GRAPHIC CHARACTERS )
: SETGR
00 00 00 00 00 00 00 00 EMPTY   DEF-CHAR ( Empty         )
00 00 00 18 18 00 00 00 TARGET  DEF-CHAR ( Target        )
3C 42 81 FF 99 99 7E 3C SOKO    DEF-CHAR ( Soko          )
3C 42 BD FF BD 99 7E 3C SOKO 1+ DEF-CHAR ( Soko + Target )
00 7E 42 42 42 42 7E 00 BOX     DEF-CHAR ( Box           )
00 7E 42 5A 5A 42 7E 00 BOX 1+  DEF-CHAR ( Box + Target  )
FF AB D5 AB D5 AB D5 FF WALL    DEF-CHAR ( Wall          )
;

DECIMAL

: AT XPOS ! YPOS ! ; ( row\col ... )

20 CONSTANT COLUMNSIZE
COLUMNSIZE NEGATE CONSTANT UP
COLUMNSIZE        CONSTANT DOWN
-1                CONSTANT LEFT
 1                CONSTANT RIGHT

COLUMNSIZE 21 * CONSTANT MAPSIZE

CREATE SCRATCH MAPSIZE ALLOT

: PUTCHAR
    DUP SOKO =
    OVER SOKO 1+ = OR
    OVER BOX 1+ = OR
    IF HIGHLIGHT ELSE NORMAL THEN
    LCD-EMIT ;

: 8TYPE
0 DO DUP I + C@ PUTCHAR LOOP DROP ;

: SCRATCH>SCR ( -- , copy level map to screen)
  SCRATCH 22 1
  DO
    I 0 AT DUP 20 8TYPE 20 +
  LOOP DROP ;

: MAP>SCRATCH ( level -- , copy map to scratch )
  ( copy all to scratch )
  MAPSIZE 2/ 0 DO
    DUP MAPSIZE 2/ * I + SRAM@ SCRATCH I 2* + !
  LOOP DROP

  ( adjust playfield values )
  SCRATCH COLUMNSIZE +
  COLUMNSIZE 20 * 0
  DO
    DUP DUP C@ DUP
    BL = IF DROP EMPTY SWAP C! ELSE
    EMPTY OR SWAP C! THEN
    1+
  LOOP DROP ;

: >UPPER ( c1 -- c2 ,convert to uppercase)
  DUP 96 > IF 223 AND THEN ;
: INC ( a -- )   1 SWAP +! ;
: DEC ( a -- )  -1 SWAP +! ;

0 VARIABLE SOKO>   ( holds SOKO screen address )
0 VARIABLE #BOX    ( number of boxes out of target )
0 VARIABLE #STEP   ( number of steps )
0 VARIABLE #PUSH   ( number of pushes )

: SCRATCH! ( b\addr -- )
  OVER OVER C!
  SCRATCH - ( calculate offset )
  COLUMNSIZE /MOD ( get row and column)
  1+ ( add 1 to row )
  SWAP AT PUTCHAR ( put character on screen )
;

: STEP ( a1 -- , move SOKO one step)
  DUP C@ TARGET AND SOKO OR OVER SCRATCH! ( place SOKO in new position )
  SOKO> @                                 ( previous SOKO position )
  DUP C@ TARGET AND 15 AND                ( check previous contents )
  IF TARGET ELSE EMPTY THEN               ( Target or Blank )
  SWAP SCRATCH!                           ( remove SOKO from old position )
  SOKO> !
  #STEP INC ;

: PUSH ( a1 a2 -- , push a BOX)
  OVER C@ TARGET AND 15 AND IF #BOX INC THEN     ( Box entered a target region )
  DUP  C@ DUP TARGET AND 15 AND IF #BOX DEC 1 SOUND! ELSE 0 SOUND! THEN ( Box exited a target area )
  TARGET AND BOX OR SWAP SCRATCH!                ( Move Box )
  STEP                                           ( Move Soko )
  #STEP DEC                                      ( Inc Pushes but not Steps )
  #PUSH INC ;

: GO ( dir -- , try to move Soko in the specified direction)
 SOKO> @ OVER +         ( next position )
 DUP C@ WALL =
 IF DROP DROP           ( if WALL, do nothing )
 ELSE
   DUP C@ DUP EMPTY = SWAP TARGET = OR
   IF STEP 0 SOUND! DROP         ( if Blank or Target, do Step)
   ELSE
     SWAP OVER +        ( over next position )
     DUP C@ DUP EMPTY = SWAP TARGET = OR
     IF   PUSH          ( if Blank or Target, do Push )
     ELSE DROP DROP     ( else, do nothing )
     THEN
   THEN
 THEN ;

: WALK ( c -- c, interpret key and move Soko )
  DUP [CHAR] I = IF UP    GO ELSE
  DUP [CHAR] K = IF DOWN  GO ELSE
  DUP [CHAR] J = IF LEFT  GO ELSE
  DUP [CHAR] L = IF RIGHT GO
  THEN THEN THEN THEN ;

: .#### ( a -- , formatted score type)
  @ 0 <# # # # # #> TYPE ;

: .SCORE ( update score )
  3 28 AT #BOX  @ .
  5 28 AT #STEP .####
  7 28 AT #PUSH .#### ;

: .FRAME ( Draw screen )
  PAGE ." ______SokoACE_______ version 1.0"
   3 22 AT ." BOXES ?"
   5 22 AT ." STEPS"
   7 21 AT ." PUSHES"
  10 21 AT ." I Up"
  11 21 AT ." K Down"
  12 21 AT ." J Left"
  13 21 AT ." L Right"
  15 21 AT ." N Level + 1"
  16 21 AT ." P Level - 1"
  17 21 AT ." R Restart"
  20 21 AT ." Q Quit"
  22  0 AT ." _by Ricardo F Lopes_   c 2006" ;

: SCAN ( Scan screen map for Soko position and count boxes out of target )
  0 #BOX ! ( reset number of boxes)
  MAPSIZE SCRATCH + COLUMNSIZE SCRATCH +
  DO
    I C@
    DUP SOKO = IF I SOKO> ! ELSE ( Search for SOKO start position )
    DUP BOX  = IF #BOX INC       ( Count Boxes out of target )
    THEN THEN DROP
  LOOP ;

0 VARIABLE LEVEL

: INITLEVEL ( level -- , Initialize Level)
  0 #STEP ! ( reset steps count )
  0 #PUSH ! ( reset pushes count )
  .FRAME
  0 MAX 29 MIN DUP LEVEL !
  MAP>SCRATCH
  SCRATCH>SCR
  SCAN ;

: MAP? ( c -- c , Change level)
  DUP [CHAR] N = IF LEVEL @ 1+ INITLEVEL ELSE ( Next level)
  DUP [CHAR] P = IF LEVEL @ 1- INITLEVEL ELSE ( Previous level)
  DUP [CHAR] R = IF LEVEL @    INITLEVEL      ( Re-start same level)
  THEN THEN THEN ;

: BUTTONS $08F0 IO@ ; \ Read button state

: BUTTON-TO-KEY
  BUTTONS
  DUP 2 = IF [CHAR] I ELSE
  DUP 1 = IF [CHAR] K ELSE
  DUP 4 = IF [CHAR] J ELSE
  DUP 8 = IF [CHAR] L ELSE
  DUP $100 = IF [CHAR] R ELSE
  DUP $200 = IF [CHAR] N ELSE
  DUP $400 = IF [CHAR] P ELSE 0
  THEN THEN THEN THEN THEN THEN THEN SWAP ;

: GET-KEY
  BEGIN
    BUTTON-TO-KEY DUP IF
      BEGIN BUTTONS 0= UNTIL ELSE
      DROP DROP KEY? IF KEY >UPPER ELSE 0 THEN ?DUP
    THEN
  UNTIL
;

: PLAY ( Main code, run this to play SokoACE)
  DINT LCD-INIT NOCAPTION
  ( load levels from binding $DABBAD20 )
  32 LOAD-SRAM 70 MS
  +LCD
  SETGR     ( initialize graphics )
  0 INITLEVEL    ( start the first level )
  BEGIN
    .SCORE       ( Update Score)
    GET-KEY      ( Get key pressed )
    WALK         ( Move SOKO )
    MAP?         ( Check for level request)
    1 9 AT
    #BOX @ 0=    ( No boxes left?)
    IF
      ." Done !" 2 SOUND! ( 100 50 BEEP 75 25 BEEP Level completed !)
    THEN
    [CHAR] Q = UNTIL
  ." Quit." ;

' PLAY INIT !
