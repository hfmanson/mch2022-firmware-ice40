: lcd-init-graphics ( -- )

  ." Trying to get access to LCD... Press ESC to exit." cr

  begin
    esc? if welcome quit then
    lcd-ctrl io@ 4 ( LCD_MODE ) and
  until

  1 ( LCD_CS_N )                    lcd-ctrl io!
  1 ms
  1 ( LCD_CS_N ) 2 ( LCD_RST_N ) or lcd-ctrl io!
  120 ms
                 2 ( LCD_RST_N )    lcd-ctrl io!

  \ Initialisation sequence

  $CF ( ILI9341_POWERB    ) cmd>lcd $00 >lcd $C1 >lcd $30 >lcd
  $ED ( ILI9341_POWER_SEQ ) cmd>lcd $64 >lcd $03 >lcd $12 >lcd $81 >lcd
  $E8 ( ILI9341_DTCA      ) cmd>lcd $85 >lcd $00 >lcd $78 >lcd
  $CB ( ILI9341_POWERA    ) cmd>lcd $39 >lcd $2C >lcd $00 >lcd $34 >lcd $02 >lcd
  $F7 ( ILI9341_PRC       ) cmd>lcd $20 >lcd
  $EA ( ILI9341_DTCB      ) cmd>lcd $00 >lcd $00 >lcd
  $C0 ( ILI9341_LCMCTRL   ) cmd>lcd $23 >lcd
  $C1 ( ILI9341_POWER2    ) cmd>lcd $10 >lcd
  $C5 ( ILI9341_VCOM1     ) cmd>lcd $3e >lcd $28 >lcd
  $C7 ( ILI9341_VCOM2     ) cmd>lcd $86 >lcd
  $36 ( ILI9341_MADCTL    ) cmd>lcd $08 >lcd
  $3A ( ILI9341_COLMOD    ) cmd>lcd $55 >lcd
  $B1 ( ILI9341_FRMCTR1   ) cmd>lcd $00 >lcd $18 >lcd
  $B6 ( ILI9341_DFC       ) cmd>lcd $08 >lcd $82 >lcd $27 >lcd
  $F2 ( ILI9341_3GAMMA_EN ) cmd>lcd $00 >lcd
  $26 ( ILI9341_GAMSET    ) cmd>lcd $01 >lcd

  $E0 ( ILI9341_PVGAMCTRL ) cmd>lcd $0F >lcd $31 >lcd $2B >lcd $0C >lcd $0E >lcd
                                    $08 >lcd $4E >lcd $F1 >lcd $37 >lcd $07 >lcd
                                    $10 >lcd $03 >lcd $0E >lcd $09 >lcd $00 >lcd
  $E1 ( ILI9341_NVGAMCTRL ) cmd>lcd $00 >lcd $0E >lcd $14 >lcd $03 >lcd $11 >lcd
                                    $07 >lcd $31 >lcd $C1 >lcd $48 >lcd $08 >lcd
                                    $0F >lcd $0C >lcd $31 >lcd $36 >lcd $0F >lcd

  $F6 ( ILI9341_INTERFACE ) cmd>lcd $00 >lcd $40 >lcd $00 >lcd

  $11 ( ILI9341_SLPOUT    ) cmd>lcd
  $29 ( ILI9341_DISPON    ) cmd>lcd
;

\ -----------------------------------------------------------------------------
\  Procedural graphics
\ -----------------------------------------------------------------------------

: lcd-erase ( -- )
  lcd-init-graphics
  \ Page Address Set (0 to 319)
  $2B cmd>lcd  0 >lcd  0 >lcd  319 8 rshift >lcd 319 $ff and >lcd
  \ Column Address Set (0 to 240)
  $2A cmd>lcd  0 >lcd  0 >lcd  239 8 rshift >lcd 239 $ff and >lcd
  $2C ( ILI9341_RAMWR ) cmd>lcd

  320
  begin
    1-

    240
    begin
      1-
      0 lcd-data io!
      0 lcd-data io!
      dup 0 =
    until
    drop

    dup 0 =
  until
  drop
;

320,0 2constant width
240,0 2constant height

0,0 2variable sx
0,0 2variable cx
0,0 2variable sy
0,0 2variable cy

: window ( rx lx ty by -- )
	2over 2over d- height 2swap f/ sy 2!
	0. 2swap d- sy 2@ f* cy 2! 2drop
	2over 2over d- width  2swap f/ sx 2!
	0. 2swap d- sx 2@ f* cx 2! 2drop
;

: setpixel ( x y -- )
	$2A cmd>lcd dup 8 rshift dup >lcd over >lcd >lcd >lcd
	$2B cmd>lcd dup 8 rshift dup >lcd over >lcd >lcd >lcd
	$2C cmd>lcd $FF >lcd $FF >lcd
;

: pset ( x y -- )
	sy 2@ f* cy 2@ d+ nip >r
	sx 2@ f* cx 2@ d+ nip r>
	setpixel
;

0,0 2variable a
0,0 2variable b
0,0 2variable c

: w ( x -- w )
	2dup
	2dup
	f*
	2dup
	c 2@ f*
	2swap
	1,0
	d+
	f/
	2swap
	a 2@ f*
	d+
;

: mira ( x y -- )
	2over w
	( x y w )
	>r >r

	( x y, w on rstack )
	begin
	( x y )
	2over 2over pset
	( x y )
	r> r>
	( x y w )
	2swap
	( x w y )
	b 2@ f*
	( x w y*b )
	d+
	( x x1 )
	2dup
	( x x1 x1 )
	w
	( x x1 w1)
	2dup
	( x x1 w1 w1)
	>r >r
	( x x1 w1)
	>r >r
	( x x1 )
	2swap
	( x1 x )
	r> r>
	( x1 x w1)
	2swap
	( x1 w1 x)
	d-
	( x1 w1-x)
	esc?
	until
	r> r>
	2drop
	2drop
	2drop
;



: demo1
lcd-erase
( rx: 12.0, lx: -9.0, ty: 6.5, by: -10.5 )
12,0 -9,0 6,5 -10,5 window
( a: -0.48 )
-0,48 a 2!
( b: 0.94 )
0,94 b 2!
( c: 2.0 - 2.0 * a )
2,0 2,0 a 2@ f* d- c 2!
( 4.0 0.0 mira )
4,0 0,0 mira
;

: demo2
lcd-erase
( rx: 12.0, lx: -9.0, ty: 10.5, by: -10.5 )
12,0 -9,0 10,5 -10,5 window
( a: -0.4 )
-0,4 a 2!
( b: 0.99 )
0,99 b 2!
( c: 2.0 - 2.0 * a )
2,0 2,0 a 2@ f* d- c 2!
( 4.0 0.0 mira )
4,0 0,0 mira
;

: demo3
lcd-erase
( rx: 20.0, lx: -20.0, ty: 15.0, by: -15.0 )
20,0 -20,0 15,0 -15,0 window
( a: 0.7 )
0,7 a 2!
( b: 0.9998 )
0,9998 b 2!
( c: 2.0 - 2.0 * a )
2,0 2,0 a 2@ f* d- c 2!
( 0.0 12.0 mira )
0,0 12,0 mira
;

: demo4
lcd-erase
( rx: 100.0, lx: -100.0, ty: 100.0, by: -100.0 )
40,0 -40,0 40,0 -40,0 window
( a: 0.31 )
0,31 a 2!
( b: 1.0 )
1,0 b 2!
( c: 2.0 - 2.0 * a )
2,0 2,0 a 2@ f* d- c 2!
( 0.0 12.0 mira )
12,0 0,0 mira
;
