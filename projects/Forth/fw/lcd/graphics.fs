
\ -----------------------------------------------------------------------------
\ 320x240 LCD Display ILI9341
\ -----------------------------------------------------------------------------

\ Registers:

  $0890 constant lcd-ctrl  \ LCD control lines
  $08A0 constant lcd-data  \ LCD data, writeonly. Set $100 for commands.

\ -----------------------------------------------------------------------------

: cmd>lcd ( c -- ) $100 or lcd-data io! ; \ Logic handles the command/data line
:    >lcd ( c -- ) $FF and lcd-data io! ;  \ and pulses write line.

\ -----------------------------------------------------------------------------

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
  
  $35 ( ILI9341_TEON      ) cmd>lcd $00 >lcd
;

: draw-rect ( color x1 x2 y1 y2 -- )
  dup
  $970 io!
  over - 1+ >r
  $950 io!
  dup
  $980 io!
  over - 1+ r> um*
  $990 io!
  $9A0 io!
  $960 io!
  $940 io!
;


: draw-line ( color x1 x2 y1 y2 -- )
  $970 io!
  $950 io!
  $980 io!
  $960 io!
  $9B0 io!
;

: cls
0 0 319 0 239 draw-rect
;

: setpixel ( x y -- )
$2A cmd>lcd dup 8 rshift dup >lcd over >lcd >lcd >lcd
$2B cmd>lcd dup 8 rshift dup >lcd over >lcd >lcd >lcd
$2C cmd>lcd $FF >lcd $FF >lcd
;

' lcd-init-graphics init !

