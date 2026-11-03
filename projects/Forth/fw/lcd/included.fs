
\ Definitions in high-level Forth that can be compiled by the small
\ nucleus itself. They are included into the bitstream for default.

\ #######   CORE   ############################################

: [']
    '
; immediate 0 foldable

: [char]
    char
; immediate 0 foldable

: (
    [char] ) parse 2drop
; immediate 0 foldable

: u>= ( u1 u2 -- ? ) u< invert ; 2 foldable
: u<= ( u1 u2 -- ? ) u> invert ; 2 foldable
: >=  ( n1 n2 -- ? )  < invert ; 2 foldable
: <=  ( n1 n2 -- ? )  > invert ; 2 foldable

: else
    postpone ahead
    swap
    postpone then
; immediate

: while
    postpone if
    swap
; immediate

: repeat
     postpone again
     postpone then
; immediate

: create ( "<name>" -- ; -- addr )
    :
    here 2 cells + postpone literal
    postpone ;
;

: buffer: ( u "<name>" -- ; -- addr )
   create allot 0 foldable
;

: >body ( addr -- addr' )
    @ -1 1 rshift and \ Remove the literal opcode MSB
;

: m* ( n1 n2 -- d )
    2dup xor >r
    abs swap abs um*
    r> 0< if dnegate then
; 2 foldable

: variable ( x "name" -- ; -- addr )
    create ,
    0 foldable
;

: constant ( x "name" -- ; -- x ) : postpone literal postpone ; 0 foldable ;

: sgn ( u1 n1 -- n2 ) \ n2 is u1 with the sign of n1
    0< if negate then
; 2 foldable

\ Divide d1 by n1, giving the symmetric quotient n3 and the remainder
\ n2.
: sm/rem ( d1 n1 -- n2 n3 )
    2dup xor >r     \ combined sign, for quotient
    over >r         \ sign of dividend, for remainder
    abs >r dabs r>
    um/mod          ( remainder quotient )
    swap r> sgn     \ apply to remainder
    swap r> sgn     \ apply to quotient
; 3 foldable

\ Divide d1 by n1, giving the floored quotient n3 and the remainder n2.
\ Adapted from hForth
: fm/mod ( d1 n1 -- n2 n3 )
    dup >r 2dup xor >r
    >r dabs r@ abs
    um/mod
    r> 0< if
        swap negate swap
    then
    r> 0< if
        negate         \ negative quotient
        over if
            r@ rot - swap 1-
        then
    then
    r> drop
; 3 foldable

: */mod ( n1 n2 n3 -- n4 n5 ) >r m* r> sm/rem ; 3 foldable
: */    ( n1 n2 n3 -- n4 )    */mod nip ; 3 foldable

: spaces ( n -- )
    begin
        dup 0>
    while
        space 1-
    repeat
    drop
;

( Pictured numeric output                    JCB 08:06 07/18/14)
\ Adapted from hForth

\ "The size of the pictured numeric output string buffer shall
\ be at least (2*n) + 2 characters, where n is the number of
\ bits in a cell."

create BUF0
16 cells 2 + 128 max
allot here constant BUF

0 variable hld

: <# ( -- )
    BUF hld !
;

: hold ( c -- )
    hld @ 1- dup hld ! c!
;

: sign ( n -- )
    0< if
        [char] - hold
    then
;

: .digit ( u -- c )
  9 over <
  [char] A [char] 9 1 + -
  and +
  [char] 0 +
;

: # ( ud -- ud* )
    0 base @ um/mod >r base @ um/mod swap
    .digit hold r>
;

: #s ( ud -- 0 0 )
    begin
        #
        2dup d0=
    until
;

: #> ( ud -- addr len )
    2drop hld @ BUF over -
;

: (d.) ( d -- addr len )
    dup >r dabs <# #s r> sign #>
;

: ud. ( ud -- )
    <# #s #> type space
;

: d. ( d -- )
    (d.) type space
;

: . ( n -- )
    s>d d.
;

: u. ( u -- )
    0 d.
;

: rtype ( caddr u1 u2 -- ) \ display character string specified by caddr u1
                           \ in a field u2 characters wide.
  2dup u< if over - spaces else drop then
  type
;

: d.r ( d length -- )
    >r (d.)
    r> rtype
;

: .r ( n length -- )
    >r s>d r> d.r
;

: u.r ( u length -- )
    0 swap d.r
;

( Memory operations                          JCB 18:02 05/31/15)

: move ( addr1 addr2 u -- )
    >r 2dup u< if
        r> cmove>
    else
        r> cmove
    then
;

: /mod ( n1 n2 -- n3 n4 ) >r s>d r> sm/rem ; 2 foldable
: /    ( n1 n2 -- n3 )    /mod nip ; 2 foldable
: mod  ( n1 n2 -- n3 )    /mod drop ; 2 foldable

: ."
    [char] " parse
    state @ if
        postpone sliteral
        postpone type
    else
        type
    then
; immediate 0 foldable

\ #######   CORE EXT   ########################################

: pad ( -- addr )
    here aligned
;

: within ( n1|u1 n2|u2 n3|u3 -- flag ) over - >r - r> u< ; 3 foldable

: s"
    [char] " parse
    state @ if
        postpone sliteral
    then
; immediate

( CASE                                       JCB 09:15 07/18/14)
\ From ANS specification A.3.2.3.2

: case ( -- 0 ) 0 ; immediate  ( init count of ofs )

: of  ( #of -- orig #of+1 / x -- )
    1+    ( count ofs )
    >r    ( move off the stack in case the control-flow )
          ( stack is the data stack. )
    postpone over  postpone = ( copy and test case value)
    postpone if    ( add orig to control flow stack )
    postpone drop  ( discards case value if = )
    r>             ( we can bring count back now )
; immediate

: endof ( orig1 #of -- orig2 #of )
    >r   ( move off the stack in case the control-flow )
         ( stack is the data stack. )
    postpone else
    r>   ( we can bring count back now )
; immediate

: endcase  ( orig1..orign #of -- )
    postpone drop  ( discard case value )
    0 ?do
      postpone then
    loop
; immediate

\ #######   DICTIONARY   ######################################

: cornerstone ( "name" -- )
  create
    forth 2@        \ preserve FORTH and DP after this
    , 2 cells + ,
  does>
    2@ forth 2! \ restore FORTH and DP
;

\ -------------------------------------------------------------
\  Double tools
\ -------------------------------------------------------------

: 2or  ( d1 d2 -- d ) >r swap >r or  r> r> or  ; 4 foldable
: 2and ( d1 d2 -- d ) >r swap >r and r> r> and ; 4 foldable
: 2xor ( d1 d2 -- d ) >r swap >r xor r> r> xor ; 4 foldable

: d0<   ( d -- ? ) nip 0< ; 2 foldable

: d= ( x0 x1 y0 y1 -- ? )

  swap ( x0 x1 y1 y0 )
  >r   ( x0 x1 y1 R: y0 )
  =    ( x0 x1=y1 R: y0 )
  swap ( x1=y1 x0 R: y0 )
  r>   ( x1=y1 x0 y0 )
  =    ( x1=y1 x0=y0 )
  and
; 4 foldable

: d<> d= not ; 4 foldable

: d2/  ( x1 x2 -- x1' x2' ) >r 1 rshift r@ 8 cells 1- lshift or r> 2/       ; 2 foldable
: dshr ( x1 x2 -- x1' x2' ) >r 1 rshift r@ 8 cells 1- lshift or r> 1 rshift ; 2 foldable

\ : 2lshift  ( ud u -- ud* ) begin dup while >r d2*  r> 1- repeat drop ; 3 foldable
\ : 2arshift (  d u --  d* ) begin dup while >r d2/  r> 1- repeat drop ; 3 foldable
\ : 2rshift  ( ud u -- ud* ) begin dup while >r dshr r> 1- repeat drop ; 3 foldable

: 2lshift ( low high u -- )
  dup >r ( low high u R: u )
  lshift ( low high* )
  over 8 cells r@ - rshift or
  over r@ 8 cells - lshift or
  swap r> lshift swap
; 3 foldable

: 2rshift ( low high u -- )
  >r swap ( high low R: u )
  r@ rshift
  over 8 cells r@ - lshift or
  over r@ 8 cells - rshift or
  swap
  r> rshift
; 3 foldable

: 2arshift ( low high u -- )
  dup >r 8 cells u< ( low high R: u )
  if
    swap ( high low R: u )
    r@ rshift
    over 8 cells r@ - lshift or
  else
    nip dup r@ 8 cells - arshift
  then
  swap r> arshift
; 3 foldable

: 2nip ( d1 d2 -- d2 )
  >r nip nip r>
; 4 foldable

: 2rot ( d1 d2 d3 -- d2 d3 d1 )
  >r >r ( d1 d2 R: d3 )
  2swap ( d2 d1 R: d3 )
  r> r> ( d2 d1 d3 )
  2swap ( d2 d3 d1 )
; 6 foldable

: d<            \ ( al ah bl bh -- flag )
    rot         \ al bl bh ah
    2dup =
    if
        2drop u<
    else
        > nip nip
    then
; 4 foldable

: d>  ( d1 d2 -- ? ) 2swap d< ; 4 foldable
: d>= ( d1 d2 -- ? ) d< not   ; 4 foldable
: d<= ( d1 d2 -- ? ) d> not   ; 4 foldable

: dmin ( d1 d2 -- d ) 2over 2over d< if 2drop else 2nip then ; 4 foldable
: dmax ( d1 d2 -- d ) 2over 2over d< if 2nip else 2drop then ; 4 foldable

: du<           \ ( al ah bl bh -- flag )
    rot         \ al bl bh ah
    2dup =
    if
        2drop u<
    else
        u> nip nip
    then
; 4 foldable

: du>  ( d1 d2 -- ? ) 2swap du< ; 4 foldable
: du>= ( d1 d2 -- ? ) du< not   ; 4 foldable
: du<= ( d1 d2 -- ? ) du> not   ; 4 foldable

\ -------------------------------------------------------------
\  Fixpoint output
\ -------------------------------------------------------------

: hold< ( c -- ) \ Add a character at the end of the number string
  hld @   dup 1- dup hld !    BUF hld @ -  move
  BUF 1- c!
;

: f# ( u -- u ) base @ um* .digit hold< ;

: f.n ( f n -- ) ( f-Low f-High n -- ) \ Prints a s15.16 number

  >r ( Low High R: n )

  dup 0< if [char] - emit then
  dabs
  ( uLow uHigh )
  0 <# #s   ( uLow 0 0 )
  drop swap ( 0 uLow )

  [char] , hold<
  r> 0 ?do f# loop

  #> type space
;

: f. ( f -- ) 8 cells f.n ;

\ -------------------------------------------------------------
\  Fixpoint calculations
\ -------------------------------------------------------------

: 2variable ( d -- ) create , , 0 foldable ;
\ : 2constant ( d -- ) create , , 0 foldable does> 2@ ;
: 2constant ( d -- ) swap : postpone literal postpone literal postpone ; 0 foldable ;

: s>f ( n -- f ) 0 swap ; 1 foldable  \ Signed integer --> Fixpoint s15.16
\ : f>s ( f -- n ) nip    ; 2 foldable  \ Fixpoint s15.16 --> Signed integer

: f* ( f1 f2 -- f )

        dup >r dabs
  2swap dup >r dabs

            ( d c b a )
  swap >r   ( d c a R: b )
  2dup *    ( d c a ac R: b )
  >r        ( d c a R: b ac )
  >r        ( d c R: b ac a )
  over      ( d c d R: b ac a )
  r> um*    ( d c L H R: b ac )
  r> +      ( d c L H' R: b )
  rot       ( d L H' c R: b )
  r@        ( d L H' c b R: b )
  um* d+    ( d L' H'' R: b )
  rot       ( L' H'' d R: b )
  r>        ( L' H'' d b )
  um* nip 0 ( L' H'' db 0 )
  d+        ( L'' H''' )

  r> r> xor 0< if dnegate then

; 4 foldable

0. 2variable dividend
0. 2variable shift
0. 2variable divisor

: (ud/mod) ( -- )

  16 cells
  begin

    \ Shift the long chain of four cells.

       dividend cell+ @ dup 8 cells 1- rshift >r 2*    dividend cell+ !
    r> dividend       @ dup 8 cells 1- rshift >r 2* or dividend       !
    r>    shift cell+ @ dup 8 cells 1- rshift >r 2* or    shift cell+ !
    r>    shift       @                          2* or    shift       !

    \ Subtract divisor when shifted out value is large enough

    shift 2@ divisor 2@  du>=

    if \ Greater or Equal: Subtract !
      shift 2@ divisor 2@ d- shift 2!
      dividend cell+ @ 1+ dividend cell+ !
    then

    1- dup 0=
  until
  drop
;

: ud/mod ( ud1 ud2 -- ud-rem ud-div )

     divisor 2!
  0. shift 2!
     dividend 2!

  (ud/mod)

  shift 2@
  dividend 2@

; 4 foldable

: f/ ( f1 f2 -- f )

  dup >r dabs  divisor 2!
  dup >r dabs  0 Shift 2! 0 swap dividend 2!

  (ud/mod)

  dividend 2@
  r> r> xor 0< if dnegate then

; 4 foldable

\ -------------------------------------------------------------
\   Most positive and negative s15.16 values possible
\ -------------------------------------------------------------

$FFFF $7FFF 2constant +inf  \  32767,9999847412109375
$0000 $8000 2constant -inf  \ -32768,0000000000000000

\ -------------------------------------------------------------
\  Sine and Cosine with Cordic algorithm
\ -------------------------------------------------------------

hex
create numbertable
C90F , 76B1 , 3EB6 , 1FD5 , 0FFA , 07FF , 03FF , 01FF , 00FF , 007F , 003F , 001F , 000F , 0007 , 0003 , 0001 ,
decimal

: e^ka ( u -- x ) 2* numbertable + @ ; 1 foldable

\ -----------------------------------------------------------------------------
\   Common building blocks for different CORDIC modes
\ -----------------------------------------------------------------------------

0. 2variable cordic-x
0. 2variable cordic-y
0. 2variable cordic-z

: cordic-step-plus ( i -- ) >r
    cordic-x 2@ cordic-y 2@ r@ 2arshift d+
    cordic-y 2@ cordic-x 2@ r@ 2arshift d- cordic-y  2!
    cordic-z 2@             r> e^ka 0   d+ cordic-z  2!
                                           cordic-x  2!
;

: cordic-step-minus ( i -- ) >r
    cordic-x 2@ cordic-y 2@ r@ 2arshift d-
    cordic-y 2@ cordic-x 2@ r@ 2arshift d+ cordic-y  2!
    cordic-z 2@             r> e^ka 0   d- cordic-z  2!
                                           cordic-x  2!
;

\ -----------------------------------------------------------------------------
\   Angle --> Sine and Cosine
\ -----------------------------------------------------------------------------

: cordic-sincos ( f-angle -- f-cosine f-sine )
                ( Angle between -Pi/2 and +Pi/2 ! )

  $0,9B74      cordic-x 2! \ Scaling value to cancel gain of the algorithm
   0,0         cordic-y 2!
               cordic-z 2!

  0
  begin
    dup
    cordic-z @ 0< \ 2@ d0<
    if
      cordic-step-plus
    else
      cordic-step-minus
    then

    1+ dup 16 =
  until
  drop
;

: (sin) ( f-angle -- f-sine )   cordic-sincos cordic-y 2@ ; 2 foldable
: (cos) ( f-angle -- f-cosine ) cordic-sincos cordic-x 2@ ; 2 foldable

3,14159   2constant pi
pi 2,0 f/ 2constant pi/2
pi 4,0 f/ (cos) f. \ Displays cos(Pi/4)

: cos ( f-angle -- f-cosine )
  dabs
  pi/2 ud/mod drop 3 and ( Quadrant f-angle )

  case
    0 of                 (cos)         endof
    1 of dnegate pi/2 d+ (cos) dnegate endof
    2 of                 (cos) dnegate endof
    3 of dnegate pi/2 d+ (cos)         endof
  endcase

; 2 foldable

: sin ( f-angle -- f-sine )
  dup >r \ Save sign
  dabs
  pi/2 ud/mod drop 3 and ( Quadrant f-angle )

  case
    0 of                 (sin)          endof
    1 of dnegate pi/2 d+ (sin)          endof
    2 of                 (sin)  dnegate endof
    3 of dnegate pi/2 d+ (sin)  dnegate endof
  endcase

  r> 0< if dnegate then
; 2 foldable

: cossin ( f-angle -- f-cosine f-sine )
  dup >r \ Save sign
  dabs
  pi/2 ud/mod drop 3 and ( Quadrant f-angle )

  case
    0 of                 cordic-sincos cordic-x 2@         cordic-y 2@         endof
    1 of dnegate pi/2 d+ cordic-sincos cordic-x 2@ dnegate cordic-y 2@         endof
    2 of                 cordic-sincos cordic-x 2@ dnegate cordic-y 2@ dnegate endof
    3 of dnegate pi/2 d+ cordic-sincos cordic-x 2@         cordic-y 2@ dnegate endof
  endcase

  r> 0< if dnegate then
; 2 foldable

\ -----------------------------------------------------------------------------
\  Integer XY vector --> Polar coordinates
\ -----------------------------------------------------------------------------

: cordic-vectoring ( x y -- d-r f-angle )

  \ The CORDIC algorithm on its own works fine with angles between -Pi/2 ... +Pi/2.
  \ Need to handle angles beyond, which translates to if x < 0,  by an additional step:

  over 0<  \ x < 0 ?
  if
    negate swap negate swap

    dup 0< if \ Now y < 0 ?
      pi         cordic-z 2!
    else
      pi dnegate cordic-z 2!
    then
  else
    0,0  cordic-z 2!
  then

  \ Improve accuracy by exploiting 32 bit dynamic range during calculations

  s>d 12 2lshift cordic-y 2!
  s>d 12 2lshift cordic-x 2!

  0
  begin
    dup
    cordic-y @ 0< \ 2@ d0<
    if
      cordic-step-minus
    else
      cordic-step-plus
    then

    1+ dup 16 =
  until
  drop
;

: atan2     ( x y -- f-angle )             cordic-vectoring cordic-z 2@             ; 2 foldable
: xy>polar  ( x y -- f-angle d-magnitude ) atan2 cordic-x 2@ $0,9B74 f* 12 2arshift ; 2 foldable
: magnitude ( x y -- d-magnitude )         xy>polar 2nip                            ; 2 foldable

\ -------------------------------------------------------------
\  Square and root
\ -------------------------------------------------------------

: sqr ( f -- f^2 ) 2dup f* ; 2 foldable

: sqrt ( f -- sqrt ) ( L H )

  >r >r

  0,0

    2dup $0080 or      sqr   r> r@ over >r  du<= if $0080 or then
    2dup $0040 or      sqr   r> r@ over >r  du<= if $0040 or then
    2dup $0020 or      sqr   r> r@ over >r  du<= if $0020 or then
    2dup $0010 or      sqr   r> r@ over >r  du<= if $0010 or then

    2dup $0008 or      sqr   r> r@ over >r  du<= if $0008 or then
    2dup $0004 or      sqr   r> r@ over >r  du<= if $0004 or then
    2dup $0002 or      sqr   r> r@ over >r  du<= if $0002 or then
    2dup $0001 or      sqr   r> r@ over >r  du<= if $0001 or then

    over $8000 or over sqr   r> r@ over >r  du<= if swap $8000 or swap then
    over $4000 or over sqr   r> r@ over >r  du<= if swap $4000 or swap then
    over $2000 or over sqr   r> r@ over >r  du<= if swap $2000 or swap then
    over $1000 or over sqr   r> r@ over >r  du<= if swap $1000 or swap then

    over $0800 or over sqr   r> r@ over >r  du<= if swap $0800 or swap then
    over $0400 or over sqr   r> r@ over >r  du<= if swap $0400 or swap then
    over $0200 or over sqr   r> r@ over >r  du<= if swap $0200 or swap then
    over $0100 or over sqr   r> r@ over >r  du<= if swap $0100 or swap then

  rdrop rdrop
; 2 foldable

\ -------------------------------------------------------------
\  Exponential function
\ -------------------------------------------------------------

\ s31.32 comma parts of all but first coefficient in Horner expansion of
\ a partial sum of the series expansion of exp(x).  The whole parts are 0
\ and are supplied in code.

create exp-coef

$1745 , \  1/11
$1999 , \  1/10
$1C71 , \  1/9
$2000 , \  1/8
$24AD , \  1/7
$2AAA , \  1/6
$3333 , \  1/5
$4000 , \  1/4
$5555 , \  1/3
$8000 , \  1/2

: exp-1to1 ( x -- expx )
  \ Calculate exp(x) for x an s31.32 value.  Values are correct when
  \ when rounded to six decimal places when x is between +/-0.7.  Uses an
  \ 11-term partial sum evaluated using Horner's method.
  \ Calculate Horner terms
  1,0   \ Starting Horner term is 1
  10 0 do
    \ Multiply last term by x and coefficient, then add to get new term
    2over f* i   2* exp-coef + @     0 f* 0 1 d+
  loop
  \ Last part of expansion
  2over f* 0 1 d+
  2nip
; 2 foldable

$B172 $0000 2constant lnof2 \      0,6931457519531250

\ Return the floor of an s15.16 value df
: floor ( df -- df ) nip 0 swap ; 2 foldable

: pow2 ( x -- 2^x )
  \ Return 2 raised to the power x where x is s15.16
  \ If x is 0, return 1
  2dup 0,0 d= if 2drop 1,0 exit then
  \ If x < -16, 0 is returned.  If x >= 15, returns s15.16 ceiling
  2dup -16,0 d< if 2drop 0,0 exit then
  2dup 15,0 d< not if 2drop +inf exit then
  \ Get largest integer n such that n <= x so x = z + n, 0 <= z < 1
  2dup floor 2swap 2over d-
  ( n z )
  \ Get exp(z*ln2) = 2^z, then shift n times to get 2^x = (2^n)*(2^z)
  lnof2 f* exp-1to1 2swap nip
  ( 2^z n )  \ n now a single
  dup 0= if
    drop
  else
    dup 0< if
      negate 2rshift
    else
      2lshift
    then
  then
; 2 foldable

$7154 $0001 2constant 1overlnof2

: exp ( f -- exp ) \ exp(x) = pow2(x/ln(2))
  1overlnof2 f* pow2
; 2 foldable

\ -------------------------------------------------------------------------
\  Helper for logarithmic functions
\ -------------------------------------------------------------------------

: log2-1to2 ( y -- log2y )
  \ Helper function that requires y is s31.32 value with 1 <= y < 2
  0 0 2swap 0
  ( retval y cum_m )
  \ while((cum_m < 33) && (y > 1))
  begin dup 2over
    ( retval y cum_m cum_m y )
    1,0 d> swap 33 < and while
    ( retval y cum_m )
    rot rot 0 -rot        \ m = 0, z = y
    ( retval cum_m m z)
    \ Do z = z*z, m = m+1 until 2 <= z.  We also get z < 4
    begin
      2dup f* rot 1 + -rot
      ( retval cum_m m z )
      2dup 2,0 d< not
    until
    \ At this point z = y^(2^m) so that log2(y) = (2^(-m))*log2(z)
    \ = (2^(-m))*(1 + log2(z/2)) and 1 <= z/2 < 2
    \ We will add m to cum_m and add 2*(-cum_m) to the returned value,
    \ then iterate with a new y = z/2
    ( retval cum_m m z )
    2swap + -rot dshr >r >r   \ cum_m = cum_m + m, y = z/2
    ( retval cum_m ) ( R: y=z/2 )
    \ retval = retval + 2^-cum_m
    dup 1,0 rot 0 do dshr loop
    ( retval cum_m 2^-cum_m )
    rot >r d+
    ( retval ) ( R: y cum_m )
    r> r> r> rot
    ( retval y cum_m )
  repeat
  drop 2drop

; 2 foldable

\ -------------------------------------------------------------------------
\  Logarithmic functions
\ -------------------------------------------------------------------------

: log2 ( x -- log2x )
  \ Calculates base 2 logarithm of positive s15.16 value x

  \ Treat error and special cases
  \ Check that x > 0.  If not, return "minus infinity"
  2dup 0,0 d> not if 2drop -inf exit then
  \ If x = 1, return 0
  2dup 1,0 d= if 2drop 0,0 exit then

  \ Find the n such that 1 <= (2^(-n))*x < 2
  \ This n is the integer part (characteristic) of log2(x)
  0 -rot
  ( n=0 y=x )
  2dup 1,0 d> if
    \ Do n = n+1, y = y/2 while (y >= 2)
    begin 2dup 2,0 d< not while
      ( n y )
      dshr rot 1 + -rot
    repeat
  else
    \ Do n = n-1, y = 2*y while (y < 1)
    begin 2dup 1,0 d< while
      ( n y )
      d2* rot 1 - -rot
    repeat
  then

  \ Now y = (2^(-n))*x so log2(x) = n + log2(y) and we use the
  \ helper function to get log2(y) since 1 <= y < 2
  log2-1to2 rot 0 swap d+
  ( log2x )
; 2 foldable


: ln ( x -- lnx )
  \ Return the natural logarithm of a postive s15.16 value x
  log2 lnof2 f*
; 2 foldable

\ #######   MEMORY   ##########################################

: unused ( -- u ) $3000 here - ; \ 12 kb

\ #######   IO   ##############################################

: cycles ( -- u ) $8000 io@ ;

   24 constant cycles/us  \ For 24 MHz
24000 constant cycles/ms

: delay-cycles ( cycles -- )
  cycles ( cycles start )
  begin
    pause
    2dup ( cycles start cycles start )
    cycles ( cycles start cycles start current )
    swap - ( cycles start cycles elapsed )
    u<=
  until
  2drop
;

: us ( u -- )       cycles/us *  delay-cycles      ;
: ms ( u -- ) 0 ?do cycles/ms    delay-cycles loop ;

: randombit ( -- 0 | 1 ) $2000 io@ 2 rshift 1 and ;
: random ( -- x ) 0  16 0 do 2* randombit or 100 0 do loop loop ;

: ticks ( -- u ) $4000 io@ ;

: nextirq ( cycles -- ) \ Trigger the next interrupt u cycles after the last one.
  $4000 io@  \ Read current tick
  -           \ Subtract the cycles already elapsed
  8 -          \ Correction for the cycles neccessary to do this
  invert        \ Timer counts up to zero to trigger the interrupt
  $4000 io!      \ Prepare timer for the next irq
;

: sram@ ( addr -- x ) $0800 io! $0810 io@ ;
: sram! ( x addr -- ) $0800 io! $0810 io! ;

: esc? ( -- ? ) key? if key 27 = else false then ;

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

: sinplot
319 0 do i i s>f 29,0 f/ sin 100,0 f* 100,0 d+ nip setpixel loop ;

pi 2,0 f* 2constant 2pi
create sintab 40 allot
create costab 40 allot

: poly ( color n -- )
dup
>r
2pi r@ s>f f/
0,0
r> 0 do   
   2dup sin 100,0 f* 160,0 d+ nip i 2* sintab + !
   2dup cos 100,0 f* 120,0 d+ nip i 2* costab + !
   2over d+ 
loop
2drop
2drop
dup 1- 0 do 
   dup i 1+ do
     over
     j 2* sintab + @
     i 2* sintab + @
     j 2* costab + @
     i 2* costab + @
     draw-line
     200 us
   loop
loop
drop
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
cls
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
cls
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
cls
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
cls
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
create sintab
    0 , 3 , 6 , 9 , 12 , 15 , 18 , 21 , 24 , 28 , 31 , 34 , 37 , 40 , 43 ,
    46 , 48 , 51 , 54 , 57 , 60 , 63 , 65 , 68 , 71 , 73 , 76 , 78 , 81 ,
    83 , 85 , 88 , 90 , 92 , 94 , 96 , 98 , 100 , 102 , 104 , 106 , 108 ,
    109 , 111 , 112 , 114 , 115 , 116 , 118 , 119 , 120 , 121 , 122 ,
    123 , 124 , 124 , 125 , 126 , 126 , 127 , 127 , 127 , 127 , 127 ,
    127 , 127 , 127 , 127 , 127 , 127 , 126 , 126 , 125 , 124 , 124 , 123
        , 122 , 121 , 120 , 119 , 118 , 117 , 115 , 114 , 112 , 111 , 109 ,
    108 , 106 , 104 , 102 , 100 , 99 , 97 , 94 , 92 , 90 , 88 , 86 , 83 ,
    81 , 78 , 76 , 73 , 71 , 68 , 65 , 63 , 60 , 57 , 54 , 52 , 49 , 46 ,
    43 , 40 , 37 , 34 , 31 , 28 , 25 , 22 , 18 , 15 , 12 , 9 , 6 , 3 ,
    0 , -2 , -6 , -9 , -12 , -15 , -18 , -21 , -24 , -27 , -30 , -33 ,
    -36 , -39 , -42 , -45 , -48 , -51 , -54 , -57 , -60 , -62 , -65 ,
    -68 , -70 , -73 , -76 , -78 , -81 , -83 , -85 , -88 , -90 , -92 ,
    -94 , -96 , -98 , -100 , -102 , -104 , -106 , -107 , -109 , -111 ,
    -112 , -114 , -115 , -116 , -118 , -119 , -120 , -121 , -122 , -123 ,
    -124 , -124 , -125 , -126 , -126 , -127 , -127 , -127 , -127 , -127 ,
    -127 , -127 , -127 , -127 , -127 , -127 , -126 , -126 , -125 , -124 ,
    -124 , -123 , -122 , -121 , -120 , -119 , -118 , -117 , -115 , -114 ,
    -113 , -111 , -109 , -108 , -106 , -104 , -103 , -101 , -99 , -97 ,
    -95 , -92 , -90 , -88 , -86 , -83 , -81 , -79 , -76 , -74 , -71 ,
    -68 , -66 , -63 , -60 , -57 , -55 , -52 , -49 , -46 , -43 , -40 ,
    -37 , -34 , -31 , -28 , -25 , -22 , -19 , -16 , -12 , -9 , -6 , -3 ,
	
0 variable t	
: color-index ( j i -- color-index )
  t @ + $ff and 2* sintab + @ 256 + swap 128 + t @ $ff and 2* sintab + @ + $ff and 2* sintab + @ + 2/
;

: color ( color-index factor -- color )
  * t @ + $ff and 2* sintab + @ 128 +
;

: my-draw-rect ( color x y )
  3 lshift
  >r
  3 lshift
  dup
  7 +
  r>
  dup
  7 +
  draw-rect
;

: draw-part ( j i -- )
  over over
  color-index
  >r
  r@ 1 color
  3 rshift 11 lshift
  r@ 7 color
  2 rshift 5 lshift
  r> 2 color
  $1f and
  or or
  -rot
  my-draw-rect
;

: plasma
  30 0 do
    40 0 do
    i     j draw-part
	i 1 + j draw-part
	i 2 + j draw-part
	i 3 + j draw-part
    4 +loop
  loop
;

: plasma1
  begin
	begin $0890 io@ 4 and until
    plasma
    t @ 1 + t !
  esc? until
;
( loading forth images and SRAM data through FPGA bindings )
( fid is LSB of $DABBAD00 )
( download forth core and image and bindings )
( examples )
( on PC: fpga.py forth.bin 0xdabbad00:soko-image.bin  0xdabbad01:soko-maps.bin 0xdabbad02:snake-image.bin )
( 2 load-forth )
( inside sokoace code, to load levels )
( 1 load-sram )

: fid! ( fid -- ) $900 io! ;
: load-sram ( fid -- ) fid! 1 $910 io! ;
: load-forth ( fid -- ) fid! 1 $920 io! ;
( play sounds through SPI using ESP32 soundserver )

: sound! ( sid -- ) $930 io! ;
