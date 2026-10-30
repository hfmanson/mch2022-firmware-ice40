320,0 2constant width
240,0 2constant height

0,0 2variable sx
0,0 2variable cx
0,0 2variable sy
0,0 2variable cy

: lcd-erase
	-lcd dint nocaption lcd-init page
;

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
