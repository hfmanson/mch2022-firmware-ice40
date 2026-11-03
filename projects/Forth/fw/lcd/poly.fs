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
