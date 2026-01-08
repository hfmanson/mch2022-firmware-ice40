( loading forth images and SRAM data through FPGA bindings )
( fid is LSB of $DABBAD00 )
( download forth core and image and bindings )
( examples )
( on PC: fpga.py forth.bin 0xdabbad00:soko-image.bin  0xdabbad01:soko-maps.bin 0xdabbad02:snake-image.bin )
( 2 load-forth )
( inside sokoace code, to load levels )
( 1 load-sram )

: fid! ( fid -- ) $900 io! ;
: load-sram ( fid -- ) fid! 1 $910 io! 0 $910 io! ;
: load-forth ( fid -- ) fid! 1 $920 io! ;
