#include <stdbool.h>
#include <stdint.h>

#include "config.h"

volatile uint32_t *fb32 = (volatile uint32_t *)PSRAM_BASE;

void lcd_cls(void)
{
	for (int i = 0; i < 320*240/2; i++)
		fb32[i] = 0x00000000;
}

void psram_set_pixel(int x, int y, uint16_t color)
{
    uint32_t pix_idx = (uint32_t)(y * 240 + x);
    uint32_t word_idx = pix_idx >> 1;
    uint32_t word = fb32[word_idx];

    if (pix_idx & 1)
        word = (word & 0xFFFF0000) | color;           // Lage 16 bits (oneven pixel)
    else
        word = (word & 0x0000FFFF) | ((uint32_t)color << 16); // Hoge 16 bits (even pixel)

    fb32[word_idx] = word;
}

inline int abs(int x)
{
	return x < 0 ? -x : x;
}

void plot_line(int x0, int y0, int x1, int y1, uint16_t color)
{
    int dx = abs(x1 - x0);
    int sx = x0 < x1 ? 1 : -1;
    int dy = -abs(y1 - y0);
    int sy = y0 < y1 ? 1 : -1;
    int error = dx + dy;
    
    for (;;) {
        psram_set_pixel(x0, y0, color);
        int e2 = 2 * error;
        if (e2 >= dy) {
            if (x0 == x1) break;
            error = error + dy;
            x0 = x0 + sx;
        }
        if (e2 <= dx) {
            if (y0 == y1) break;
            error = error + dx;
            y0 = y0 + sy;
        }
    }
}

#include <stdint.h>

#define SCREEN_W 240
#define SCREEN_H 320

#define CENTER_X (SCREEN_W / 2)
#define CENTER_Y (SCREEN_H / 2)

#define TABLE_SIZE 256
#define SCALE      1024

/* Sinustabel geschaald met factor 1024. */
const int16_t sintab[TABLE_SIZE] =
{
     0,    25,    50,    75,   100,   125,   150,   175,
   200,   224,   249,   273,   297,   321,   345,   369,
   392,   415,   438,   460,   483,   505,   526,   548,
   569,   590,   610,   630,   650,   669,   688,   706,
   724,   742,   759,   775,   792,   807,   822,   837,
   851,   865,   878,   891,   903,   915,   926,   936,
   946,   955,   964,   972,   980,   987,   993,   999,
  1004,  1009,  1013,  1016,  1019,  1021,  1023,  1024,
  1024,  1024,  1023,  1021,  1019,  1016,  1013,  1009,
  1004,   999,   993,   987,   980,   972,   964,   955,
   946,   936,   926,   915,   903,   891,   878,   865,
   851,   837,   822,   807,   792,   775,   759,   742,
   724,   706,   688,   669,   650,   630,   610,   590,
   569,   548,   526,   505,   483,   460,   438,   415,
   392,   369,   345,   321,   297,   273,   249,   224,
   200,   175,   150,   125,   100,    75,    50,    25,
     0,   -24,   -49,   -74,   -99,  -124,  -149,  -174,
  -199,  -223,  -248,  -272,  -296,  -320,  -344,  -368,
  -391,  -414,  -437,  -459,  -482,  -504,  -525,  -547,
  -568,  -589,  -609,  -629,  -649,  -668,  -687,  -705,
  -723,  -741,  -758,  -774,  -791,  -806,  -821,  -836,
  -850,  -864,  -877,  -890,  -902,  -914,  -925,  -935,
  -945,  -954,  -963,  -971,  -979,  -986,  -992,  -998,
 -1003, -1008, -1012, -1015, -1018, -1020, -1022, -1023,
 -1023, -1023, -1022, -1020, -1018, -1015, -1012, -1008,
 -1003,  -998,  -992,  -986,  -979,  -971,  -963,  -954,
  -945,  -935,  -925,  -914,  -902,  -890,  -877,  -864,
  -850,  -836,  -821,  -806,  -791,  -774,  -758,  -741,
  -723,  -705,  -687,  -668,  -649,  -629,  -609,  -589,
  -568,  -547,  -525,  -504,  -482,  -459,  -437,  -414,
  -391,  -368,  -344,  -320,  -296,  -272,  -248,  -223,
  -199,  -174,  -149,  -124,   -99,   -74,   -49,   -24
};

/* cos(x) = sin(x + 90°) */
#define COS(a) sintab[((a) + 64) & 255]
#define SIN(a) sintab[(a) & 255]

#define MAX_SIDES 30

int x[MAX_SIDES];
int y[MAX_SIDES];

void draw_polygon(int sides, int radius, int angle, uint16_t color)
{
    int i, j;

    int step = TABLE_SIZE / sides;
	
    for (i = 0; i < sides; i++)
    {
        x[i] = CENTER_X + ((COS(angle) * radius) >> 10);
        y[i] = CENTER_Y + ((SIN(angle) * radius) >> 10);
		angle += step;
	}
    for (i = 0; i < sides - 1; i++)
    {
		int xi = x[i];
		int yi = y[i];
		for (j = i + 1; j < sides; j++)
		{
            plot_line(xi, yi, x[j], y[j], color);
		}
	}	
}
