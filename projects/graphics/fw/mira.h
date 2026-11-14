#include "lcd.h"
#include "q.h"

#define Q_WIDTH  0x01400000
#define Q_HEIGHT 0x00f00000

#define W_LX 0xfff70000 //  -9.0
#define W_RX 0x000c0000 //  12.0
#define W_BY 0xfff58000 // -10.5
#define W_TY 0x00068000 // 6.5

#define M_X  0x00040000 // 4.0
#define M_Y  0x00000000 // 0.0
#define M_A  0xffff851f // -0.48
#define M_B  0x0000f0a4 // 0.94

typedef  int16_t hd_t; /* half Q width,      signed */
typedef uint64_t lu_t; /* double Q width,  unsigned */

void window(q_t lx, q_t rx, q_t ty, q_t by);
