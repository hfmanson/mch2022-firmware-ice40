#include "mira.h"

static q_t cx, sx, cy, sy;

void window(q_t lx, q_t rx, q_t ty, q_t by)
{
	sx = qdiv(Q_WIDTH, qsub(rx, lx));
	cx = qmul(qnegate(lx), sx);
	sy = qdiv(Q_HEIGHT, qsub(ty, by));
	cy = qmul(qnegate(by), sy);
}

static void
pset(q_t x, q_t y)
{
	lcd_setpixel(qtoi(qadd(cx, qmul(sx, x))), qtoi(qadd(cy, qmul(sy, y))), 0xffff);
}

#define Q_TWO 0x00020000

/*
function (i, x, y, a, b) {
	var c = 2.0 - 2.0 * a;
	var w = a * x + c * x * x / (1 + x * x);
	while (i-- !== 0)
	{
		that.pset(x, y);
		var z = x;
		x = b * y + w;
		w = a * x + c * x * x / (1 + x * x);
		y = w - z;
	}
}
*/
void mira(int i, q_t x, q_t y, q_t a, q_t b)
{
	q_t c = qsub(Q_TWO, qmul(Q_TWO, a));
	q_t x2 = qmul(x, x);
	q_t w = qadd(qmul(a, x), qdiv(qmul(c, x2), qadd(qinfo.one, x2)));
	while (i--)
	{
		pset(x, y);
		q_t z = x;
		x = qadd(qmul(b, y), w);
		x2 = qmul(x, x);
		w = qadd(qmul(a, x), qdiv(qmul(c, x2), qadd(qinfo.one, x2)));
		y = qsub(w, z);
	}
}
