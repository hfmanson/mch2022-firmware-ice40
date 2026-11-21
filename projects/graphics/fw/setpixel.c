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

struct wb_line_accel {
    uint32_t csr;   // Bit 31: busy (read), Write 1: start line
    uint32_t p0;    // (y0 << 16) | (x0 & 0xFFFF)
    uint32_t p1;    // (y1 << 16) | (x1 & 0xFFFF)
    uint32_t color; // 16-bit RGB565 color
} __attribute__((packed, aligned(4)));

static volatile struct wb_line_accel * const line_regs = (void *)(PSRAM_BASE + 0x3FFF0);

void draw_line(int x0, int y0, int x1, int y1, uint16_t color)
{
    while (line_regs->csr & (1 << 31));

    line_regs->p0    = ((uint32_t)(uint16_t)y0 << 16) | (uint16_t)x0;
    line_regs->p1    = ((uint32_t)(uint16_t)y1 << 16) | (uint16_t)x1;
    line_regs->color = color;
    line_regs->csr   = 1;

    while (line_regs->csr & (1 << 31));
}
