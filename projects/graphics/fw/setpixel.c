#include <stdbool.h>
#include <stdint.h>

#include "config.h"

void psram_set_pixel(int x, int y, uint16_t color)
{
    volatile uint32_t *fb32 = (volatile uint32_t *)PSRAM_BASE;
    uint32_t pix_idx = (uint32_t)(y * 240 + x);
    uint32_t word_idx = pix_idx >> 1;
    uint32_t word = fb32[word_idx];

    if (pix_idx & 1)
        word = (word & 0xFFFF0000) | color;           // Lage 16 bits (oneven pixel)
    else
        word = (word & 0x0000FFFF) | ((uint32_t)color << 16); // Hoge 16 bits (even pixel)

    fb32[word_idx] = word;
}
