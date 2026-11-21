/*
 * setpixel.h
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <stdint.h>

extern volatile uint32_t *fb32;

void lcd_cls(void);
void psram_set_pixel(int x, int y, uint16_t color);
void draw_line(int x0, int y0, int x1, int y1, uint16_t color);
