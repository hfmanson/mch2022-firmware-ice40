/*
 * fw_app.c
 *
 * Copyright (C) 2022 Sylvain Munaut
 * SPDX-License-Identifier: MIT
 */

#include <alloca.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "config.h"
#include "console.h"
#include "lcd.h"
#include "led.h"
#include "memtest.h"
#include "mini-printf.h"
#include "misc.h"
#include "msg.h"
#include "qpi.h"
#include "mira.h"

// ---------------------------------------------------------------------------
// Test routines
// ---------------------------------------------------------------------------

#define SOC_RESP_OK		0x00000000
#define SOC_RESP_ERR_FAIL	0xffbadbad
#define SOC_RESP_ERR_INVAL	0xdeaddead
#define SOC_RESP_ERR(n)		(0xff000000 | (n))


static uint32_t
h_cmd_lcd_init_test(uint32_t param)
{
	int f;

	/* Check LCD is assigned to FPGA */
	gpio_set_dir(MISC_GPIO_LCD_MODE, false);
	if (!gpio_get_val(MISC_GPIO_LCD_MODE))
		return SOC_RESP_ERR(1);

	/* Force LCD to reset */
	lcd_keep_reset();

	/* Check there is no fmark */
	f = measure_framerate();

	if (f != 0)
		return SOC_RESP_ERR(4);

	/* Run init sequence */
	lcd_init();

	/* Fill LCD with pattern */
	lcd_fill(LCD_PATTERN_BARS);

	/* Check reported frame rate is within bound */
	f = measure_framerate();

	if (f == 0)
		return SOC_RESP_ERR(5);
	else if (f < 65)
		return SOC_RESP_ERR(6);
	else if (f > 85)
		return SOC_RESP_ERR(7);

	return SOC_RESP_OK;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
void show_plasma(int t);

void main()
{
	uint32_t t = 0;

	/* Init console IO */
	console_init();

	const uint8_t tx_buf[1] = { 0x35 };
	spi_xfer(tx_buf, 1, 0, NULL, 0);

	volatile uint32_t *fb32 = (volatile uint32_t *)PSRAM_BASE;
	puts("MCH2022 iCE40 initialized\n");

	printf("h_cmd_lcd_init_test: %08x\n", h_cmd_lcd_init_test(0));
	
	/* Main loop */
	while (1) {
		/* Debug interactive console */
		int cmd = getchar_nowait();
		switch (cmd) {
		case -1:
			break;

		case 'b':
			printf("Black LCD\n");
			lcd_fill(LCD_PATTERN_BLACK);
			window(W_LX, W_RX, W_TY, W_BY);
			mira(10000, M_X, M_Y, M_A, M_B);
			break;
		case 'l':
			printf("%08x\n", h_cmd_lcd_init_test(0));
			break;
		case 'm':
			printf("Blit\n");
			lcd_blit_from_psram(0);
			break;
		case 'w':
			printf("write psram\n");
			for (int i = 0; i < 320*240; i++)
				fb32[i] = 0x00000000;
			break;
		case 'r':
			printf("read psram: %08x\n", fb32[0]);
			break;
		case 'p':
			printf("psram_set_pixel\n");
			for (int i = 0; i < 320*240/2; i++)
				fb32[i] = 0x001F001F;
			for (int i = 30; i < 200/2; i++)
				fb32[i + 480] = 0xf800f800;
			psram_set_pixel(30, 60, 0xFFFF);
			break;
		case '1':
			printf("plasma\n");
/*			
			while (1) {
				// 1. Wacht op het begin van de verticale terugslag (V-Sync)
				wait_vsync();

				// 2. Start de hardware-blitter en wacht tot hij klaar is (~5,5 ms)
				lcd_blit_from_psram(0);

				// 3. De blitter is nu klaar (blit_active == 0) en de PSRAM is weer vrij.
				//    Gebruik de overige ~9 tot 11 ms om het volgende frame in PSRAM te tekenen!
				show_plasma(t++);
			}
*/
				wait_vsync();

				// 2. Start de hardware-blitter en wacht tot hij klaar is (~5,5 ms)
				lcd_blit_from_psram(0);

				// 3. De blitter is nu klaar (blit_active == 0) en de PSRAM is weer vrij.
				//    Gebruik de overige ~9 tot 11 ms om het volgende frame in PSRAM te tekenen!
				uint32_t before = cycles_now();
				show_plasma(t++);
				uint32_t after = cycles_now();
				printf("cycles elapsed: %d\n", after - before);
			break;
		}
	}
}
