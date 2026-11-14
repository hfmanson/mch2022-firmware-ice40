/*
 * memtest.c
 *
 * Driver for the Memory tester core
 *
 * Copyright (C) 2022 Sylvain Munaut
 * SPDX-License-Identifier: MIT
 */

#include <stdbool.h>
#include <stdint.h>

#include "config.h"
#include "console.h"
#include "memtest.h"


struct wb_memtest {
	uint32_t cmd;
	uint32_t addr;
} __attribute__((packed,aligned(4)));

#define MT_CMD_DUAL		(1 << 18)
#define MT_CMD_CHECK_RST	(1 << 17)
#define MT_CMD_READ		(1 << 16)
#define MT_CMD_WRITE		(0 << 16)
#define MT_CMD_BUF_ADDR(x)	((x) << 8)
#define MT_CMD_LEN(x)		((x) - 1)


static volatile struct wb_memtest * const mt_regs = (void*)(MEMTEST_BASE);
static volatile uint32_t * const mt_mem = (void*)(MEMTEST_BASE + 0x400);


static void
mt_cmd_write(uint32_t ram_addr, uint32_t buf_addr, uint32_t len)
{
	mt_regs->addr = ram_addr;
	mt_regs->cmd =
		MT_CMD_WRITE |
		MT_CMD_BUF_ADDR(buf_addr) |
		MT_CMD_LEN(len);

	while (!(mt_regs->cmd & 1));
}

static void
mt_cmd_read(uint32_t ram_addr, uint32_t buf_addr, uint32_t len, bool check_reset)
{
	mt_regs->addr = ram_addr;
	mt_regs->cmd =
		(check_reset ? MT_CMD_CHECK_RST : 0) |
		MT_CMD_READ |
		MT_CMD_BUF_ADDR(buf_addr) |
		MT_CMD_LEN(len);

	while (!(mt_regs->cmd & 1));
}

bool
mt_run(uint32_t size, bool debug)
{
	volatile uint32_t *psram = (volatile uint32_t *)(MEMTEST_BASE);
	uint32_t words = size >> 2;
	int err = 0;

	/* Vul PSRAM direct */
	for (uint32_t i = 0; i < words; i++) {
		psram[i] =
			(((i << 2) + 0) << 24) |
			(((i << 2) + 1) << 16) |
			(((i << 2) + 2) <<  8) |
			(((i << 2) + 3) <<  0);
	}

	/* Lees terug en print de eerste 32 woorden */
	for (uint32_t i = 0; i < words; i++) {
		uint32_t expected =
			(((i << 2) + 0) << 24) |
			(((i << 2) + 1) << 16) |
			(((i << 2) + 2) <<  8) |
			(((i << 2) + 3) <<  0);
		uint32_t val = psram[i];

		if (i < 32)
			printf("%02x %08x\n", (unsigned int)i, (unsigned int)val);

		if (val != expected) {
			err++;
			if (err > 32)
				return false;
		}
	}

	return err == 0;
}
