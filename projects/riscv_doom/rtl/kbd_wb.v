/*
 * kbd_wb.v
 *
 * vim: ts=4 sw=4
 *
 * Copyright (C) 2019-2020  Sylvain Munaut <tnt@246tNt.com>
 * Adapted for BLE keyboard Henri Manson <hfmanson@gmail.com>
 * SPDX-License-Identifier: CERN-OHL-P-2.0
 */

`default_nettype none

module kbd_wb #(
	parameter integer DW = 32
)(
	// Bus interface
	input  wire [1:0]    wb_addr,
	output wire [DW-1:0] wb_rdata,
	input  wire [DW-1:0] wb_wdata,
	input  wire          wb_we,
	input  wire          wb_cyc,
	output wire          wb_ack,

	// keyboard
	input  wire [ 7:0] kbd_rx_data,
	input  wire        kbd_rx_stb,

	// Clock / Reset
	input  wire clk,
	input  wire rst
);

	// Signals
	// -------

	// RX fifo
	wire [ 7:0] urf_wdata;
	wire        urf_wren;
	wire        urf_full;
	wire [ 7:0] urf_rdata;
	wire        urf_rden;
	wire        urf_empty;

	reg         urf_overflow;
	wire        urf_overflow_clr;

	// Bus IF
	wire          ub_rdata_rst;
	reg  [DW-1:0] ub_rdata;
	reg           ub_rd_data;
	reg           ub_rd_ctrl;
	reg           ub_ack;

	// RX FIFO
	// -------

	fifo_sync_ram #(
		.DEPTH(16),
		.WIDTH(8)
	) uart_rx_fifo_I (
		.wr_data(urf_wdata),
		.wr_ena(urf_wren),
		.wr_full(urf_full),
		.rd_data(urf_rdata),
		.rd_ena(urf_rden),
		.rd_empty(urf_empty),
		.clk(clk),
		.rst(rst)
	);

	// RX glue
	assign urf_wdata = kbd_rx_data;
	assign urf_wren  = kbd_rx_stb & ~urf_full;

	// Overflow
	always @(posedge clk or posedge rst)
		if (rst)
			urf_overflow <= 1'b0;
		else
			urf_overflow <= (urf_overflow & ~urf_overflow_clr) | (kbd_rx_stb & urf_full);


	// Bus interface
	// -------------

	always @(posedge clk)
		if (ub_ack) begin
			ub_rd_data <= 1'b0;
		end else begin
			ub_rd_data <= ~wb_we & wb_cyc & (wb_addr == 2'b00);
		end

	always @(posedge clk)
		if (ub_ack) begin
			ub_rd_data <= 1'b0;
		end else begin
			ub_rd_data <= ~wb_we & wb_cyc & (wb_addr == 2'b00);
		end

	always @(posedge clk)
		if (ub_ack)
			ub_ack <= 1'b0;
		else
			ub_ack <= wb_cyc & (~wb_we | (wb_addr == 2'b01) );

	assign ub_rdata_rst = ub_ack | wb_we | ~wb_cyc;

	always @(posedge clk)
		if (ub_rdata_rst)
			ub_rdata <= { DW{1'b0} };
		else
			ub_rdata <= { urf_empty, { (DW-9){1'b0} }, urf_rdata };

	assign urf_rden  = ub_rd_data & ~ub_rdata[DW-1];
	assign urf_overflow_clr = ub_rd_ctrl & ub_rdata[DW-2];

	assign wb_rdata = ub_rdata;
	assign wb_ack = ub_ack;

endmodule // uart_wb
