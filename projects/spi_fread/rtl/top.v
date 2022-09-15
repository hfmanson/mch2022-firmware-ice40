/*
 * top.v
 *
 * vim: ts=4 sw=4
 *
 * Top-level module for MCH2022 SPI skeleton
 *
 */

`default_nettype none

module top (
	// UART (to RP2040)
	output wire       uart_tx,

	// SPI Slave (to ESP32)
	input  wire       spi_mosi,
	output wire       spi_miso,
	input  wire       spi_clk,
	input  wire       spi_cs_n,

	// IRQ
	output wire       irq_n,

	// RGB Leds
	output wire [2:0] rgb,

	// Clock
	input  wire       clk_in
);

	// Signals
	// -------

	// Button reports
	wire [15:0] btn_rpt_state;
	wire [15:0] btn_rpt_change;
	wire        btn_rpt_stb;

	// UART sender
	reg   [7:0] uart_data;
	reg         uart_valid;
	wire        uart_ack;

	// Clock / Reset
	wire        clk;
	wire        rst;



	// SPI Flexible
	// ------------

	// This example uses the individual SPI core components
	// which is more flexible and allows multiple connections to the
	// protocol wrapper. Here there is a command decoder for the button
	// state reports and the wishbone interface.


	// Signals
		// Raw core IF
	wire [7:0] usr_mosi_data;
	wire       usr_mosi_stb;

	wire [7:0] usr_miso_data;
	wire       usr_miso_ack;

	wire       csn_state;
	wire       csn_rise;
	wire       csn_fall;

		// Protocol IF
	wire [7:0] pw_wdata;
	wire       pw_wcmd;
	wire       pw_wstb;

	wire       pw_end;

	wire       pw_req;
	wire       pw_gnt;

	wire [7:0] pw_rdata;
	wire       pw_rstb;

	wire [3:0] pw_irq;
	wire       irq;

	// Device Core
	spi_dev_core core_I (
		.spi_miso      (spi_miso),
		.spi_mosi      (spi_mosi),
		.spi_clk       (spi_clk),
		.spi_cs_n      (spi_cs_n),
		.usr_mosi_data (usr_mosi_data),
		.usr_mosi_stb  (usr_mosi_stb),
		.usr_miso_data (usr_miso_data),
		.usr_miso_ack  (usr_miso_ack),
		.csn_state     (csn_state),
		.csn_rise      (csn_rise),
		.csn_fall      (csn_fall),
		.clk           (clk),
		.rst           (rst)
	);

	// Protocol wrapper
	spi_dev_proto proto_I (
		.usr_mosi_data (usr_mosi_data),
		.usr_mosi_stb  (usr_mosi_stb),
		.usr_miso_data (usr_miso_data),
		.usr_miso_ack  (usr_miso_ack),
		.csn_state     (csn_state),
		.csn_rise      (csn_rise),
		.csn_fall      (csn_fall),
		.pw_wdata      (pw_wdata),
		.pw_wcmd       (pw_wcmd),
		.pw_wstb       (pw_wstb),
		.pw_end        (pw_end),
		.pw_req        (pw_req),
		.pw_gnt        (pw_gnt),
		.pw_rdata      (pw_rdata),
		.pw_rstb       (pw_rstb),
		.pw_irq        (pw_irq),
		.irq           (irq),
		.clk           (clk),
		.rst           (rst)
	);

	// Command decoder for the F4 command
	// (button state reports from ESP32)
	spi_dev_scmd #(
		.CMD_BYTE (8'hf4),
		.CMD_LEN  (4)
	) scmd_f4_I (
		.pw_wdata (pw_wdata),
		.pw_wcmd  (pw_wcmd),
		.pw_wstb  (pw_wstb),
		.pw_end   (pw_end),
		.cmd_data ({btn_rpt_state, btn_rpt_change}),
		.cmd_stb  (btn_rpt_stb),
		.clk      (clk),
		.rst      (rst)
	);

	assign pw_irq[3:1] = 3'b000;
	assign irq_n = irq ? 1'b0 : 1'bz;

	// LEDS
	wire red, green, blue;
	SB_RGBA_DRV #(
		.CURRENT_MODE("0b1"),       // half current
		.RGB0_CURRENT("0b000011"),  // 4 mA
		.RGB1_CURRENT("0b000011"),  // 4 mA
		.RGB2_CURRENT("0b000011")   // 4 mA
	) RGBA_DRIVER (
		.CURREN(1'b1),
		.RGBLEDEN(1'b1),
		.RGB1PWM(red),
		.RGB2PWM(blue),
		.RGB0PWM(green),
		.RGB0(rgb[0]),
		.RGB1(rgb[1]),
		.RGB2(rgb[2])
	);

	// read 64 bytes from ESP file with FID 0xDABBAD00

    reg [7:0] mem [0:63]; // 64x8 bit memory

	reg request = 1; // immediately request
	reg [7:0] counter = 8'h0;
	wire file_request_ready; // true when request processed
	wire [7:0] file_data;  // one byte of memory
	wire       file_data_wstrb; // asserted when a new byte is available

	spi_dev_fread #(
		.INTERFACE("STREAM")
	) _fread (
		.clk (clk),
		.rst (rst),

		// SPI interface
		.pw_wdata     (pw_wdata),
		.pw_wcmd      (pw_wcmd),
		.pw_wstb      (pw_wstb),
		.pw_end       (pw_end),
		.pw_req       (pw_req),
		.pw_gnt       (pw_gnt),
		.pw_rdata     (pw_rdata),
		.pw_rstb      (pw_rstb),
		.pw_irq       (pw_irq[0]),

		// Read request interface
		.req_file_id  (32'hDABBAD00),
		.req_offset   (32'h0),
		.req_len      (10'd63), // One less than the actual requested length!

		.req_valid    (request),
		.req_ready    (file_request_ready),

		// Stream reply interface
		.resp_data    (file_data),
		.resp_valid   (file_data_wstrb)
	);

	always @(posedge clk)
	begin
		if (counter < 64) begin
			red <= 1'b1;
			// load ram from ESP
			if (request) request <= request & ~file_request_ready;
			else begin
				if (file_data_wstrb) begin                
					// store next byte
					mem[counter] <= file_data;
					counter <= counter + 1;
				end
			end
		end
		else if (counter < 128) begin
			red <= 1'b0;
			if (~uart_valid) begin
				uart_data <= mem[counter[5:0]];
				counter <= counter + 1;
				uart_valid <= 1'b1;
			end
			else uart_valid <= ~uart_ack;
		end
		else
		begin
			// UART
			// ----
			// Send key presses to UART for debug
			if (btn_rpt_stb) begin
				casez (btn_rpt_change[10:0])
					11'bzzzzzzzzzz1: uart_data <= btn_rpt_state[0] ? "D" : "d";
					11'bzzzzzzzzz1z: uart_data <= btn_rpt_state[1] ? "U" : "u";
					11'bzzzzzzzz1zz: uart_data <= btn_rpt_state[2] ? "L" : "l";
					11'bzzzzzzz1zzz: uart_data <= btn_rpt_state[3] ? "R" : "r";
					11'bzzzzzz1zzzz: uart_data <= btn_rpt_state[4] ? "F" : "f";
					11'bzzzzz1zzzzz: green <= btn_rpt_state[5] ? 1'b1 : 1'b0;
					11'bzzzz1zzzzzz: red <= btn_rpt_state[6] ? 1'b1 : 1'b0;
					11'bzzz1zzzzzzz: blue <= btn_rpt_state[7] ? 1'b1 : 1'b0;
					11'bzz1zzzzzzzz: uart_data <= btn_rpt_state[8] ? "S" : "s";
					11'bz1zzzzzzzzz: uart_data <= btn_rpt_state[9] ? "A" : "a";
					11'b1zzzzzzzzzz: uart_data <= btn_rpt_state[10] ? "B" : "b";
					default: uart_data <= 8'hxx;
				endcase
			end
			// Valid
			uart_valid <= (uart_valid & ~uart_ack) | (btn_rpt_stb & (|btn_rpt_change[10:8] | |btn_rpt_change[4:0]));
		end
	end

	// Core
	uart_tx #(
		.DIV_WIDTH(8)
	) uart_I (
		.tx    (uart_tx),
		.data  (uart_data),
		.valid (uart_valid),
		.ack   (uart_ack),
		.div   (8'd28), // 30M / (28 + 2) = 1 MBaud
		.clk   (clk),
		.rst   (rst)
	);

	// Clock/Reset Generation
	// ----------------------

	sysmgr sysmgr_I (
		.clk_in (clk_in),
		.clk    (clk),
		.rst    (rst)
	);

endmodule // top
