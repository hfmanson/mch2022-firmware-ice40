`default_nettype none
module psram_blitter (
	// Memory interface (naar qpi_memctrl)
	output wire [31:0] mi_addr,
	output wire [ 6:0] mi_len,
	output wire        mi_rw,
	output wire        mi_valid,
	input  wire        mi_ready,
	output wire [31:0] mi_wdata,
	input  wire        mi_wack,
	input  wire [31:0] mi_rdata,
	input  wire        mi_rstb,
	// LCD PHY interface
	output reg  [ 7:0] phy_data,
	output reg         phy_rs,
	output reg         phy_valid,
	input  wire        phy_ready,
	output wire        blit_active,
	// Wishbone interface
	input  wire [31:0] wb_wdata,
	output wire [31:0] wb_rdata,
	input  wire [ 1:0] wb_addr,
	input  wire        wb_we,
	input  wire        wb_cyc,
	output reg         wb_ack,
	input  wire        clk,
	input  wire        rst
);
	reg [2:0]  state;
	reg        auto_clear;
	reg [31:0] start_addr, cur_addr, block_addr;
	reg [9:0]  burst_cnt;
	reg [6:0]  xfer_idx;
	reg [5:0]  rd_idx;
	reg [1:0]  byte_sel;
	reg        req_sent;
	wire [31:0] buf_rdata;
	wire [5:0] next_rd_idx = (state == 3'd3 && phy_valid && phy_ready && byte_sel == 2'd3)
	? (rd_idx + 6'd1) : rd_idx;
	ram_sdp #(.AWIDTH(6), .DWIDTH(32)) buf_I (
		.wr_addr(xfer_idx[5:0]),
		.wr_data(mi_rdata),
		.wr_ena(mi_rstb && (state == 3'd2)),
		.rd_addr(next_rd_idx),
		.rd_data(buf_rdata),
		.rd_ena(1'b1),
		.clk(clk)
	);
	always @(*) begin
		if (state == 3'd1) begin
			phy_data = 8'h2C;
		end else begin
			case (byte_sel)
				2'd0: phy_data = buf_rdata[31:24];
				2'd1: phy_data = buf_rdata[23:16];
				2'd2: phy_data = buf_rdata[15:8];
				2'd3: phy_data = buf_rdata[7:0];
			endcase
		end
	end
	assign blit_active = (state != 3'd0);
	assign mi_addr     = (state == 3'd5) ? block_addr : cur_addr;
	assign mi_len      = 7'd63;
	assign mi_rw       = (state != 3'd5); // 1 = Read (blit), 0 = Write (clear)
	assign mi_valid    = ((state == 3'd2) || (state == 3'd5)) && !req_sent;
	assign mi_wdata    = 32'd0;
	always @(posedge clk)
		wb_ack <= wb_cyc & ~wb_ack;
	assign wb_rdata = wb_ack ? {blit_active, 31'd0} : 32'd0;
	always @(posedge clk) begin
		if (rst) begin
			state      <= 3'd0;
			auto_clear <= 1'b0;
			start_addr <= 32'd0;
			phy_valid  <= 1'b0;
			phy_rs     <= 1'b0;
			req_sent   <= 1'b0;
		end else begin
			if (wb_ack && wb_we && wb_addr == 2'd1)
			start_addr <= wb_wdata;
			case (state)
				3'd0: begin // IDLE
					phy_valid <= 1'b0;
					if (wb_ack && wb_we && wb_addr == 2'd0) begin
						cur_addr   <= start_addr;
						auto_clear <= wb_wdata[0];
						burst_cnt  <= 10'd600;
						state      <= 3'd1;
					end
				end
				3'd1: begin // SEND CMD 0x2C
					phy_rs    <= 1'b0;
					phy_valid <= 1'b1;
					if (phy_valid && phy_ready) begin
						phy_valid  <= 1'b0;
						block_addr <= cur_addr;
						xfer_idx   <= 7'd0;
						req_sent   <= 1'b0;
						state      <= 3'd2;
					end
				end
				3'd2: begin // READ 64 WORDS FROM PSRAM
					if (mi_valid && mi_ready) begin
						req_sent <= 1'b1;
						cur_addr <= cur_addr + 32'd64;
					end
					if (mi_rstb)
						xfer_idx <= xfer_idx + 7'd1;
					if (xfer_idx == 7'd64) begin
						rd_idx   <= 6'd0;
						byte_sel <= 2'd0;
						state    <= 3'd3;
					end
				end
				3'd3: begin // SEND 256 BYTES TO LCD
					phy_rs    <= 1'b1;
					phy_valid <= 1'b1;
					if (phy_valid && phy_ready) begin
						if (byte_sel == 2'd3) begin
							byte_sel <= 2'd0;
							if (rd_idx == 6'd63) begin
								phy_valid <= 1'b0;
								if (auto_clear) begin
									xfer_idx <= 7'd0;
									req_sent <= 1'b0;
									state    <= 3'd5;
								end else begin
									burst_cnt <= burst_cnt - 10'd1;
									state     <= (burst_cnt == 10'd1) ? 3'd0 : 3'd4;
								end
							end else begin
								rd_idx <= rd_idx + 6'd1;
							end
						end else begin
							byte_sel <= byte_sel + 2'd1;
						end
					end
				end
				3'd4: begin // PREP NEXT BURST
					block_addr <= cur_addr;
					xfer_idx   <= 7'd0;
					req_sent   <= 1'b0;
					state      <= 3'd2;
				end
				3'd5: begin // CLEAR JUST-BLITTED 64 WORDS TO 0
					if (mi_valid && mi_ready)
						req_sent <= 1'b1;
					if (mi_wack)
						xfer_idx <= xfer_idx + 7'd1;
					if (xfer_idx == 7'd64) begin
						burst_cnt <= burst_cnt - 10'd1;
						state     <= (burst_cnt == 10'd1) ? 3'd0 : 3'd4;
					end
				end
			endcase
		end
	end
endmodule
