`default_nettype none
module psram_wb #(
	parameter integer ADDR_WIDTH = 16,
	parameter integer PORTRAIT   = 0
)(
	output wire [31:0] mi_addr,
	output wire [ 6:0] mi_len,
	output wire        mi_rw,
	output reg         mi_valid,
	input  wire        mi_ready,
	output wire [31:0] mi_wdata,
	input  wire        mi_wack,
	input  wire [31:0] mi_rdata,
	input  wire        mi_rstb,
	input  wire [ADDR_WIDTH-1:0] wb_addr,
	input  wire [31:0]           wb_wdata,
	output wire [31:0]           wb_rdata,
	input  wire                  wb_we,
	input  wire                  wb_cyc,
	output reg                   wb_ack,
	input  wire                  clk,
	input  wire                  rst
);
	reg        busy;
	reg [31:0] rdata_latched;
	reg [2:0]  l_state;
	reg signed [11:0] x0, y0, x1, y1, dx, dy, err;
	reg        sx, sy;
	reg [15:0] l_color;
	reg [31:0] rmw_word;
	wire is_reg      = (wb_addr[ADDR_WIDTH-1:2] == {(ADDR_WIDTH-2){1'b1}});
	wire line_active = (l_state != 3'd0);
	// Pixel-index berekening voor 320x240 (landschap) of 240x320 (portret)
	wire [16:0] pix_idx = (PORTRAIT != 0)
	? ((y0[8:0] * 17'd240) + {8'd0, x0[8:0]})
	: ((y0[8:0] * 17'd320) + {8'd0, x0[8:0]});
	wire [31:0] line_wdata = pix_idx[0]
	? {rmw_word[31:16], l_color}
	: {l_color, rmw_word[15:0]};
	wire signed [12:0] e2 = {err, 1'b0};
	assign mi_addr  = line_active ? {16'd0, pix_idx[16:1]} : { {(32-ADDR_WIDTH){1'b0}}, wb_addr };
	assign mi_len   = 7'd0;
	assign mi_rw    = line_active ? (l_state == 3'd2) : ~wb_we;
	assign mi_wdata = line_active ? line_wdata : wb_wdata;
	always @(posedge clk) begin
		if (rst) begin
			busy <= 1'b0; mi_valid <= 1'b0; wb_ack <= 1'b0;
			rdata_latched <= 32'd0; l_state <= 3'd0;
		end else begin
			wb_ack <= 1'b0;
			case (l_state)
				3'd1: begin // INIT BRESENHAM
					dx <= (x1 >= x0) ? (x1 - x0) : (x0 - x1);
					dy <= (y1 >= y0) ? (y0 - y1) : (y1 - y0);
					sx <= (x0 < x1);
					sy <= (y0 < y1);
					l_state <= 3'd2;
				end
				3'd2: begin // START & READ 32-BIT WORD
					if (!mi_valid && !busy) begin
						err <= dx + dy;
						mi_valid <= 1'b1; busy <= 1'b1;
					end
					if (mi_valid && mi_ready) mi_valid <= 1'b0;
					if (mi_rstb) begin
						rmw_word <= mi_rdata;
						mi_valid <= 1'b1;
						l_state  <= 3'd3;
					end
				end
				3'd3: begin // WRITE BACK PIXEL
					if (mi_valid && mi_ready) mi_valid <= 1'b0;
					if (mi_wack) begin
						busy    <= 1'b0;
						l_state <= 3'd4;
					end
				end
				3'd4: begin // STEP BRESENHAM
					if (x0 == x1 && y0 == y1) begin
						l_state <= 3'd0;
					end else begin
						err <= err + ((e2 >= dy) ? dy : 12'sd0) + ((e2 <= dx) ? dx : 12'sd0);
						if (e2 >= dy) x0 <= sx ? (x0 + 12'sd1) : (x0 - 12'sd1);
						if (e2 <= dx) y0 <= sy ? (y0 + 12'sd1) : (y0 - 12'sd1);
						mi_valid <= 1'b1; busy <= 1'b1;
						l_state  <= 3'd2;
					end
				end
				default: ;
			endcase
			if (!wb_cyc) begin
				if (!line_active) begin busy <= 1'b0; mi_valid <= 1'b0; end
			end else if (is_reg && !wb_ack) begin
				if (wb_we) begin
					case (wb_addr[1:0])
						2'd0: if (wb_wdata && !line_active) l_state <= 3'd1;
						2'd1: begin x0 <= {1'b0, wb_wdata[10:0]}; y0 <= {1'b0, wb_wdata[26:16]}; end
						2'd2: begin x1 <= {1'b0, wb_wdata[10:0]}; y1 <= {1'b0, wb_wdata[26:16]}; end
						2'd3: l_color <= wb_wdata[15:0];
					endcase
				end
				rdata_latched <= {line_active, 31'd0};
				wb_ack <= 1'b1;
			end else if (!is_reg && !line_active && !busy && !wb_ack) begin
				busy <= 1'b1; mi_valid <= 1'b1;
			end else if (busy && !line_active) begin
				if (mi_valid && mi_ready) mi_valid <= 1'b0;
				if ((wb_we && mi_wack) || (!wb_we && mi_rstb)) begin
					busy <= 1'b0; rdata_latched <= mi_rdata; wb_ack <= 1'b1;
				end
			end
		end
	end
	assign wb_rdata = wb_ack ? rdata_latched : 32'd0;
endmodule
