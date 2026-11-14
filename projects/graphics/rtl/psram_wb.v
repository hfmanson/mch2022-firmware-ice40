`default_nettype none
module psram_wb #(
parameter integer ADDR_WIDTH = 16
)(
	// Memory interface (naar qpi_memctrl)
	output wire [31:0] mi_addr,
	output wire [ 6:0] mi_len,
	output wire        mi_rw,
	output reg         mi_valid,
	input  wire        mi_ready,
	output wire [31:0] mi_wdata,
	input  wire        mi_wack,
	input  wire [31:0] mi_rdata,
	input  wire        mi_rstb,
	// Wishbone interface
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
	assign mi_addr  = { {(32-ADDR_WIDTH){1'b0}}, wb_addr };
	assign mi_len   = 7'd0;
	assign mi_rw    = ~wb_we;     // 1 = Read, 0 = Write
	assign mi_wdata = wb_wdata;
	always @(posedge clk) begin
		if (rst) begin
			busy          <= 1'b0;
			mi_valid      <= 1'b0;
			wb_ack        <= 1'b0;
			rdata_latched <= 32'd0;
		end else begin
			// Standaard: ack maar 1 kloktik hoog
			wb_ack <= 1'b0;
			if (!wb_cyc) begin
				busy     <= 1'b0;
				mi_valid <= 1'b0;
			end else if (!busy && !wb_ack) begin
				// Start nieuwe PSRAM-actie
				busy     <= 1'b1;
				mi_valid <= 1'b1;
			end else if (busy) begin
				// Zodra qpi_memctrl het commando pakt, valid weer laag
				if (mi_valid && mi_ready)
					mi_valid <= 1'b0;
				// Klaar bij schrijf-ack (mi_wack) of lees-data (mi_rstb)
				if ((wb_we && mi_wack) || (!wb_we && mi_rstb)) begin
					busy          <= 1'b0;
					rdata_latched <= mi_rdata;
					wb_ack        <= 1'b1;
				end
			end
		end
	end
	assign wb_rdata = wb_ack ? rdata_latched : 32'd0;
endmodule
