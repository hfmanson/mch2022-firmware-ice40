`default_nettype none

module ram_test (
	input  wire        clk,
  input  wire        rst,
  input  wire        pw_end,
	output wire        req_valid,
	input  wire        req_ready,

	// "fread" response stream/fifo interface
	input  wire  [7:0] resp_data,
	input  wire        resp_valid,
  output wire [31:0] req_offset,
	output wire        red,
	output wire        ram_ready,

  // SPRAM
  input  wire [13:0] address,
  output wire [15:0] dataout  
);
  wire [15:0] datain;
  wire [13:0] load_address;
  wire        wren;
  wire        chipselect;

  reg [31:0] req_offset_reg;
	reg [15:0] counter;
	reg [15:0] counter;
	reg        req_valid_reg;	
  reg [31:0] buffercontent; // Start with an invalid address
  reg        first;
	reg  [7:0] resp_data_reg;

	assign req_valid = req_valid_reg;
  assign req_offset = req_offset_reg;
	assign ram_ready = counter == 16'h2000;
	assign red = ram_ready; // ram loading

  assign load_address = { 1'b0, counter[12:0]};
  assign wren = (counter < 16'h2000);
  assign chipselect = 1'b1;
  assign datain = { 8'b00000000, resp_data_reg };
  
  SB_SPRAM256KA rambank (
    .DATAIN(datain),
    .ADDRESS(ram_ready ? address: load_address),
    .MASKWREN(4'b1111),
    .WREN(wren),
    .CHIPSELECT(chipselect),
    .CLOCK(clk),
    .STANDBY(1'b0),
    .SLEEP(1'b0),
    .POWEROFF(1'b1),
    .DATAOUT(dataout)
  );

	always @(posedge clk) begin
    if (rst) begin
      req_offset_reg <= 32'd0;
      counter <= 16'd0;
      req_valid_reg <= 1'b0;
      buffercontent <= 32'hFFFFFFFF;
      first <= 1'b1;
    end
		else if (counter < 16'h2000) begin
			// load ram from ESP
			if (req_valid_reg) req_valid_reg <= req_valid_reg & ~req_ready;
      else if (buffercontent != req_offset_reg) begin
        buffercontent <= req_offset_reg;
        req_valid_reg <= 1;
        first <= 1'b1;
      end
      else begin
        if (resp_valid) begin
          resp_data_reg <= resp_data;
          counter <= counter + 1;
          first <= 1'b0;
        end
        if (pw_end & ~first) req_offset_reg <= req_offset_reg + 32'h800;
      end
		end
	end
endmodule
