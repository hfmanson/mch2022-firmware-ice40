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
	// read 6144 bytes from ESP file with FID 0xDABBAD00

	reg [15:0] mem [0:16'h17FF]; // 6144x16 bit memory
  reg [31:0] req_offset_reg;
	reg [15:0] counter;
  reg  [7:0] low_data;
	reg        req_valid_reg;	
  reg [31:0] buffercontent; // Start with an invalid address
  reg        first;
  reg [15:0] dataout_reg;

	assign req_valid = req_valid_reg;
  assign req_offset = req_offset_reg;
  assign dataout = dataout_reg;
	assign ram_ready = counter == 16'h3000;
	assign red = req_valid; // ram loading
	always @(posedge clk) begin
    if (rst) begin
      req_offset_reg <= 32'd0;
      counter <= 16'd0;
      req_valid_reg <= 1'b0;
      buffercontent <= 32'hFFFFFFFF;
      first <= 1'b1;
    end
		else if (counter < 16'h3000) begin
			// load ram from ESP
			if (req_valid_reg) req_valid_reg <= req_valid_reg & ~req_ready;
      else if (buffercontent != req_offset_reg) begin
        buffercontent <= req_offset_reg;
        req_valid_reg <= 1;
        first <= 1'b1;
      end
      else begin
        if (resp_valid) begin                
          // store next byte
          if (counter[0]) mem[counter[15:1]] <= { resp_data, low_data }; else low_data <= resp_data;
          counter <= counter + 1;
          first <= 1'b0;
        end
        if (pw_end & ~first) req_offset_reg <= req_offset_reg + 32'h800;
      end
		end
    else dataout_reg <= mem[address];
	end
endmodule
