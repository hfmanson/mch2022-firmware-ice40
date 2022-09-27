`default_nettype none

module ram_test (
	input  wire clk,
  input  wire rst,
  input  wire pw_end,
	output wire        req_valid,
	input  wire        req_ready,

	// "fread" response stream/fifo interface
	input  wire  [7:0] resp_data,
	input  wire        resp_valid,
	input  wire        uart_ack,
  output wire  [31:0] req_offset,
	output wire        red,
	output wire        ram_ready,
	output wire        uart_valid,
	output wire  [7:0] uart_data
);

	// read 64 bytes from ESP file with FID 0xDABBAD00

	reg [15:0] mem [0:16'h17FF]; // 6144x16 bit memory
  reg [31:0] req_offset_reg;
	reg [15:0] counter;
  reg [7:0] low_data;
	reg req_valid_reg;	
	reg uart_valid_reg;
  reg  [31:0] buffercontent; // Start with an invalid address
  reg first;
  wire [15:0] uart_addr;
	assign req_valid = req_valid_reg;
	assign uart_valid = uart_valid_reg;
  assign req_offset = req_offset_reg;
	assign ram_ready = counter == 16'h6000 & ~uart_valid_reg;
	assign red = req_valid; // ram loading
  assign uart_addr = (counter - 16'h3000) >> 1;
	always @(posedge clk) begin
    if (rst) begin
      req_offset_reg <= 32'd0;
      counter <= 16'd0;
      req_valid_reg <= 1'b0;
      uart_valid_reg <= 1'b0;
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
		else if (counter < 16'h6000) begin
			if (~uart_valid_reg) begin
				uart_data <= (counter[0] ? mem[uart_addr][15:8] : mem[uart_addr][7:0]);
				//uart_data <= mem[uart_addr][15:8];
				counter <= counter + 1;
				uart_valid_reg <= 1'b1;
			end
			else uart_valid_reg <= uart_valid_reg & ~uart_ack;
		end
		else uart_valid_reg <= uart_valid_reg & ~uart_ack;
	end
endmodule
