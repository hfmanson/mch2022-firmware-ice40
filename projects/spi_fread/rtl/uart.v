`default_nettype none

module uart_test (
	input  wire clk,
  input  wire rst,

	input  wire        uart_ack,
	output wire        uart_ready,
	output wire        uart_valid,
	output wire  [7:0] uart_data,

  output wire [13:0] address,
  input  wire [15:0] data

);
	reg [15:0] counter;
	reg uart_valid_reg;
  wire [15:0] uart_addr;
  reg  [7:0] uart_data_reg;

	assign uart_valid = uart_valid_reg;
	assign uart_data = uart_data_reg;
	assign uart_ready = counter == 16'h2000 & ~uart_valid_reg;
  assign uart_addr = (counter - 16'h2000) >> 1;

  assign address = { 1'b0, counter[12:0]};
  
	always @(posedge clk) begin
    if (rst) begin
      counter <= 16'd0;
      uart_valid_reg <= 1'b0;
    end
		else if (counter < 16'h2000) begin
			if (~uart_valid_reg) begin
				uart_data_reg <= data[7:0];
				counter <= counter + 1;
				uart_valid_reg <= 1'b1;
			end
			else uart_valid_reg <= uart_valid_reg & ~uart_ack;
		end
		else uart_valid_reg <= uart_valid_reg & ~uart_ack;
	end
endmodule
