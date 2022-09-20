`default_nettype none

module ram_test (
	input  wire clk,
	output wire        req_valid,
	input  wire        req_ready,

	// "fread" response stream/fifo interface
	input  wire  [7:0] resp_data,
	input  wire        resp_valid,
	input  wire        uart_ack,
	output wire        red,
	output wire        ram_ready,
	output wire        uart_valid,
	output wire  [7:0] uart_data
);

	// read 64 bytes from ESP file with FID 0xDABBAD00

	reg [7:0] mem [0:2047]; // 64x8 bit memory
	reg [15:0] counter = 16'h0;
	reg req_valid_reg = 1'b1;	
	reg uart_valid_reg = 1'b0;

	assign req_valid = req_valid_reg;
	assign uart_valid = uart_valid_reg;
	assign ram_ready = counter == 16'h800 & ~uart_valid_reg;
	assign red = counter < 16'h400; // ram loading

	always @(posedge clk) begin
		if (red) begin
			// load ram from ESP
			if (req_valid_reg) req_valid_reg <= req_valid_reg & ~req_ready;
			else begin
				if (resp_valid) begin                
					// store next byte
					mem[counter] <= resp_data;
					counter <= counter + 1;
				end
			end
		end
		else if (counter < 16'h800) begin
			if (~uart_valid_reg) begin
				uart_data <= mem[counter[9:0]];
				counter <= counter + 1;
				uart_valid_reg <= 1'b1;
			end
			else uart_valid_reg <= uart_valid_reg & ~uart_ack;
		end
		else uart_valid_reg <= uart_valid_reg & ~uart_ack;
	end
endmodule
