module button_test (
	input  wire        clk,

	input  wire        uart_ack,
	input  wire        uart_ready,
	output wire        uart_valid,
	output wire  [7:0] uart_data,

  input wire  [15:0] btn_rpt_state,
  input wire  [15:0] btn_rpt_change,
  input wire         btn_rpt_stb,

  output wire        red,
  output wire        green,
  output wire        blue
);
  reg   [7:0] uart_valid_reg;
  assign uart_valid = uart_valid_reg;

  always @(posedge clk)
  begin
    if (uart_ready) begin
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
          11'bzzzzz1zzzzz: green     <= btn_rpt_state[5] ? 1'b1 : 1'b0;
          11'bzzzz1zzzzzz: red       <= btn_rpt_state[6] ? 1'b1 : 1'b0;
          11'bzzz1zzzzzzz: blue      <= btn_rpt_state[7] ? 1'b1 : 1'b0;
          11'bzz1zzzzzzzz: uart_data <= btn_rpt_state[8] ? "S" : "s";
          11'bz1zzzzzzzzz: uart_data <= btn_rpt_state[9] ? "A" : "a";
          11'b1zzzzzzzzzz: uart_data <= btn_rpt_state[10] ? "B" : "b";
          default: uart_data <= 8'hxx;
        endcase
      end
      // Valid
      uart_valid_reg <= (uart_valid_reg & ~uart_ack) | (btn_rpt_stb & (|btn_rpt_change[10:8] | |btn_rpt_change[4:0]));
    end
  end
endmodule
