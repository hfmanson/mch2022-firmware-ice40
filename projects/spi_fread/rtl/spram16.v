`default_nettype none

module spram_test (
  input  wire        clk,
  input  wire        rst,
  input  wire        pw_end,
  output wire        req_valid,
  input  wire        req_ready,

  // "fread" response stream/fifo interface
  input  wire  [7:0] resp_data,
  input  wire        resp_valid,
  output wire [31:0] req_offset,
  output wire        ram_ready,

  // SPRAM
  input  wire [15:0] datain,  
  input  wire [13:0] address,
  input  wire  [3:0] maskwren,
  input  wire        wren,
  input  wire        chipselect,
  input  wire        standby,
  input  wire        sleep,
  input  wire        poweroff,
  output wire [15:0] dataout  
);
  reg [31:0] req_offset_reg;
  reg [15:0] counter;
  reg [15:0] counter;
  reg        req_valid_reg;  
  reg [31:0] buffercontent; // Start with an invalid address
  reg        first;
  reg  [7:0] resp_data_reg;
  reg [13:0] load_address_reg;

  assign req_valid = req_valid_reg;
  assign req_offset = req_offset_reg;
  assign ram_ready = counter == 16'h2000;

  wire [13:0] load_address = { 2'b00, load_address_reg[12:1]};
  wire [15:0] load_datain = { resp_data_reg, resp_data_reg };
  
  wire        nibble_mask_hi = load_address_reg[0];
  wire        nibble_mask_lo = !load_address_reg[0];
  wire  [3:0] load_maskwren = { nibble_mask_hi, nibble_mask_hi, nibble_mask_lo, nibble_mask_lo };

  SB_SPRAM256KA rambank (
    .DATAIN(ram_ready ? datain : load_datain),
    .ADDRESS(ram_ready ? address : load_address),
    .MASKWREN(ram_ready ? maskwren : load_maskwren),
    .WREN(ram_ready ? wren : 1'b1),
    .CHIPSELECT(ram_ready ? chipselect : 1'b1),
    .CLOCK(clk),
    .STANDBY(ram_ready ? standby : 1'b0),
    .SLEEP(ram_ready ? sleep : 1'b0),
    .POWEROFF(ram_ready ? poweroff : 1'b1),
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
          load_address_reg <= counter;
          resp_data_reg <= resp_data;
          counter <= counter + 1;
          first <= 1'b0;
        end
        if (pw_end & ~first) req_offset_reg <= req_offset_reg + 32'h800;
      end
    end
  end
endmodule
