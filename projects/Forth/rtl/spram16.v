`default_nettype none

module spram16 (
  input  wire        clk,
  input  wire        start_dma,
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
  input  wire [15:0] address,
  input  wire  [3:0] maskwren,
  input  wire        wren,
  output wire [15:0] dataout  
);
  reg [31:0] req_offset_reg;
  reg [16:0] counter;
  reg        req_valid_reg;  
  reg [31:0] buffercontent; // Start with an invalid address
  reg        first;
  reg  [7:0] resp_data_reg;
  reg [15:0] address_reg;
  reg [15:0] load_address_reg;
  reg        dma_active = 0;

  wire [15:0] load_address = { counter[16:1] };
  wire [15:0] load_datain  = { resp_data, resp_data }; // Direct de binnenkomende bus pakken
  
  wire        nibble_mask_hi = counter[0];
  wire        nibble_mask_lo = !counter[0];
  wire  [3:0] load_maskwren = { nibble_mask_hi, nibble_mask_hi, nibble_mask_lo, nibble_mask_lo };
  wire  [1:0] active_bank = mux_address[15:14];
 
  wire [15:0] sram_in_bank0, sram_in_bank1, sram_in_bank2, sram_in_bank3;

  wire [15:0] mux_datain;
  wire [15:0] mux_address;
  wire  [3:0] mux_maskwren;
  wire        mux_wren;

  assign req_valid = req_valid_reg;
  assign req_offset = req_offset_reg;
  assign ram_ready = !dma_active;

  assign mux_datain     = ram_ready ? datain     : load_datain;
  assign mux_address    = ram_ready ? address    : load_address;
  assign mux_maskwren   = ram_ready ? maskwren   : load_maskwren;
  assign mux_wren       = ram_ready ? wren       : resp_valid;
  
  SB_SPRAM256KA rambank0 (
    .DATAIN(mux_datain),
    .ADDRESS(mux_address),
    .MASKWREN(mux_maskwren),
    .WREN(mux_wren),
    .CHIPSELECT(1'b1),
    .CLOCK(clk),
    .STANDBY(1'b0),
    .SLEEP(~(active_bank == 2'b00)),
    .POWEROFF(1'b1),
    .DATAOUT(sram_in_bank0)
  );

  SB_SPRAM256KA rambank1 (
    .DATAIN(mux_datain),
    .ADDRESS(mux_address),
    .MASKWREN(mux_maskwren),
    .WREN(mux_wren),
    .CHIPSELECT(1'b1),
    .CLOCK(clk),
    .STANDBY(1'b0),
    .SLEEP(~(active_bank == 2'b01)),
    .POWEROFF(1'b1),
    .DATAOUT(sram_in_bank1)
  );

  SB_SPRAM256KA rambank2 (
    .DATAIN(mux_datain),
    .ADDRESS(mux_address),
    .MASKWREN(mux_maskwren),
    .WREN(mux_wren),
    .CHIPSELECT(1'b1),
    .CLOCK(clk),
    .STANDBY(1'b0),
    .SLEEP(~(active_bank == 2'b10)),
    .POWEROFF(1'b1),
    .DATAOUT(sram_in_bank2)
  );

  SB_SPRAM256KA rambank3 (
    .DATAIN(mux_datain),
    .ADDRESS(mux_address),
    .MASKWREN(mux_maskwren),
    .WREN(mux_wren),
    .CHIPSELECT(1'b1),
    .CLOCK(clk),
    .STANDBY(1'b0),
    .SLEEP(~(active_bank == 2'b11)),
    .POWEROFF(1'b1),
    .DATAOUT(sram_in_bank3)
  );
  
  wire [15:0] dataout = sram_in_bank3 | sram_in_bank2 | sram_in_bank1 | sram_in_bank0;

  always @(posedge clk) begin
    if (start_dma) begin
      req_offset_reg   <= 32'd0;
      counter          <= 17'd0;
      req_valid_reg    <= 1'b0;
      buffercontent    <= 32'hFFFFFFFF;
      first            <= 1'b1;
      dma_active       <= 1'b1;
    end
    else if (dma_active) begin
      if (counter < 17'h1FFFF) begin
        if (req_valid_reg) begin
          req_valid_reg <= req_valid_reg & ~req_ready;
        end 
        else if (buffercontent != req_offset_reg) begin
          buffercontent <= req_offset_reg;
          req_valid_reg <= 1'b1;
          first         <= 1'b1;
        end
        else begin
          if (resp_valid) begin
            counter <= counter + 1;
            first   <= 1'b0;
          end
          if (pw_end & ~first) req_offset_reg <= req_offset_reg + 32'h800;
        end
      end
      else if (req_offset_reg < 32'h10000) begin
        if (pw_end & ~first) req_offset_reg <= req_offset_reg + 32'h800;
      end
      else begin
        dma_active <= 1'b0;
      end
    end
  end
endmodule
