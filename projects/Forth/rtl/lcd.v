`default_nettype none

module lcd(
  input  wire clk,
  input  wire io_wr,
  input  wire [15:0] io_addr,
  input  wire [15:0] io_dout,
  output wire [15:0] io_din,

  // LCD
  output reg  [7:0] lcd_d,
  output reg        lcd_rs,
  output            lcd_wr_n,
  output            lcd_cs_n,
  output            lcd_rst_n,
  input             lcd_fmark,
  input             lcd_mode,
);

  // ######   LCD   ###########################################

  // Software control of special wires

  reg [1:0] lcd_ctrl = 2'b01;
  assign {lcd_rst_n, lcd_cs_n} = lcd_ctrl;

  // Set WR in the second half of the clock cycle using DDR pin mode to allow the data lines to settle

  SB_IO #(.PIN_TYPE(6'b0100_01)) lcdwrn (.OUTPUT_CLK(clk), .PACKAGE_PIN(lcd_wr_n), .D_OUT_0(1'b0), .D_OUT_1(lcd_write), .OUTPUT_ENABLE(1'b1));
  reg lcd_write = 0;

`ifdef TEXT_SCREEN
  // Framebuffer & font data

  reg [10:0] lcd_addr;
  reg [7:0] characters [3*512-1:0]; // [1199:0] is sufficient, but RAM blocks come in 512 bytes...
  reg [7:0]       font [1023:0]; initial $readmemh("font-c64-ascii.hex", font);

  reg [7:0] read_char;
  reg [7:0] read_font;

  // Color registers for a beautiful output

  reg [15:0] color_fg0 = 16'hFD20; // Orange
  reg [15:0] color_bg0 = 16'h000F; // Navy
  reg [15:0] color_fg1 = 16'h07FF; // Cyan
  reg [15:0] color_bg1 = 16'h000F; // Navy

  // Internal signals for textmode generation

  reg toggle = 0;
  reg fmark_sync1 = 0;
  reg fmark_sync2 = 0;
  reg updating = 0;

  reg [8:0] xpos; // 0 to 320-1
  reg [7:0] ypos; // 0 to 240-1

  reg [7:0] char;
  reg [7:0] bitmap;
  reg [8:0] data0, data1;

  reg [2:0] fontrow;
  reg colorswitch;
  reg [10:0] characterindex;

  always @(posedge clk) begin

    // Reads from font data set & character buffer

    char   <= characters[toggle ? characterindex : lcd_addr            ]; // 7-Bit ASCII. Using char[7] for alternate colors.
    bitmap <=       font[toggle ? lcd_addr       : {char[6:0], fontrow}]; // 8x8 pixel font bitmap data.

    case (toggle)
      0: read_font <= bitmap; // Software can read these values three clock cycles after setting the address.
      1: read_char <= char;
    endcase

    toggle <= ~toggle; // Toggle between high and low part of data to LCD and between logic and software read access.

    characterindex <= 0;
    fontrow <= 0;

    // Synchronise incoming asynchronous VSYNC signal to clk

    fmark_sync1 <= lcd_fmark;
    fmark_sync2 <= fmark_sync1;

    // Logik to push data to LCD

    if (fmark_sync2 & ~updating) // VSYNC active and not yet updating?
    begin
      xpos <= 0;
      ypos <= 1;

      data0 <= {1'b0, 8'h00}; //   NOP command
      data1 <= {1'b0, 8'h2C}; // RAMWR command

      lcd_write <= 0;       // Nothing to write
      updating <= toggle; // Start when toggle is high, updating starts with toggle low in next cycle.
    end
    else
    begin
      if (updating) // Currently updating? Push data. One more pair of data is pushed for RAMWR command.
      begin
        lcd_write <= 1;

        case (toggle)
          0: begin
               {lcd_rs, lcd_d} <= data0;
               colorswitch <= char[7]; // Using MSB of character to switch to a different set of colors
               characterindex <= xpos[8:3] + 40 * ypos[7:3];
             end

          1: begin
               {lcd_rs, lcd_d} <= data1;

               {data1, data0} <= bitmap[~xpos[2:0]] ?
                             colorswitch ? {1'b1, color_fg1[7:0], 1'b1, color_fg1[15:8]} :
                                           {1'b1, color_fg0[7:0], 1'b1, color_fg0[15:8]} :
                             colorswitch ? {1'b1, color_bg1[7:0], 1'b1, color_bg1[15:8]} :
                                           {1'b1, color_bg0[7:0], 1'b1, color_bg0[15:8]} ;

               fontrow <= ypos[2:0];

               if (ypos == 239) begin xpos <= xpos + 1; ypos <= 0; end else ypos <= ypos + 1;
               updating <= ~((xpos == 320) & (ypos == 1));
             end
        endcase

      end
      else // Software control of the LCD wires for initialisation sequence or if software wants to slowly draw graphics
      begin
        if (io_wr & io_addr[11] & (io_addr[8:4] == 10)) begin
          {lcd_rs, lcd_d}  <= io_dout ^ 9'h100; // Data written with 9th bit set is written in command mode.
          lcd_write        <= 1;
        end
        else
          lcd_write <= 0;
      end
    end
  end
`endif



  reg [4:0] state;
  reg [15:0] reg_x1, reg_y1, reg_x2, reg_y2, reg_color;
  reg [31:0] pixel_count;
  reg signed [15:0] dx, dy;
  reg signed[15:0] sx, sy;
  reg signed[15:0] err;
  wire signed [15:0] e2;
  assign e2 = err + err;

  reg fmark_sync1 = 0;
  reg fmark_sync2 = 0;

  // ######   IO Ports   ######################################

  /*
      0820      LCD addr        LCD addr
      0830      Font(addr)      Font(addr)  -- Readback three cycles after setting access addr
      0840      Text(addr)      Text(addr)  -- Readback three cycles after setting access addr
      0850      Color FG0       Color FG0   -- Foreground color 0
      0860      Color BG0       Color BG0   -- Background color 0
      0870      Color FG1       Color FG1   -- Foreground color 1
      0880      Color BG1       Color BG1   -- Background color 1
      0890      LCD Ctrl        LCD Ctrl
      08A0                      LCD Data (writeonly)

      0940      pixel           set color and draw pixel
      0950      X               pixel X ooordinate
      0960      Y               pixel Y ooordinate

  */

  assign io_din =

`ifdef TEXT_SCREEN
    (io_addr[11] & (io_addr[7:4] ==  2) ?  lcd_addr                                 : 16'd0) |
    (io_addr[11] & (io_addr[7:4] ==  3) ?  read_font                                : 16'd0) |
    (io_addr[11] & (io_addr[7:4] ==  4) ?  read_char                                : 16'd0) |
    (io_addr[11] & (io_addr[7:4] ==  5) ?  color_fg0                                : 16'd0) |
    (io_addr[11] & (io_addr[7:4] ==  6) ?  color_bg0                                : 16'd0) |
    (io_addr[11] & (io_addr[7:4] ==  7) ?  color_fg1                                : 16'd0) |
    (io_addr[11] & (io_addr[7:4] ==  8) ?  color_bg1                                : 16'd0) |
    (io_addr[11] & (io_addr[7:4] ==  9) ?  {updating,fmark_sync2,lcd_mode,lcd_ctrl} : 16'd0) |
    //                              10  ?  lcd_data, writeonly
`endif
    (io_addr[11] & (io_addr[7:4] ==  9) ?  {fmark_sync2, lcd_mode,lcd_ctrl}           : 16'd0);

  always @(posedge clk) begin
    fmark_sync1 <= lcd_fmark;
    fmark_sync2 <= fmark_sync1;

  
`ifdef TEXT_SCREEN
    if (io_wr & io_addr[11] & (io_addr[8:4] ==  2)) lcd_addr  <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] ==  3))       font[lcd_addr] <= io_dout[7:0];
    if (io_wr & io_addr[11] & (io_addr[8:4] ==  4)) characters[lcd_addr] <= io_dout[7:0];

    if (io_wr & io_addr[11] & (io_addr[8:4] ==  5)) color_fg0 <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] ==  6)) color_bg0 <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] ==  7)) color_fg1 <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] ==  8)) color_bg1 <= io_dout;
`endif
    if (io_wr & io_addr[11] & (io_addr[8:4] ==  9)) lcd_ctrl <= io_dout;
    //                                         10   lcd_data write needs special logic, elsewhere
    if (io_wr & io_addr[11] & (io_addr[8:4] == 21)) reg_x1 <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] == 22)) reg_y1 <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] == 23)) reg_x2 <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] == 24)) reg_y2 <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] == 25)) pixel_count[31:16] <= io_dout;
    if (io_wr & io_addr[11] & (io_addr[8:4] == 26)) pixel_count[15: 0] <= io_dout;
    case (state)
        0: begin
            if (io_wr & io_addr[11] & (io_addr[8:4] == 10)) begin
                // Als bit 8 hoog is ($100), is het een commando -> lcd_rs wordt 0
                // Als bit 8 laag is ($000), is het data     -> lcd_rs wordt 1
                lcd_rs    <= !io_dout[8];
                lcd_d     <= io_dout[7:0];
                state <= 15;
                lcd_write <= 1'b1;
            end else
            if (io_wr & io_addr[11] & (io_addr[8:4] == 20)) begin
                reg_color <= io_dout;
                state <= 1;
            end else
            if (io_wr & io_addr[11] & (io_addr[8:4] == 27)) begin
                reg_color <= io_dout;
                // --- BRESENHAM LIJN INITIALISATIE ---
                // dx = abs(x1 - x0) en sx = x0 < x1 ? 1 : -1
                if (reg_x2 >= reg_x1) begin dx <= reg_x2 - reg_x1; sx <= 16'd1;  end
                else                  begin dx <= reg_x1 - reg_x2; sx <= -16'd1; end

                // dy = -abs(y1 - y0) en sy = y0 < y1 ? 1 : -1
                if (reg_y2 >= reg_y1) begin dy <= reg_y1 - reg_y2; sy <= 16'd1;  end
                else                  begin dy <= reg_y2 - reg_y1; sy <= -16'd1; end
                state <= 17;
            end
        end
        1, 18:      begin {lcd_rs, lcd_d} <= {1'b0, 8'h2A}; lcd_write <= 1'b1; state <= state + 1; end // $2A (Column)
        2, 19, 21:  begin {lcd_rs, lcd_d} <= {1'b1, reg_x1[15:8]}; state <= state + 1; end
        3, 20, 22:  begin {lcd_rs, lcd_d} <= {1'b1, reg_x1[ 7:0]}; state <= state + 1; end
        4:          begin {lcd_rs, lcd_d} <= {1'b1, reg_x2[15:8]}; state <= state + 1; end
        5:          begin {lcd_rs, lcd_d} <= {1'b1, reg_x2[ 7:0]}; state <= state + 1; end

        6, 23:      begin {lcd_rs, lcd_d} <= {1'b0, 8'h2B}; state <= state + 1; end // $2B (Page)
        7, 24, 26:  begin {lcd_rs, lcd_d} <= {1'b1, reg_y1[15:8]}; state <= state + 1; end
        8, 25, 27:  begin {lcd_rs, lcd_d} <= {1'b1, reg_y1[ 7:0]}; state <= state + 1; end
        9:          begin {lcd_rs, lcd_d} <= {1'b1, reg_y2[15:8]}; state <= state + 1; end
        10:         begin {lcd_rs, lcd_d} <= {1'b1, reg_y2[ 7:0]}; state <= state + 1; end

        11, 28:     begin {lcd_rs, lcd_d} <= {1'b0, 8'h2C}; state <= state + 1; end // $2C (Write RAM)
        12, 29:     begin {lcd_rs, lcd_d} <= {1'b1, reg_color[15:8]}; state <= state + 1; end
        13: begin
            {lcd_rs, lcd_d} <= {1'b1, reg_color[ 7:0]};
            if (pixel_count != 1) begin
                pixel_count <= pixel_count - 1;
                state       <= 12;
            end else begin
                state <= 15;
            end
        end
        15: begin
            lcd_write <= 1'b0;
            state <= 0;
        end
        17: begin err <= dx + dy; state <= state + 1; end
        //18: {lcd_rs, lcd_d} <= {1'b0, 8'h2A}; // $2A (Column)
        //19: {lcd_rs, lcd_d} <= {1'b1, reg_x1[15:8]};
        //20: {lcd_rs, lcd_d} <= {1'b1, reg_x1[ 7:0]};
        //21: {lcd_rs, lcd_d} <= {1'b1, reg_x1[15:8]};
        //22: {lcd_rs, lcd_d} <= {1'b1, reg_x1[ 7:0]};

        //23: {lcd_rs, lcd_d} <= {1'b0, 8'h2B}; // $2B (Page)
        //24: {lcd_rs, lcd_d} <= {1'b1, reg_y1[15:8]};
        //25: {lcd_rs, lcd_d} <= {1'b1, reg_y1[ 7:0]};
        //26: {lcd_rs, lcd_d} <= {1'b1, reg_y1[15:8]};
        //27: {lcd_rs, lcd_d} <= {1'b1, reg_y1[ 7:0]};

        //28: {lcd_rs, lcd_d} <= {1'b0, 8'h2C}; // $2C (Write RAM)
        //29: {lcd_rs, lcd_d} <= {1'b1, reg_color[15:8]};
        30: begin
            {lcd_rs, lcd_d} <= {1'b1, reg_color[ 7:0]};
            if (e2 >= dy) begin
                if (reg_x1 == reg_x2) begin
                    state <= 15;
                end else begin
                    err   <= err + dy;
                    reg_x1 <= reg_x1 + sx;
                    state <= state + 1;
                end
            end else begin
                state <= state + 1;
            end
        end
        31: begin
            lcd_write <= 1'b0;
            if (e2 <= dx) begin
                if (reg_y1 == reg_y2) begin
                    state <= 0;
                end else begin
                    err   <= err + dx;
                    reg_y1 <= reg_y1 + sy;
                    state <= 5'd18;
                end
            end else begin
                state <= 5'd18;
            end
        end
    endcase
  end
endmodule
