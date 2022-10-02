/*
 * top.v
 *
 * vim: ts=4 sw=4
 *
 * Top-level module for MCH2022 SPI skeleton
 *
 */

`default_nettype none

module top (
  // UART (to RP2040)
  output wire       uart_tx,

  // SPI Slave (to ESP32)
  input  wire       spi_mosi,
  output wire       spi_miso,
  input  wire       spi_clk,
  input  wire       spi_cs_n,

  // IRQ
  output wire       irq_n,

  // RGB Leds
  output wire [2:0] rgb,

  // Clock
  input  wire       clk_in
);

  // Signals
  // -------

  // Button reports
  wire [15:0] btn_rpt_state;
  wire [15:0] btn_rpt_change;
  wire        btn_rpt_stb;

  // UART sender
  reg   [7:0] uart_data_reg;
  reg         uart_valid_reg;
  wire  [7:0] uart_data;
  wire        uart_valid;
  reg   [7:0] uart_data1;
  wire        uart_valid1;
  wire  [7:0] uart_data2;
  wire        uart_valid2;
  wire        uart_ack;

  // Clock / Reset
  wire        clk;
  wire        rst;



  // SPI Flexible
  // ------------

  // This example uses the individual SPI core components
  // which is more flexible and allows multiple connections to the
  // protocol wrapper. Here there is a command decoder for the button
  // state reports and the wishbone interface.


  // Signals
    // Raw core IF
  wire [7:0] usr_mosi_data;
  wire       usr_mosi_stb;

  wire [7:0] usr_miso_data;
  wire       usr_miso_ack;

  wire       csn_state;
  wire       csn_rise;
  wire       csn_fall;

    // Protocol IF
  wire [7:0] pw_wdata;
  wire       pw_wcmd;
  wire       pw_wstb;

  wire       pw_end;

  wire       pw_req;
  wire       pw_gnt;

  wire [7:0] pw_rdata;
  wire       pw_rstb;

  wire [3:0] pw_irq;
  wire       irq;

  // Device Core
  spi_dev_core core_I (
    .spi_miso      (spi_miso),
    .spi_mosi      (spi_mosi),
    .spi_clk       (spi_clk),
    .spi_cs_n      (spi_cs_n),
    .usr_mosi_data (usr_mosi_data),
    .usr_mosi_stb  (usr_mosi_stb),
    .usr_miso_data (usr_miso_data),
    .usr_miso_ack  (usr_miso_ack),
    .csn_state     (csn_state),
    .csn_rise      (csn_rise),
    .csn_fall      (csn_fall),
    .clk           (clk),
    .rst           (rst)
  );

  // Protocol wrapper
  spi_dev_proto proto_I (
    .usr_mosi_data (usr_mosi_data),
    .usr_mosi_stb  (usr_mosi_stb),
    .usr_miso_data (usr_miso_data),
    .usr_miso_ack  (usr_miso_ack),
    .csn_state     (csn_state),
    .csn_rise      (csn_rise),
    .csn_fall      (csn_fall),
    .pw_wdata      (pw_wdata),
    .pw_wcmd       (pw_wcmd),
    .pw_wstb       (pw_wstb),
    .pw_end        (pw_end),
    .pw_req        (pw_req),
    .pw_gnt        (pw_gnt),
    .pw_rdata      (pw_rdata),
    .pw_rstb       (pw_rstb),
    .pw_irq        (pw_irq),
    .irq           (irq),
    .clk           (clk),
    .rst           (rst)
  );

  // Command decoder for the F4 command
  // (button state reports from ESP32)
  spi_dev_scmd #(
    .CMD_BYTE (8'hf4),
    .CMD_LEN  (4)
  ) scmd_f4_I (
    .pw_wdata (pw_wdata),
    .pw_wcmd  (pw_wcmd),
    .pw_wstb  (pw_wstb),
    .pw_end   (pw_end),
    .cmd_data ({btn_rpt_state, btn_rpt_change}),
    .cmd_stb  (btn_rpt_stb),
    .clk      (clk),
    .rst      (rst)
  );

  assign pw_irq[3:1] = 3'b000;
  assign irq_n = irq ? 1'b0 : 1'bz;

  // LEDS
  reg red1, red2, green1, green2, blue;
  wire red, green;

  SB_RGBA_DRV #(
    .CURRENT_MODE("0b1"),       // half current
    .RGB0_CURRENT("0b000011"),  // 4 mA
    .RGB1_CURRENT("0b000011"),  // 4 mA
    .RGB2_CURRENT("0b000011")   // 4 mA
  ) RGBA_DRIVER (
    .CURREN(1'b1),
    .RGBLEDEN(1'b1),
    .RGB1PWM(red),
    .RGB2PWM(blue),
    .RGB0PWM(green),
    .RGB0(rgb[0]),
    .RGB1(rgb[1]),
    .RGB2(rgb[2])
  );

  // read 2048 bytes from ESP file with FID 0xDABBAD00

  wire        req_valid;  // immediately request
  wire        req_ready;  // true when request processed
  wire  [7:0] resp_data;  // one byte of memory
  wire        resp_valid; // asserted when a new byte is available
  wire [31:0] req_offset; // offset of file
  spi_dev_fread #(
    .INTERFACE("STREAM")
  ) _fread (
    .clk (clk),
    .rst (rst),

    // SPI interface
    .pw_wdata     (pw_wdata),
    .pw_wcmd      (pw_wcmd),
    .pw_wstb      (pw_wstb),
    .pw_end       (pw_end),
    .pw_req       (pw_req),
    .pw_gnt       (pw_gnt),
    .pw_rdata     (pw_rdata),
    .pw_rstb      (pw_rstb),
    .pw_irq       (pw_irq[0]),

    // Read request interface
    .req_file_id  (32'hDABBAD00),
    .req_offset   (req_offset),
    .req_len      (11'h7FF), // One less than the actual requested length!

    .req_valid    (req_valid),
    .req_ready    (req_ready),

    // Stream reply interface
    .resp_data    (resp_data),
    .resp_valid   (resp_valid)
  );

  wire ram_ready;
  wire [13:0] ram_address;
  wire [15:0] ram_dataout;
  ram_test my_ram (
    .clk (clk),
    .rst (rst),
    .pw_end       (pw_end),
    .req_valid    (req_valid),
    .req_ready    (req_ready),

    // Stream reply interface
    .resp_data    (resp_data),
    .resp_valid   (resp_valid),
    .req_offset(req_offset),
    .red(red2),
    .ram_ready(ram_ready),
    .address(ram_address),
    .dataout(ram_dataout)    
  );

  wire uart_ready;
  uart_test my_uart (
    .clk (clk),
    .rst (rst | ~ram_ready),
    .uart_ack(uart_ack),
    .uart_ready(uart_ready),
    .uart_valid(uart_valid2),
    .uart_data(uart_data2),
    .address(ram_address),
    .data(ram_dataout)
  );

  assign uart_data = uart_ready ? uart_data1 : uart_data2;
  assign uart_valid = uart_ready ? uart_valid1 : uart_valid2;
  assign red = uart_ready ? red1 : red2;
  assign green = uart_ready ? green1 : (req_offset == 32'h1000);

  button_test my_button (
    .clk (clk),
    .uart_ack(uart_ack),
    .uart_ready(uart_ready),
    .uart_valid(uart_valid1),
    .uart_data(uart_data1),
    .btn_rpt_state(btn_rpt_state),
    .btn_rpt_change(btn_rpt_change),
    .btn_rpt_stb(btn_rpt_stb),
    .red(red1),
    .green(green1),
    .blue(blue)
  );

  // Core
  uart_tx #(
    .DIV_WIDTH(8)
  ) uart_I (
    .tx    (uart_tx),
    .data  (uart_data),
    .valid (uart_valid),
    .ack   (uart_ack),
    .div   (8'd28), // 30M / (28 + 2) = 1 MBaud
    .clk   (clk),
    .rst   (rst)
  );

  // Clock/Reset Generation
  // ----------------------

  sysmgr sysmgr_I (
    .clk_in (clk_in),
    .clk    (clk),
    .rst    (rst)
  );

endmodule // top
