module spi_dev_play_sound #(
    parameter [7:0] CMD_PLAY_SOUND = 8'hfa
)(
    // Protocol wrapper interface
    input  wire  [7:0] pw_wdata,
    input  wire        pw_wcmd,
    input  wire        pw_wstb,

    input  wire        pw_end,

    output wire        pw_req,
    input  wire        pw_gnt,

    output reg   [7:0] pw_rdata,
    output reg         pw_rstb,

    // External status indicator
    output wire        pw_irq,

    // "play_sound" request submit interface
    input  wire [7:0]  req_sound_id,
    input  wire        req_valid,
    output wire        req_ready,

    // Clock / Reset
    input  wire clk,
    input  wire rst
);

    // Signals
    reg       cmd_stb_play_sound;
    reg       cmd_active_play_sound;

    reg [3:0] tx_sel;
    wire      tx_sel_valid;
    wire      tx_stb;

    // Command decoder
    always @(posedge clk)
        cmd_stb_play_sound <= pw_wstb & pw_wcmd & (pw_wdata == CMD_PLAY_SOUND);

    always @(posedge clk)
        if (rst)
            cmd_active_play_sound <= 1'b0;
        else
            cmd_active_play_sound <= (cmd_active_play_sound & ~pw_end) | cmd_stb_play_sound;

    // IRQ: sound event pending
    assign pw_irq = req_valid;

    // Only request response buffer if we have data
    assign pw_req = cmd_active_play_sound & req_valid;

    // Accept new sound event when previous one consumed
    assign req_ready = pw_gnt & pw_rstb;

    // Write control
    always @(posedge clk)
        if (~pw_gnt)
            tx_sel <= 4'h0;
        else
            tx_sel <= tx_sel + tx_sel_valid;

    assign tx_sel_valid = (tx_sel == 4'h0);
    assign tx_stb = pw_gnt & tx_sel_valid;

    // Output data
    always @(posedge clk) begin
        case (tx_sel)
            4'h0: pw_rdata <= req_sound_id;
            default: pw_rdata <= 8'hxx;
        endcase

        pw_rstb <= tx_stb;
    end

endmodule
