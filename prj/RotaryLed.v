module RotaryLed(clock0,clock180,reset,rotary,leds,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire        clock0;
   input wire        clock180;
   input wire        reset;

   input wire [1:0]  rotary;

   output wire [7:0] leds;
   output wire       vga_hsync;
   output wire       vga_vsync;
   output wire       vga_r;
   output wire       vga_g;
   output wire       vga_b;

   wire [7:0]        seq_next;
   wire [11:0]       seq_oreg;
   wire [7:0]        seq_oreg_wen;

   wire [19:0]       coderom_data_o;

   wire [4095:0]     coderomtext_data_o;

   wire [7:0]        alu_result;

   wire              mrotary_rotary_left_status;
   wire              mrotary_rotary_right_status;

   wire [7:0]        ledbank_leds;

   assign leds = ledbank_leds;

   Seq
   seq (.clock(clock0),
        .reset(reset),

        .inst(coderom_data_o),
        .inst_text(coderomtext_data_o),
        .inst_en(1),
        .ireg_0(alu_result),
        .ireg_1({7'h0,mrotary_rotary_left_status}),
        .ireg_2({7'h0,mrotary_rotary_right_status}),
        .ireg_3({8'h00}),

        .next(seq_next),
        .oreg(seq_oreg),
        .oreg_wen(seq_oreg_wen));

   RotaryLedRom
   coderom (.addr(seq_next),
            .data_o(coderom_data_o));

`ifdef SIM
   RotaryLedRomText
   coderomtext (.addr(seq_next),
                .data_o(coderomtext_data_o));
`endif

   Alu
   alu (.clock(clock180),
        .reset(reset),

        .inst(seq_oreg),
        .inst_en(seq_oreg_wen[0]),

        .result(alu_result));

   Rotary
   mrotary (.clock(clock180),
            .reset(reset),

            .inst(seq_oreg),
            .inst_en(seq_oreg_wen[1]),
            .rotary(rotary),

            .rotary_left_status(mrotary_rotary_left_status),
            .rotary_right_status(mrotary_rotary_right_status));

   LedBank
   ledbank (.clock(clock180),
            .reset(reset),

            .inst(seq_oreg),
            .inst_en(seq_oreg_wen[2]),

            .leds(ledbank_leds));

   VGA
   vga (.clock(clock180),
        .reset(reset),

        .inst(seq_oreg),
        .inst_en(seq_oreg_wen[3]),

        .vga_hsync(vga_hsync),
        .vga_vsync(vga_vsync),
        .vga_r(vga_r),
        .vga_g(vga_g),
        .vga_b(vga_b));
endmodule // RotaryLed
