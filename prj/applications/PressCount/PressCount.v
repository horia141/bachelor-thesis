module PressCount(clock0,clock180,reset,countu,countd,nr_presses,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire        clock0;
   input wire        clock180;
   input wire        reset;

   input wire        countu;
   input wire        countd;

   output wire [7:0] nr_presses;
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

   wire              pushbtnu_button_status;
   
   wire              pushbtnd_button_status;

   wire [7:0]        ledbank_leds;

   assign nr_presses = ledbank_leds;
   
   Seq
   seq (.clock(clock0),
        .reset(reset),

        .inst(coderom_data_o),
        .inst_text(coderomtext_data_o),
        .inst_en(1),
        .ireg_0(alu_result),
        .ireg_1({7'h0,pushbtnu_button_status}),
        .ireg_2({7'h0,pushbtnd_button_status}),
        .ireg_3(8'h00),

        .next(seq_next),
        .oreg(seq_oreg),
        .oreg_wen(seq_oreg_wen));

   PressCountRom
   coderom (.addr(seq_next),
            .data_o(coderom_data_o));

`ifdef SIM
   PressCountRomText
   coderomtext (.addr(seq_next),
                .data_o(coderomtext_data_o));
`endif

   Alu
   alu (.clock(clock180),
        .reset(reset),

        .inst(seq_oreg),
        .inst_en(seq_oreg_wen[0]),

        .result(alu_result));

   PushBtn #(.DebounceWait(40000),
             .DebounceSize(16))
   pushbtnu (.clock(clock180),
             .reset(reset),

             .inst(seq_oreg),
             .inst_en(seq_oreg_wen[1]),
             .button(countu),

             .button_status(pushbtnu_button_status));

   PushBtn #(.DebounceWait(40000),
             .DebounceSize(16))
   pushbtnd (.clock(clock180),
             .reset(reset),

             .inst(seq_oreg),
             .inst_en(seq_oreg_wen[2]),
             .button(countd),

             .button_status(pushbtnd_button_status));

   LedBank
   ledbank (.clock(clock180),
            .reset(reset),

            .inst(seq_oreg),
            .inst_en(seq_oreg_wen[3]),

            .leds(ledbank_leds));

   VGA1
   vga (.clock(clock180),
        .reset(reset),

        .inst(seq_oreg),
        .inst_en(seq_oreg_wen[4]),

        .vga_hsync(vga_hsync),
        .vga_vsync(vga_vsync),
        .vga_r(vga_r),
        .vga_g(vga_g),
        .vga_b(vga_b));
endmodule // PressCount
