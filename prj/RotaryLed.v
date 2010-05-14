module RotaryLed(clock,reset,rotary,leds);
   input wire        clock;
   input wire 	     reset;

   input wire [1:0]  rotary;

   output wire [7:0] leds;

   wire [7:0] 	     seq_next;
   wire [11:0] 	     seq_oreg;
   wire [7:0] 	     seq_oreg_wen;

   wire [19:0] 	     coderom_data_o;

   wire [7:0] 	     alu_result;

   wire 	     mrotary_rotary_left;
   wire 	     mrotary_rotary_right;

   wire [7:0] 	     ledbank_leds;

   assign leds = ledbank_leds;

   Seq
   seq (.clock(clock),
	.reset(reset),

	.inst(coderom_data_o),
	.inst_en(1),
	.ireg_0(alu_result),
	.ireg_1({7'h0,mrotary_rotary_left}),
	.ireg_2({7'h0,mrotary_rotary_right}),
	.ireg_3({8'h00}),

	.next(seq_next),
	.oreg(seq_oreg),
	.oreg_wen(seq_oreg_wen));

   RotaryLedRom
   coderom (.addr(seq_next[3:0]),
	    .data_o(coderom_data_o));

   Alu
   alu (.clock(clock),
	.reset(reset),

	.inst(seq_oreg),
	.inst_en(seq_oreg_wen[0]),

	.result(alu_result));

   Rotary
   mrotary (.clock(clock),
	    .reset(reset),

	    .inst(seq_oreg),
	    .inst_en(seq_oreg_wen[1]),
	    .rotary(rotary),

	    .rotary_left(mrotary_rotary_left),
	    .rotary_right(mrotary_rotary_right));

   LedBank
   ledbank (.clock(clock),
	    .reset(reset),

	    .inst(seq_oreg),
	    .inst_en(seq_oreg_wen[2]),

	    .leds(ledbank_leds));
endmodule // RotaryLed
