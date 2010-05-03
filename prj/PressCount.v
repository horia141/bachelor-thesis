module PressCount(clock,reset,count,nr_presses);
   input wire        clock;
   input wire        reset;

   input wire        count;

   output wire [7:0] nr_presses;

   wire [7:0] 	     seq_next;
   wire [11:0] 	     seq_oreg;
   wire [7:0] 	     seq_oreg_wen;

   wire [19:0] 	     coderom_data_o;

   wire [7:0] 	     alu_result;

   wire 	     pushbtn_button_status;

   wire [7:0] 	     ledbank_leds;

   assign nr_presses = ledbank_leds;
   
   Seq
   seq (.clock(clock),
	.reset(reset),

	.inst(coderom_data_o),
	.inst_en(1),
	.ireg_0(alu_result),
	.ireg_1({7'h0,pushbtn_button_status}),
	.ireg_2(8'h00),
	.ireg_3(8'h00),

	.next(seq_next),
	.oreg(seq_oreg),
	.oreg_wen(seq_oreg_wen));

   PressCountRom
   coderom (.addr(seq_next[2:0]),
	    .data_o(coderom_data_o));

   Alu
   alu (.clock(clock),
	.reset(reset),

	.inst(seq_oreg),
	.inst_en(seq_oreg_wen[0]),

	.result(alu_result));

   PushBtn
   pushbtn (.clock(clock),
	    .reset(reset),

	    .inst(seq_oreg),
	    .inst_en(seq_oreg_wen[1]),
	    .button(count),

	    .button_status(pushbtn_button_status));

   LedBank
   ledbank (.clock(clock),
	    .reset(reset),

	    .inst(seq_oreg),
	    .inst_en(seq_oreg_wen[2]),

	    .leds(ledbank_leds));
endmodule // PressCount
