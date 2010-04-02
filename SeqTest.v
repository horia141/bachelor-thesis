`timescale 1ns/10ps

module SeqTest;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_wen;
   reg [7:0]   ireg_0;
   reg [7:0]   ireg_1;
   reg [7:0]   ireg_2;
   reg [7:0]   ireg_3;

   wire [7:0]  next;
   wire [11:0] oreg;
   wire [7:0]  oreg_wen;

   initial begin
      #0 $dumpfile("out/SeqTest.vcd");
      #0 $dumpvars;

      #1000 $finish;
   end

   initial begin
      #0 clock = 1;
      forever #2 clock = ~clock;
   end

   initial begin
      #0 reset = 0;
      #1 reset = 1;
      #4 reset = 0;
   end

   Seq #()
   seq (.clock(clock),
	.reset(reset),

	.inst(inst),
	.inst_wen(inst_wen),
	.ireg_0(ireg_0),
	.ireg_1(ireg_1),
	.ireg_2(ireg_2),
	.ireg_3(ireg_3),

	.next(next),
	.oreg(oreg),
	.oreg_wen(oreg_wen));
endmodule // SeqTest
