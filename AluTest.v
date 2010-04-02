`timescale 1ns/10ps

module AluTest;
   reg        clock;
   reg 	      reset;

   reg [11:0] inst;
   reg 	      inst_wen;

   wire [7:0] result;

   initial begin
      #0 $dumpfile("AluTest.vcd");
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

   Alu #()
   alu (.clock(clock),
	.reset(reset),
	
	.inst(inst),
	.inst_wen(inst_wen),

	.result(result));
endmodule
