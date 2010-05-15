`timescale 1ns/10ps

module AluSim;
   reg        clock;
   reg 	      reset;

   reg [11:0] inst;
   reg 	      inst_en;

   wire [7:0] result;

   initial begin
      #0 $dumpfile(`VCDFILE);
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

   initial begin
      #0 inst_en = 0;

      // Test each instruction
      #0.1 inst_en = 0;
      
      #8 inst = {`Alu_LDI,8'hAB};
      inst_en = 1;


      #4 inst = {`Alu_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end

   Alu
   alu (.clock(clock),
	.reset(reset),
	
	.inst(inst),
	.inst_en(inst_en),

	.result(result));
endmodule // AluSim
