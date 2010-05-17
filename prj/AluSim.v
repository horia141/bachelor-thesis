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
      #0.1 inst_en = 0;

      // Test each instruction
      #8 inst = {`Alu_LDI,8'hAB};
      inst_en = 1;

      #4 inst = {`Alu_ADD,8'h01};
      inst_en = 1;

      #4 inst = {`Alu_SUB,8'h02};
      inst_en = 1;

      #4 inst = {`Alu_NOT,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Alu_AND,8'h16};
      inst_en = 1;

      #4 inst = {`Alu_IOR,8'h22};
      inst_en = 1;

      #4 inst = {`Alu_XOR,8'hF9};
      inst_en = 1;

      #4 inst = {`Alu_SHL,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Alu_SHR,8'bxxxxxxxx};
      inst_en = 1;
      
      #4 inst = {`Alu_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction
      #4 inst = {`Alu_LDI,8'h0A};
      inst_en = 0;

      #4 inst = {`Alu_ADD,8'h01};
      inst_en = 1;

      // Test bad instruction
      #4 inst = {8'hF,8'h10};
      inst_en = 1;

      #4 inst = {`Alu_LDI,8'hA0};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`Alu_LDI,8'hB0};
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
