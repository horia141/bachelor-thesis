`timescale 1ns/10ps

module AluTest;
   reg        clock;
   reg 	      reset;

   reg [11:0] inst;
   reg 	      inst_wen;

   wire [7:0] result;

   initial begin
      #0 $dumpfile("out/AluTest.vcd");
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
      // Test for all instructions after reset.
      #8 inst = {`Alu_LDI,8'h1A};
      #0 inst_wen = 1;

      #4 inst = {`Alu_ADD,8'h01};
      #0 inst_wen = 1;

      #4 inst = {`Alu_NOP,8'h00};
      #0 inst_wen = 1;

      #4 inst = {`Alu_SUB,8'h02};
      #0 inst_wen = 1;

      #4 inst = {`Alu_NOT,8'h00};
      #0 inst_wen = 1;

      #4 inst = {`Alu_AND,8'h0F};
      #0 inst_wen = 1;

      #4 inst = {`Alu_IOR,8'hF1};
      #0 inst_wen = 1;

      #4 inst = {`Alu_XOR,8'hAF};
      #0 inst_wen = 1;

      #4 inst = {`Alu_SHL,8'h01};
      #0 inst_wen = 1;

      #4 inst = {`Alu_SHR,8'h02};
      #0 inst_wen = 1;

      #4 inst = {`Alu_NOP,8'h00};
      #0 inst_wen = 1;

      #4 inst = {`Alu_ADD,8'h01};
      #0 inst_wen = 1;

      // A little delay here from the #4 to simulate the actual delay from
      // a real wire, when receiving a new command from a controller.
      #5 inst_wen = 0;

      #7 inst = {`Alu_SUB,8'h02};
      #0 inst_wen = 1;

      // Same type of delay as above.
      #5 inst_wen = 0;

      // Same type of delay as above.
      #4 inst_wen = 1;

      // An invalid instruction
      #3 inst = {4'hF,8'h02};
      #0 inst_wen = 1;

      // This instruction won't execute, because we're in the error state.
      #4 inst = {`Alu_ADD,8'h03};
      #0 inst_wen = 1;

      // We need to reset the system to be able to pull
      // it out of the error state.
      #2 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #2 inst = {`Alu_LDI,8'hAA};
      #0 inst_wen = 1;

      #4 inst = {`Alu_NOP,8'h00};
      #0 inst_wen = 1;
   end

   Alu #()
   alu (.clock(clock),
	.reset(reset),
	
	.inst(inst),
	.inst_wen(inst_wen),

	.result(result));
endmodule
