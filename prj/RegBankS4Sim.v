`timescale 1ns/10ps

module RegBankS4Sim;
   reg        clock;
   reg        reset;

   reg [11:0] inst;
   reg 	      inst_en;

   wire [7:0] out;

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

      // Test each instruction.
      #8 inst = {`RegBankS4_LD0,8'hAE};
      inst_en = 1;

      #4 inst = {`RegBankS4_RDO,8'bxxxxxx00};
      inst_en = 1;

      #4 inst = {`RegBankS4_LD1,8'hAB};
      inst_en = 1;

      #4 inst = {`RegBankS4_RDO,8'bxxxxxx01};
      inst_en = 1;

      #4 inst = {`RegBankS4_LD2,8'hFF};
      inst_en = 1;

      #4 inst = {`RegBankS4_RDO,8'bxxxxxx10};
      inst_en = 1;

      #4 inst = {`RegBankS4_LD3,8'h22};
      inst_en = 1;

      #4 inst = {`RegBankS4_RDO,8'bxxxxxx11};
      inst_en = 1;

      #4 inst = {`RegBankS4_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`RegBankS4_LD1,8'h87};
      inst_en = 0;

      #4 inst = {`RegBankS4_LD0,8'hAE};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'hAB};
      inst_en = 1;

      #4 inst = {`RegBankS4_LD1,8'h27};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`RegBankS4_LD0,8'h1A};
      inst_en = 1;

      #4 inst = {`RegBankS4_NOP,8'bxxxxxxxx};
      inst_en = 1;      
   end

   RegBankS4
   rbs4 (.clock(clock),
	 .reset(reset),

	 .inst(inst),
	 .inst_en(inst_en),

	 .out(out));
endmodule // RegBankS4Sim
