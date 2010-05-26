`timescale 1ns/10ps

module RegBankS8Sim;
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
      #8 inst = {`RegBankS8_LD0,8'hAE};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx000};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD1,8'hAB};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx001};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD2,8'hEF};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx010};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD3,8'h2F};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx011};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD4,8'h72};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx100};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD5,8'h3E};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx101};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD6,8'h92};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx110};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD7,8'h61};
      inst_en = 1;

      #4 inst = {`RegBankS8_RDO,8'bxxxxx111};
      inst_en = 1;

      #4 inst = {`RegBankS8_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`RegBankS8_LD1,8'h87};
      inst_en = 0;

      #4 inst = {`RegBankS8_LD0,8'hAE};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'hAB};
      inst_en = 1;

      #4 inst = {`RegBankS8_LD1,8'h27};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`RegBankS8_LD0,8'h1A};
      inst_en = 1;

      #4 inst = {`RegBankS8_NOP,8'bxxxxxxxx};
      inst_en = 1;      
   end

   RegBankS8
   rbs8 (.clock(clock),
	 .reset(reset),

	 .inst(inst),
	 .inst_en(inst_en),

	 .out(out));
endmodule // RegBankS8Sim
