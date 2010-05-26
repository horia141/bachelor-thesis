`timescale 1ns/10ps

module RegBankP8Sim;
   reg        clock;
   reg        reset;

   reg [11:0] inst;
   reg 	      inst_en;

   wire [7:0] out_0;
   wire [7:0] out_1;
   wire [7:0] out_2;
   wire [7:0] out_3;
   wire [7:0] out_4;
   wire [7:0] out_5;
   wire [7:0] out_6;
   wire [7:0] out_7;

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
      #8 inst = {`RegBankP8_LD0,8'hBA};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD1,8'hFE};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD2,8'h23};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD3,8'h43};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD4,8'h12};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD5,8'hEA};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD6,8'hFE};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD7,8'hAB};
      inst_en = 1;
      
      #4 inst = {`RegBankP8_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`RegBankP8_LD1,8'h87};
      inst_en = 0;

      #4 inst = {`RegBankP8_LD0,8'hAE};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'hAB};
      inst_en = 1;

      #4 inst = {`RegBankP8_LD1,8'h27};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`RegBankP8_LD0,8'h1A};
      inst_en = 1;

      #4 inst = {`RegBankP8_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end

   RegBankP8
   rbp8 (.clock(clock),
	 .reset(reset),

	 .inst(inst),
	 .inst_en(inst_en),

	 .out_0(out_0),
	 .out_1(out_1),
	 .out_2(out_2),
	 .out_3(out_3),
	 .out_4(out_4),
	 .out_5(out_5),
	 .out_6(out_6),
	 .out_7(out_7));
endmodule // RegBankP8Sim
