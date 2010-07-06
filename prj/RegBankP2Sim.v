`timescale 1ns/10ps

module RegBankP2Sim;
   reg        clock;
   reg        reset;

   reg [11:0] inst;
   reg        inst_en;

   wire [7:0] out_0;
   wire [7:0] out_1;

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
      #8 inst = {`RegBankP2_LD0,8'hBA};
      inst_en = 1;

      #4 inst = {`RegBankP2_LD1,8'hFE};
      inst_en = 1;

      #4 inst = {`RegBankP2_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`RegBankP2_LD1,8'h87};
      inst_en = 0;

      #4 inst = {`RegBankP2_LD0,8'hAE};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'hAB};
      inst_en = 1;

      #4 inst = {`RegBankP2_LD1,8'h27};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`RegBankP2_LD0,8'h1A};
      inst_en = 1;

      #4 inst = {`RegBankP2_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end

   RegBankP2
   rbp2 (.clock(clock),
         .reset(reset),

         .inst(inst),
         .inst_en(inst_en),

         .out_0(out_0),
         .out_1(out_1));
endmodule // RegBankP2Sim
