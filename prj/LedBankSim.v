`timescale 1ns/10ps

module SeqSim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;

   wire [7:0]  leds;

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
      #8 inst = {`LedBank_LDI,8'b00101100};
      inst_en = 1;

      #4 inst = {`LedBank_LD0,7'bxxxxxxx,1'b1};
      inst_en = 1;
      
      #4 inst = {`LedBank_LD1,7'bxxxxxxx,1'b1};
      inst_en = 1;

      #4 inst = {`LedBank_LD2,7'bxxxxxxx,1'b0};
      inst_en = 1;

      #4 inst = {`LedBank_LD3,7'bxxxxxxx,1'b0};
      inst_en = 1;

      #4 inst = {`LedBank_LD4,7'bxxxxxxx,1'b1};
      inst_en = 1;

      #4 inst = {`LedBank_LD5,7'bxxxxxxx,1'b0};
      inst_en = 1;

      #4 inst = {`LedBank_LD6,7'bxxxxxxx,1'b1};
      inst_en = 1;

      #4 inst = {`LedBank_LD7,7'bxxxxxxx,1'b1};
      inst_en = 1;

      #4 inst = {`LedBank_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`LedBank_LDI,8'b11001100};
      inst_en = 0;

      #4 inst = {`LedBank_LD1,7'bxxxxxxx,1'b0};
      inst_en = 1;

      // Test badinstruction.
      #4 inst = {8'hF,8'h10};
      inst_en = 1;

      #4 inst = {`LedBank_LDI,8'b11010101};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`LedBank_LDI,8'b10100101};
      inst_en = 1;

      #4 inst = {`LedBank_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end

   LedBank
   ledbank (.clock(clock),
	    .reset(reset),

	    .inst(inst),
	    .inst_en(inst_en),

	    .leds(leds));
endmodule // SeqSim
