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
      // Test for all instructions after reset.
      #8 inst = {`LedBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LDI,8'b11010111};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD0,8'bxxxxxxx0};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD1,8'bxxxxxxx0};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD2,8'bxxxxxxx0};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD3,8'bxxxxxxx1};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD4,8'bxxxxxxx0};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD5,8'bxxxxxxx1};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD6,8'bxxxxxxx0};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD7,8'bxxxxxxx0};
      #0 inst_en = 1;

      #4 inst = {`LedBank_LD6,8'bxxxxxxx1};
      #0 inst_en = 1;

      // A little delay from #4 to simulate the actual delay from a real
      // wire when receiving a new command from a controller.
      #5 inst_en = 0;

      #7 inst = {`LedBank_LDI,8'b10100101};
      #0 inst_en = 1;

      // Same type of delay as above.
      #5 inst_en = 0;

      // Same type of delay as above.
      #4 inst_en = 1;

      // An invalid instruction
      #3 inst = {4'hE,8'hAA};
      #0 inst_en = 1;

      // This instruction won't execute, because we are in the error state.
      #4 inst = {`LedBank_LDI,8'b10010010};
      #0 inst_en = 1;

      // We need to reset the system to be able to pull
      // it out of the error state.
      #2 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #2 inst = {`LedBank_LDI,8'b00100101};
      #0 inst_en = 1;

      #4 inst = {`LedBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
   end

   LedBank #()
   ledbank (.clock(clock),
	    .reset(reset),

	    .inst(inst),
	    .inst_en(inst_en),

	    .leds(leds));
endmodule // SeqSim
