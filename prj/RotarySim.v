`timescale 1ns/10ps

module RotarySim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;
   reg [1:0]   rotary;

   wire        rotary_left;
   wire        rotary_right;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #10000 $finish;
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
      // Test for all instructions after reset
      #8 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;
      rotary = 2'b00;

      #4 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      // A little delay from #4 to simulate the actual delay from a real
      // wire when receining a new command from a controller.
      #5 inst_en = 0;

      #7 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      // Same type of delay as above.
      #5 inst_en = 0;

      // Same type of delay as above.
      #4 inst_en = 1;

      // An invalid instructions. This will send the Rotary into the erory state.
      #3 inst = {4'hA,8'hBB};
      inst_en = 1;

      // This instruction won't execute, because the Rotary is in the error state.
      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Only a reset will return the Rotary into a normal, ready, state.
      #9 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #3 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Simulate a basic turn left of the rotary button.
      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;
      rotary = 2'b01;

      #7 rotary = 2'b00;

      #9 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      // Simulate a basic turn right of the rotary button.
      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #3 rotary = 2'b10;

      #5 rotary = 2'b00;

      #4 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      // Simulate a turn left beging done for a long time and reads being made simultaneously.
      // Only one read/clear cycle chould return Triggered, the others should be Free.
      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #3 rotary = 2'b01;

      #5 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      #2 rotary = 2'b00;

      // Simulate a turn right beging done for a long time and reads being made simultaneously.
      // Only one read/clear cycle chould return Triggered, the others should be Free.
      #6 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #3 rotary = 2'b10;

      #5 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      #2 rotary = 2'b00;

      // Simulate many turn left actions with only one read.
      #6 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 rotary = 2'b01;

      #7 rotary = 2'b00;

      #5 rotary = 2'b01;

      #4 rotary = 2'b00;

      #12 rotary = 2'b01;

      #7 rotary = 2'b00;

      #1 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDL,8'bxxxxxxxx};
      inst_en = 1;

      // Simulate many turn right actions with only one read.
      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 rotary = 2'b10;

      #7 rotary = 2'b00;

      #5 rotary = 2'b10;

      #4 rotary = 2'b00;

      #12 rotary = 2'b10;

      #7 rotary = 2'b00;

      #1 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDR,8'bxxxxxxxx};
      inst_en = 1;
   end

   Rotary
   rotary (.clock(clock),
	   .reset(reset),

	   .inst(inst),
	   .inst_en(inst_en),

	   .rotary(rotary),

	   .rotary_left(rotary_left),
	   .rotary_right(rotary_right));
endmodule // RotarySim
