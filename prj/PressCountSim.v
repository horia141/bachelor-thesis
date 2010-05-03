`timescale 1ns/10ps

module PressCountSim;
   reg         clock;
   reg 	       reset;

   reg 	       count;

   wire [7:0]  nr_presses;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #3000 $finish;
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
      #0  count = 0;
      #12 count = 1;
      #5  count = 0;

      #43 count = 1;
      #5  count = 0;

      #124 count = 1;
      #233 count = 0;
   end

   PressCount
   presscount (.clock(clock),
	       .reset(reset),

	       .count(count),

	       .nr_presses(nr_presses));
endmodule // PressCountSim
