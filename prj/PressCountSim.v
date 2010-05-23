`timescale 1ns/10ps

module PressCountSim;
   reg         clock0;
   reg 	       clock180;
   reg 	       reset;

   reg 	       countu;
   reg 	       countd;

   wire [7:0]  nr_presses;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #5000 $finish;
   end

   initial begin
      #0 clock0 = 1;
      forever #2 clock0 = ~clock0;
   end

   initial begin
      #0 clock180 = 0;
      forever #2 clock180 = ~clock180;
   end

   initial begin
      #0 reset = 0;
      #1 reset = 1;
      #4 reset = 0;
   end

   initial begin
      #0 countu = 0;
      countd = 0;
      
      #12 countu = 1;
      
      #5  countu = 0;

      #43 countu = 1;
      
      #5  countu = 0;

      #124 countd = 1;
      
      #233 countd = 0;

      #4 countu = 1;

      #100 countu = 0;

      #4 countu = 1;

      #100 countu = 0;
   end

   PressCount
   presscount (.clock0(clock0),
	       .clock180(clock180),
	       .reset(reset),

	       .countu(countu),
	       .countd(countd),

	       .nr_presses(nr_presses));
endmodule // PressCountSim
