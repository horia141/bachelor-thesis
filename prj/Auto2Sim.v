`timescale 1ns/10ps

module Auto2Sim;
   reg         clock0;
   reg 	       clock180;
   reg 	       reset;

   wire [7:0]  leds;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #3000 $finish;
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

   Auto2
   auto2 (.clock0(clock0),
	  .clock180(clock180),
	  .reset(reset),

	  .leds(leds));
endmodule // Auto2Sim
