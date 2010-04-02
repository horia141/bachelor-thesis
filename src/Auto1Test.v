`timescale 1ns/10ps

module Auto1Test;
   reg         clock;
   reg 	       reset;

   wire [23:0] counter;

   initial begin
      #0 $dumpfile("out/Auto1Test.vcd");
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

   Auto1 #()
   auto1 (.clock(clock),
	  .reset(reset),

	  .counter(counter));
endmodule
