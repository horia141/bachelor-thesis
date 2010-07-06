`timescale 1ns/10ps

module SlowClockSim;
   reg  clock;
   reg  reset;

   wire outclock;

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

   SlowClock #(.SlowFactor(8))
   slowclock (.clock(clock),
              .reset(reset),

              .outclock(outclock));
endmodule // SlowClockSim
