module ClockManagerFPGA(clock,reset,leds);
   input wire        clock;
   input wire        reset;

   output wire [2:0] leds;

   wire 	     cm_locked;
   wire 	     cm_clock0;
   wire 	     cm_clock2x;

   assign leds[2] = cm_locked;

   ClockManager
   cm (.clock(clock),
       .reset(reset),

       .locked(cm_locked),
       .clock0(cm_clock0),
       .clock2x(cm_clock2x));

   SlowClock #(.SlowFactor(32'h00FFFFFF))
   sc0 (.clock(cm_clock0),
	.reset(reset & cm_locked),

	.outclock(leds[1]));

   SlowClock #(.SlowFactor(32'h00FFFFFF))
   sc1 (.clock(cm_clock2x),
	.reset(reset & cm_locked),

	.outclock(leds[0]));
endmodule // ClockManagerFPGA
