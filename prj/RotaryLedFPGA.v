module RotaryLedFPGA(clock,reset,rotary,leds);
   input wire        clock;
   input wire 	     reset;

   input wire [1:0]  rotary;

   output wire [7:0] leds;

   RotaryLed
   rotaryled (.clock(clock),
	      .reset(reset),

	      .rotary(~rotary),

	      .leds(leds));
endmodule // RotaryLedFPGA
