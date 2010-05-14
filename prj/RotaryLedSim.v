`timescale 1ns/10ps

module RotaryLedSim;
   reg         clock;
   reg 	       reset;

   reg [1:0]   rotary;
 	       
   wire [7:0]  leds;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #5000 $finish;
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
      #7 rotary = 2'b01;

      #8 rotary = 2'b11;
      
      #8 rotary = 2'b00;

      #6 rotary = 2'b01;

      #8 rotary = 2'b11;

      #13 rotary = 2'b00;

      #43 rotary = 2'b10;

      #16 rotary = 2'b11;
      
      #11 rotary = 2'b00;
   end

   RotaryLed
   rotaryled (.clock(clock),
	       .reset(reset),

	       .rotary(rotary),

	       .leds(leds));
endmodule // ProtaryLedSim
