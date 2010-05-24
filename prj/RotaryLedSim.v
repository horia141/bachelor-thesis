`timescale 1ns/10ps

module RotaryLedSim;
   reg         clock0;
   reg 	       clock180;
   reg 	       reset;

   reg [1:0]   rotary;
 	       
   wire [7:0]  leds;

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
      #10 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;

      #40 rotary = 2'b01;

      #40 rotary = 2'b11;

      #40 rotary = 2'b00;
   end

   RotaryLed
   rotaryled (.clock0(clock0),
	      .clock180(clock180),
	      .reset(reset),

	      .rotary(rotary),

	      .leds(leds));
endmodule // ProtaryLedSim
