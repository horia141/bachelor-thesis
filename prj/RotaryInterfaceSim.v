`timescale 1ns/10ps

module RotaryInterfaceSim;
   reg       clock;
   reg 	     reset;

   reg [1:0] rotary;

   wire      rotary_left;
   wire      rotary_right;

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

   initial begin
      #0 rotary = 2'b00;

      #10 rotary = 2'b10;

      #4 rotary = 2'b11;

      #4 rotary = 2'b00;

      #20 rotary = 2'b01;

      #40 rotary = 2'b00;

      #20 rotary = 2'b01;

      #20 rotary = 2'b10;
   end

   RotaryInterface
   rotaryint (.clock(clock),
	      .reset(reset),

	      .rotary(rotary),

	      .rotary_left(rotary_left),
	      .rotary_right(rotary_right));
endmodule // RotaryInterfaceSim
