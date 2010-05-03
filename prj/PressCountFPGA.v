module PressCountFPGA(clock,reset,countu,countd,nr_presses);
   input wire        clock;
   input wire 	     reset;

   input wire 	     countu;
   input wire 	     countd;

   output wire [7:0] nr_presses;

   PressCount
   presscount (.clock(clock),
	       .reset(reset),

	       .countu(countu),
	       .countd(countd),

	       .nr_presses(nr_presses));
endmodule // PressCountFPGA
