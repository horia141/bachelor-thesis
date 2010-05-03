module PressCountFPGA(clock,reset,count,nr_presses);
   input wire        clock;
   input wire 	     reset;

   input wire 	     count;

   output wire [7:0] nr_presses;

   PressCount
   presscount (.clock(clock),
	       .reset(reset),

	       .count(count),

	       .nr_presses(nr_presses));
endmodule // PressCountFPGA
