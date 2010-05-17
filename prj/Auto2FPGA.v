module Auto2FPGA(clock,reset,leds);
   input wire        clock;
   input wire        reset;
   output wire [7:0] leds;

   wire 	     slowc_outclock;

   wire [23:0] 	     auto2_counter;

   assign leds = auto2_counter[7:0];

   SlowClock #(.SlowFactor(32'h00FFFFFF))
   slowc (.clock(clock),
	  .reset(reset),

	  .outclock(slowc_outclock));

   Auto2
   auto2 (.clock0(slowc_outclock),
	  .clock180(~slowc_outclock),
	  .reset(reset),

	  .counter(auto2_counter));
endmodule // Auto2FPGA
