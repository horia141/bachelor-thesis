module Auto1FPGA(clock,reset,leds);
   input wire        clock;
   input wire        reset;
   output wire [7:0] leds;

   wire 	     slowc_outclock;

   wire [23:0] 	     auto1_counter;

   assign leds = auto1_counter[7:0];

   SlowClock #(.SlowFactor(32'h00FFFFFF))
   slowc (.clock(clock),
	  .reset(reset),
	  .outclock(slowc_outclock));

   Auto1
   auto1 (.clock(slowc_outclock),
	  .reset(reset),
	  .counter(auto1_counter));
endmodule // Auto1FPGA
