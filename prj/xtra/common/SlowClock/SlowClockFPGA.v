module SlowClockFPGA(clock,reset,led);
   input wire  clock;
   input wire  reset;
   output wire led;

   SlowClock #(.SlowFactor(32'h00FFFFFF))
   slowc (.clock(clock),
          .reset(reset),
          
          .outclock(led));
endmodule // SlowClockFPGA
