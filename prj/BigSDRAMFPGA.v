module BigSDRAMFPGA(clock,reset,clock_fb,leds,ddr_clk,ddr_clkn,ddr_cke,ddr_csn,ddr_rasn,ddr_casn,ddr_wen,ddr_ba,ddr_addr,ddr_dm,ddr_dq,ddr_dqs);
   input wire         clock;
   input wire 	      reset;

   input wire 	      clock_fb;

   output wire [7:0]  leds;
   output wire 	      ddr_clk;
   output wire 	      ddr_clkn;
   output wire 	      ddr_cke;
   output wire 	      ddr_csn;
   output wire 	      ddr_rasn;
   output wire 	      ddr_casn;
   output wire 	      ddr_wen;
   output wire [1:0]  ddr_ba;
   output wire [12:0] ddr_addr;
   output wire [1:0]  ddr_dm;
   inout wire [15:0]  ddr_dq;
   inout wire [1:0]   ddr_dqs;

   wire 	      cm_locked;
   wire 	      cm_clock0;
   wire 	      cm_clock180;
   wire 	      cm_clock270;
   wire 	      cm_clock2x;
   wire 	      cm_clock2x180;

   assign ddr_clk = cm_clock2x;
   assign ddr_clkn = cm_clock2x180;

   ClockManager #(.ExtFeedBack(1))
   cm (.clock(clock),
       .reset(reset),

       .clock_fb(clock_fb),

       .locked(cm_locked),
       .clock0(cm_clock0),
       .clock180(cm_clock180),
       .clock270(cm_clock270),
       .clock2x(cm_clock2x),
       .clock2x180(cm_clock2x180));

   BigSDRAM
   bigsdram (.clock0(cm_clock0),
	     .clock180(cm_clock180),
	     .clock270(cm_clock270),
	     .reset(reset & cm_locked),

	     .leds(leds),
	     .ddr_cke(ddr_cke),
	     .ddr_csn(ddr_csn),
	     .ddr_rasn(ddr_rasn),
	     .ddr_casn(ddr_casn),
	     .ddr_wen(ddr_wen),
	     .ddr_ba(ddr_ba),
	     .ddr_addr(ddr_addr),
	     .ddr_dm(ddr_dm),
	     .ddr_dq(ddr_dq),
	     .ddr_dqs(ddr_dqs));
endmodule // BigSDRAMFPGA
