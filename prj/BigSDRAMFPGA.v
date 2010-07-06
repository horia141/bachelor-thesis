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

   wire 	      cm1_locked;
   wire 	      cm1_clock0;
   wire 	      cm1_clock180;
   wire 	      cm1_clock270;
   wire 	      cm1_clock2x0;

   wire 	      cm2_locked;
   wire 	      cm2_clock0;
   wire 	      cm2_clock90;
   wire 	      cm2_clock180;

   assign ddr_clk = cm2_clock0;
   assign ddr_clkn = cm2_clock180;

   ClockManager #(.ExternalFeedBack(0))
   cm1 (.clock(clock),
	.reset(reset),

	.locked(cm1_locked),
	.clock0(cm1_clock0),
	.clock180(cm1_clock180),
	.clock270(cm1_clock270),
	.clock2x(cm1_clock2x0));

   ClockManager #(.ExternalFeedBack(1))
   cm2 (.clock(cm1_clock2x0),
	.reset(reset),

	.clock_fb(clock_fb),

	.locked(cm2_locked),
	.clock0(cm2_clock0),
	.clock90(cm2_clock90),
	.clock180(cm2_clock180));

   BigSDRAM
   bigsdram (.clock0(cm1_clock0),
	     .clock180(cm1_clock180),
	     .clock270(cm1_clock270),
	     .reset(reset & cm1_locked & cm2_locked),

	     .leds(leds),
	     .ddr_clock0(cm2_clock0),
	     .ddr_clock90(cm2_clock90),
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
