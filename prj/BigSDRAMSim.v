`timescale 1ns/1ps

module BigSDRAMSim;
   reg         clock0;
   reg 	       clock90;
   reg 	       clock180;
   reg 	       clock270;
   reg 	       reset;

   reg 	       count;

   wire [7:0]  leds;
   wire        ddr_cke;
   wire        ddr_csn;
   wire        ddr_rasn;
   wire        ddr_casn;
   wire        ddr_wen;
   wire [1:0]  ddr_ba;
   wire [12:0] ddr_addr;
   wire [1:0]  ddr_dm;
   wire [15:0] ddr_dq;
   wire [1:0]  ddr_dqs;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #500000 $finish;
   end

   initial begin
      #0 clock0 = 1;
      forever #10 clock0 = ~clock0;
   end

   initial begin
      #5 clock90 = 1;
      forever #10 clock90 = ~clock90;
   end

   initial begin
      #10 clock180 = 1;
      forever #10 clock180 = ~clock180;
   end

   initial begin
      #15 clock270 = 1;
      forever #10 clock270 = ~clock270;
   end

   initial begin
      #0 reset = 0;
      #60 reset = 1;
      #40 reset = 0;
   end

   initial begin
      #0 count = 0;
      #205180 count = 1;
      #202000 count = 0;
   end

   Ddr
   ddr (.Clk(clock180),
	.Clk_n(clock0),
	
	.Cke(ddr_cke),
	.Cs_n(ddr_csn),
	.Ras_n(ddr_rasn),
	.Cas_n(ddr_casn),
	.We_n(ddr_wen),
	.Ba(ddr_ba),
	.Addr(ddr_addr),
	.Dm(ddr_dm),
	.Dq(ddr_dq),
	.Dqs(ddr_dqs));

   BigSDRAM
   bigsdram (.clock0(clock0),
	     .clock90(clock90),
	     .clock180(clock180),
	     .clock270(clock270),
	     .reset(reset),

	     .count(count),

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
endmodule // BigSDRAMSim
