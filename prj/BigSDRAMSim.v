`timescale 1ns/1ps

module BigSDRAMSim;
   reg         clock0;
   reg 	       clock90;
   reg 	       clock180;
   reg 	       clock270;
   reg 	       reset;

   wire        bigsdram_leds;
   wire        bigsdram_ddr_cke;
   wire        bigsdram_ddr_csn;
   wire        bigsdram_ddr_rasn;
   wire        bigsdram_ddr_casn;
   wire        bigsdram_ddr_wen;
   wire [1:0]  bigsdram_ddr_ba;
   wire [12:0] bigsdram_ddr_addr;
   wire [1:0]  bigsdram_ddr_dm;
   wire [15:0] bigsdram_ddr_dq;
   wire [1:0]  bigsdram_ddr_dqs;

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

   Ddr
   ddr (.Clk(clock180),
	.Clk_n(clock0),
	
	.Cke(bigsdram_ddr_cke),
	.Cs_n(bigsdram_ddr_csn),
	.Ras_n(bigsdram_ddr_rasn),
	.Cas_n(bigsdram_ddr_casn),
	.We_n(bigsdram_ddr_wen),
	.Ba(bigsdram_ddr_ba),
	.Addr(bigsdram_ddr_addr),
	.Dm(bigsdram_ddr_dm),
	.Dq(bigsdram_ddr_dq),
	.Dqs(bigsdram_ddr_dqs));

   BigSDRAM
   bigsdram (.clock0(clock0),
	     .clock90(clock90),
	     .clock180(clock180),
	     .clock270(clock270),
	     .reset(reset),

	     .leds(bigsdram_leds),
	     .ddr_cke(bigsdram_ddr_cke),
	     .ddr_csn(bigsdram_ddr_csn),
	     .ddr_rasn(bigsdram_ddr_rasn),
	     .ddr_casn(bigsdram_ddr_casn),
	     .ddr_wen(bigsdram_ddr_wen),
	     .ddr_ba(bigsdram_ddr_ba),
	     .ddr_addr(bigsdram_ddr_addr),
	     .ddr_dm(bigsdram_ddr_dm),
	     .ddr_dq(bigsdram_ddr_dq),
	     .ddr_dqs(bigsdram_ddr_dqs));
endmodule // BigSDRAMSim
