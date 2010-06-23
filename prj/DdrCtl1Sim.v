`timescale 1ns/10ps

module DdrCtl1Sim;
   reg         clock0;
   reg 	       clock90;
   reg 	       clock180;
   reg 	       clock270;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;

   wire [31:0] page;
   wire        ready;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #200000 $finish;
   end

   initial begin
      #0 clock0 = 1;
      forever #20 clock0 = ~clock0;
   end

   initial begin
      #5 clock90 = 1;
      forever #20 clock90 = ~clock90;
   end

   initial begin
      #10 clock180 = 1;
      forever #20 clock180 = ~clock180;
   end

   initial begin
      #15 clock270 = 1;
      forever #20 clock270 = ~clock270;
   end

   initial begin
      #0 reset = 0;
      #60 reset = 1;
      #40 reset = 0;
   end

   DdrCtl1
   ddrctl (.clock0(clock0),
	   .clock90(clock90),
	   .clock180(clock180),
	   .clock270(clock270),
	   .reset(reset),

	   .inst(inst),
	   .inst_en(inst_en),

	   .page(page),
	   .ready(ready));
endmodule // DdrCtl1Sim
