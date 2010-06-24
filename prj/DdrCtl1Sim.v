`timescale 1ns/1ps

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

   wire        ddrctl_ddr_cke;
   wire        ddrctl_ddr_csn;
   wire        ddrctl_ddr_rasn;
   wire        ddrctl_ddr_casn;
   wire        ddrctl_ddr_wen;
   wire [1:0]  ddrctl_ddr_ba;
   wire [12:0] ddrctl_ddr_addr;
   wire [1:0]  ddrctl_ddr_dm;
   wire [15:0] ddrctl_ddr_dq;
   wire [1:0]  ddrctl_ddr_dqs;

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
      #0.1 inst_en = 0;

      // Test each instruction.
      #204455 inst = {`DdrCtl1_LA0,8'h12};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LA1,8'h3F};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LA2,8'h2B};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LA3,8'h00};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD0,8'hAA};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD1,8'hBB};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD2,8'hCC};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD3,8'hDD};
      inst_en = 1;

      #20 inst = {`DdrCtl1_WRP,8'bxxxxxxxx};
      inst_en = 1;

      #20 inst = {`DdrCtl1_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #120 inst = {`DdrCtl1_LD0,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD1,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD2,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD3,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_RDP,8'bxxxxxxxx};
      inst_en = 1;

      #20 inst = {`DdrCtl1_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #140 inst = {`DdrCtl1_LD1,8'h0A};
      inst_en = 0;

      #20 inst = {`DdrCtl1_LD1,8'h01};
      inst_en = 1;

      // Test bad instruction.
//       #20 inst = {8'hF,8'h10};
//       inst_en = 1;

//       #20 inst = {`DdrCtl1_LD2,8'hA0};
//       inst_en = 1;

//       #40 reset = 1;

//       #80 reset = 0;

//       #20 inst = {`DdrCtl1_LD2,8'hAB};
//       inst_en = 1;

//       #20 inst = {`DdrCtl1_NOP,8'bxxxxxxxx};
//       inst_en = 1;

      // Test writing to another bank.
      #20 inst = {`DdrCtl1_LA0,8'h12};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LA1,8'h3F};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LA2,8'h2B};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LA3,8'h01};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD0,8'hEE};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD1,8'hFF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD2,8'h11};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD3,8'h22};
      inst_en = 1;

      #20 inst = {`DdrCtl1_WRP,8'bxxxxxxxx};
      inst_en = 1;

      #20 inst = {`DdrCtl1_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #120 inst = {`DdrCtl1_LD0,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD1,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD2,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_LD3,8'hEF};
      inst_en = 1;

      #20 inst = {`DdrCtl1_RDP,8'bxxxxxxxx};
      inst_en = 1;

      #20 inst = {`DdrCtl1_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #20 inst_en = 0;
   end // initial begin

   Ddr
   ddr (.Clk(clock90),
	.Clk_n(clock270),
	
	.Cke(ddrctl_ddr_cke),
	.Cs_n(ddrctl_ddr_csn),
	.Ras_n(ddrctl_ddr_rasn),
	.Cas_n(ddrctl_ddr_casn),
	.We_n(ddrctl_ddr_wen),
	.Ba(ddrctl_ddr_ba),
	.Addr(ddrctl_ddr_addr),
	.Dm(ddrctl_ddr_dm),
	.Dq(ddrctl_ddr_dq),
	.Dqs(ddrctl_ddr_dqs));

   DdrCtl1
   ddrctl (.clock0(clock0),
	   .clock90(clock90),
	   .reset(reset),

	   .inst(inst),
	   .inst_en(inst_en),

	   .page(page),
	   .ready(ready),

	   .ddr_cke(ddrctl_ddr_cke),
	   .ddr_csn(ddrctl_ddr_csn),
	   .ddr_rasn(ddrctl_ddr_rasn),
	   .ddr_casn(ddrctl_ddr_casn),
	   .ddr_wen(ddrctl_ddr_wen),
	   .ddr_ba(ddrctl_ddr_ba),
	   .ddr_addr(ddrctl_ddr_addr),
	   .ddr_dm(ddrctl_ddr_dm),
	   .ddr_dq(ddrctl_ddr_dq),
	   .ddr_dqs(ddrctl_ddr_dqs));
endmodule // DdrCtl1Sim
