module BigSDRAM(clock0,clock180,clock270,reset,leds,ddr_cke,ddr_csn,ddr_rasn,ddr_casn,ddr_wen,ddr_ba,ddr_addr,ddr_dm,ddr_dq,ddr_dqs);
   input wire         clock0;
   input wire 	      clock180;
   input wire 	      clock270;
   input wire 	      reset;

   output wire [7:0]  leds;
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

   wire [7:0] 	      seq_next;
   wire [11:0] 	      seq_oreg;
   wire [7:0] 	      seq_oreg_wen;

   wire [19:0] 	      rom_data_o;

   wire [31:0] 	      ddrctl_page;
   wire 	      ddrctl_ready;

   wire 	      swc_ready;

   Seq
   seq (.clock(clock0),
	.reset(reset),

	.inst(rom_data_o),
	.inst_en(1),
	.ireg_0(ddrctl_page[7:0]),
	.ireg_1({7'h00,ddrctl_ready}),
	.ireg_2({7'h00,swc_ready}),
	.ireg_3(8'h00),

	.next(seq_next),
	.oreg(seq_oreg),
	.oreg_wen(seq_oreg_wen));

   BigSDRAMRom
   rom (.addr(seq_next),
	.data_o(rom_data_o));

   DdrCtl1
   ddrctl (.clock0(clock180),
	   .clock90(clock270),
	   .clock180(clock0),
	   .reset(reset),

	   .inst(seq_oreg),
	   .inst_en(seq_oreg_wen[0]),

	   .page(ddrctl_page),
	   .ready(ddrctl_ready),

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

   LedBank
   ledbank (.clock(clock180),
	    .reset(reset),

	    .inst(seq_oreg),
	    .inst_en(seq_oreg_wen[1]),
   
	    .leds(leds));

   Swc
   swc (.clock(clock180),
	.reset(reset),

	.inst(seq_oreg),
	.inst_en(seq_oreg_wen[2]),

	.ready(swc_ready));
endmodule // BigSDRAM
