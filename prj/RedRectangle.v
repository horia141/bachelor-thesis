module RedRectangle(clock0,clock180,reset,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire    clock0;
   input wire 	 clock180;
   input wire 	 reset;

   output wire 	 vga_hsync;
   output wire 	 vga_vsync;
   output wire 	 vga_r;
   output wire 	 vga_g;
   output wire 	 vga_b;

   wire [7:0] 	 seq_next;
   wire [11:0] 	 seq_oreg;
   wire [7:0] 	 seq_oreg_wen;

   wire [19:0] 	 coderom_data_o;

   wire [4095:0] coderomtext_data_o;

   wire [7:0] 	 alu_result;

   wire [7:0] 	 regbank_out_0;
   wire [7:0] 	 regbank_out_1;

   Seq
   seq (.clock(clock0),
	.reset(reset),

	.inst(coderom_data_o),
	.inst_text(coderomtext_data_o),
	.inst_en(1),
	.ireg_0(alu_result),
	.ireg_1(regbank_out_0),
	.ireg_2(regbank_out_1),
	.ireg_3(8'h00),

	.next(seq_next),
	.oreg(seq_oreg),
	.oreg_wen(seq_oreg_wen));

   RedRectangleRom
   coderom (.addr(seq_next),
	    .data_o(coderom_data_o));

`ifdef SIM
   RedRectangleRomText
   coderomtext (.addr(seq_next),
                .data_o(coderomtext_data_o));
`endif
   
   Alu
   alu (.clock(clock180),
        .reset(reset),

        .inst(seq_oreg),
        .inst_en(seq_oreg_wen[0]),

        .result(alu_result));

   RegBankP2
   regbank (.clock(clock180),
	    .reset(reset),

	    .inst(seq_oreg),
	    .inst_en(seq_oreg_wen[1]),

	    .out_0(regbank_out_0),
	    .out_1(regbank_out_1));

   VGA2
   vga (.clock(clock180),
	.reset(reset),
	
	.inst(seq_oreg),
	.inst_en(seq_oreg_wen[2]),

	.vga_hsync(vga_hsync),
	.vga_vsync(vga_vsync),
	.vga_r(vga_r),
	.vga_g(vga_g),
	.vga_b(vga_b));
endmodule // RedRectangle

