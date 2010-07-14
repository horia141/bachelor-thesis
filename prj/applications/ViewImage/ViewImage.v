module ViewImage(clock0,clock180,reset,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire         clock0;
   input wire         clock180;
   input wire         reset;

   output wire        vga_hsync;
   output wire        vga_vsync;
   output wire        vga_r;
   output wire        vga_g;
   output wire        vga_b;

   wire [7:0] 	      seq_next;
   wire [11:0] 	      seq_oreg;
   wire [7:0] 	      seq_oreg_wen;

   wire [19:0] 	      coderom_data_o;

   wire [4095:0]      coderomtext_data_o;

   wire [7:0] 	      alu_result;

   wire [7:0] 	      regbank_out;

   wire [3:0] 	      internalrom_data_o;

   wire [3:0] 	      imgrom_out;
   wire [15:0] 	      imgrom_rom_addr;

   Seq
   seq (.clock(clock0),
        .reset(reset),

        .inst(coderom_data_o),
        .inst_text(coderomtext_data_o),
        .inst_en(1),
        .ireg_0(alu_result),
        .ireg_1(regbank_out),
        .ireg_2(imgrom_out),
        .ireg_3(8'h00),

        .next(seq_next),
        .oreg(seq_oreg),
        .oreg_wen(seq_oreg_wen));

   ViewImageRom
   coderom (.addr(seq_next),
            .data_o(coderom_data_o));

`ifdef SIM
   ViewImageRomText
   coderomtext (.addr(seq_next),
                .data_o(coderomtext_data_o));
`endif

   Alu
   alu (.clock(clock180),
        .reset(reset),

        .inst(seq_oreg),
        .inst_en(seq_oreg_wen[0]),

        .result(alu_result));

   RegBankS8
   regbank (.clock(clock180),
	    .reset(reset),

	    .inst(seq_oreg),
	    .inst_en(seq_oreg_wen[1]),

	    .out(regbank_out));

   ViewImageImgROM
   internalrom (.clock(clock0),
		.reset(reset),

		.we(0),
		.addr(imgrom_rom_addr),
		.data_i(4'b0000),
		.data_o(internalrom_data_o));

   ROMMatrix #(.ROMRows(128),
	       .ROMCols(128),
	       .ROMDataSize(4))
   imgrom (.clock(clock180),
	   .reset(reset),

	   .inst(seq_oreg),
	   .inst_en(seq_oreg_wen[2]),

	   .out(imgrom_out),

	   .rom_addr(imgrom_rom_addr),
	   .rom_data_o(internalrom_data_o));

   VGA2
   vga (.clock(clock180),
        .reset(reset),
        
        .inst(seq_oreg),
        .inst_en(seq_oreg_wen[3]),

        .vga_hsync(vga_hsync),
        .vga_vsync(vga_vsync),
        .vga_r(vga_r),
        .vga_g(vga_g),
        .vga_b(vga_b));
endmodule // ViewImage
