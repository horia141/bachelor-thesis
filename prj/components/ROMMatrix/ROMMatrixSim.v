`timescale 1ns/10ps

module ROMMatrixSim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;

   wire        out;

   wire        testrom_data_o;

   wire [15:0] rommatrix_rom_addr;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #1000 $finish;
   end

   initial begin
      #0 clock = 1;
      forever #2 clock = ~clock;
   end

   initial begin
      #0 reset = 0;
      #1 reset = 1;
      #4 reset = 0;
   end

   initial begin
      #0.1 inst_en = 0;

      // Test each instruction.

      #8 inst = {`ROMMatrix_LDR,8'h02};
      inst_en = 1;

      #4 inst = {`ROMMatrix_LDC,8'h02};
      inst_en = 1;

      #4 inst = {`ROMMatrix_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`ROMMatrix_LDR,8'h0A};
      inst_en = 0;

      #4 inst = {`ROMMatrix_LDC,8'h01};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'h10};
      inst_en = 1;

      #4 inst = {`ROMMatrix_LDR,8'h0B};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`ROMMatrix_LDR,8'h03};
      inst_en = 1;

      #4 inst = {`ROMMatrix_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test reading from improper address.
      #4 inst = {`ROMMatrix_LDR,8'h32};
      inst_en = 1;

      #4 inst = {`ROMMatrix_LDC,8'h34};
      inst_en = 1;

      #4 inst = {`ROMMatrix_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end

   ROMMatrixTestROM
   testrom (.addr(rommatrix_rom_addr),
	    .data_o(testrom_data_o));

   ROMMatrix #(.ROMRows(16),
	       .ROMCols(4),
	       .ROMDataSize(1))
   rommatrix (.clock(clock),
	      .reset(reset),

	      .inst(inst),
	      .inst_en(inst_en),

	      .out(out),

	      .rom_addr(rommatrix_rom_addr),
	      .rom_data_o(testrom_data_o));
endmodule // ROMMatrixSim
