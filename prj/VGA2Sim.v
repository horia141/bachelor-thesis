`timescale 1ns/10ps

module VGA2Sim;
   reg        clock;
   reg        reset;

   reg [11:0] inst;
   reg        inst_en;

   wire       vga_hsync;
   wire       vga_vsync;
   wire       vga_r;
   wire       vga_g;
   wire       vga_b;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #100000 $finish;
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
      #8 inst = {`VGA2_LDR,8'h00};
      inst_en = 1;

      #4 inst = {`VGA2_LDC,8'h20};
      inst_en = 1;

      #4 inst = {`VGA2_LDD,5'bxxxxx,3'b100};
      inst_en = 1;

      #4 inst = {`VGA2_LDI,5'bxxxxx,3'b010};
      inst_en = 1;

      #4 inst = {`VGA2_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`VGA2_LDD,5'bxxxxx,3'b010};
      inst_en = 0;

      #4 inst = {`VGA2_LDR,8'h04};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,16'hBEEF};
      inst_en = 1;

      #4 inst = {`VGA2_LDC,8'h3F};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #8 inst = {`VGA2_LDR,8'h00};
      inst_en = 1;

      #4 inst = {`VGA2_LDC,8'h20};
      inst_en = 1;

      #4 inst = {`VGA2_LDD,5'bxxxxx,3'b100};
      inst_en = 1;

      #4 inst = {`VGA2_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test writing to another row.
      #8 inst = {`VGA2_LDR,8'h01};
      inst_en = 1;

      #4 inst = {`VGA2_LDC,8'h04};
      inst_en = 1;

      #4 inst = {`VGA2_LDD,5'bxxxxx,3'b010};
      inst_en = 1;

      #4 inst = {`VGA2_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end // initial begin

   VGA2
   vga (.clock(clock),
	.reset(reset),

	.inst(inst),
	.inst_en(inst_en),

	.vga_hsync(vga_hsync),
	.vga_vsync(vga_vsync),
	.vga_r(vga_r),
	.vga_g(vga_g),
	.vga_b(vga_b));
endmodule // VGA2Sim
