`timescale 1ns/10ps

module ViewImageSim;
   reg         clock0;
   reg         clock180;
   reg         reset;

   wire        vga_hsync;
   wire        vga_vsync;
   wire        vga_r;
   wire        vga_g;
   wire        vga_b;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #80000 $finish;
   end

   initial begin
      #0 clock0 = 1;
      forever #2 clock0 = ~clock0;
   end

   initial begin
      #0 clock180 = 0;
      forever #2 clock180 = ~clock180;
   end

   initial begin
      #0 reset = 0;
      #1 reset = 1;
      #4 reset = 0;
   end

   ViewImage
   view (.clock0(clock0),
	 .clock180(clock180),
	 .reset(reset),

	 .vga_hsync(vga_hsync),
	 .vga_vsync(vga_vsync),
	 .vga_r(vga_r),
	 .vga_g(vga_g),
	 .vga_b(vga_b));
endmodule // ViewImageSim
