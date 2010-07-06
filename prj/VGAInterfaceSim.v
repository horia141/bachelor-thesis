`timescale 1ns/10ps

module VGAInterfaceSim;
   reg         clock;
   reg 	       reset;

   reg 	       color_r;
   reg 	       color_g;
   reg 	       color_b;

   wire [10:0] fb_addr_h;
   wire [10:0] fb_addr_v;

   wire        vga_hsync;
   wire        vga_vsync;
   wire        vga_r;
   wire        vga_g;
   wire        vga_b;

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
      #0 color_r = 0;
      color_g = 0;
      color_b = 0;
      
      #40 color_r = 1;
      color_g = 0;
      color_b = 0;

      #40 color_r = 1;
      color_g = 1;
      color_b = 0;

      #40 color_r = 0;
      color_g = 1;
      color_b = 0;

      #40 color_r = 0;
      color_g = 0;
      color_b = 0;

      #40 color_r = 0;
      color_g = 0;
      color_b = 1;
   end // initial begin

   VGAInterface #(.HAddrSize(11),
		  .HVisibleArea(4),
		  .HFrontPorch(2),
		  .HSyncPulse(3),
		  .HBackPorch(2),
		  .VAddrSize(11),
		  .VVisibleArea(5),
		  .VFrontPorch(2),
		  .VSyncPulse(3),
		  .VBackPorch(2))
   vgaint (.clock(clock),
	   .reset(reset),

	   .color_r(color_r),
	   .color_g(color_g),
	   .color_b(color_b),

	   .fb_addr_h(fb_addr_h),
	   .fb_addr_v(fb_addr_v),

	   .vga_hsync(vga_hsync),
	   .vga_vsync(vga_vsync),
	   .vga_r(vga_r),
	   .vga_g(vga_g),
	   .vga_b(vga_b));
endmodule // VGAInterfaceSim
