module VGA2InterfaceFPGA(clock,reset,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire  clock;
   input wire  reset;

   output wire vga_hsync;
   output wire vga_vsync;
   output wire vga_r;
   output wire vga_g;
   output wire vga_b;

   VGA2Interface #(.HAddrSize(11),
                   .HVisibleArea(800),
                   .HFrontPorch(56),
                   .HSyncPulse(120),
                   .HBackPorch(64),
                   .VAddrSize(11),
                   .VVisibleArea(600),
                   .VFrontPorch(37),
                   .VSyncPulse(6),
                   .VBackPorch(23))
   vgaint (.clock(clock),
           .reset(reset),

           .color_r(1),
           .color_g(1),
           .color_b(0),

           .vga_hsync(vga_hsync),
           .vga_vsync(vga_vsync),
           .vga_r(vga_r),
           .vga_g(vga_g),
           .vga_b(vga_b));
endmodule // VGA2InterfaceFPGA
