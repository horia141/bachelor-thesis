module BlockFallFPGA(clock,reset,leds,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire        clock;
   input wire        reset;
   
   output wire [7:0] leds;
   output wire       vga_hsync;
   output wire       vga_vsync;
   output wire       vga_r;
   output wire       vga_g;
   output wire       vga_b;

   wire              cm_locked;
   wire              cm_clock0;
   wire              cm_clock180;

   ClockManager
   cm (.clock(clock),
       .reset(reset),
       
       .locked(cm_locked),
       .clock0(cm_clock0),
       .clock180(cm_clock180));

   BlockFall
   bf (.clock0(cm_clock0),
       .clock180(cm_clock180),
       .reset(reset & cm_locked),

       .vga_hsync(vga_hsync),
       .vga_vsync(vga_vsync),
       .vga_r(vga_r),
       .vga_g(vga_g),
       .vga_b(vga_b));
endmodule // BlockFallFPGA
