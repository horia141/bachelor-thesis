`timescale 1ns/10ps

module PushBtnInterfaceSim;
   reg  clock;
   reg  reset;

   reg  button;

   wire button_pressed;

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
      #0 button = 0;
      #7 button = 1;
      #6 button = 0;

      #9 button = 1;
      #50 button = 0;

      #5 button = 1;
      #100 button = 0;

      #5 button = 1;
      #4 button = 0;

      #4 button = 1;
      #4 button = 0;

      #4 button = 1;
      #4 button = 0;

      #5 button = 1;
      #7 button = 0;
   end

   PushBtnInterface
   pushbtnint (.clock(clock),
               .reset(reset),

               .button(button),

               .button_pressed(button_pressed));
endmodule // PushBtnInterfaceSim
