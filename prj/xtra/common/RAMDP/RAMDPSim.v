`timescale 1ns/10ps

module RAMDPSim;
   reg        clock;
   reg        reset;

   reg        we;
   reg [3:0]  addr0;
   reg [3:0]  addr1;
   reg [7:0]  data_i;

   wire [7:0] data_o0;
   wire [7:0] data_o1;

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
      #0.1 we = 0;

      #8 we = 1;
      addr0 = 4'hA;
      data_i = 8'hBB;
      addr1 = 4'hA;
      
      #4 we = 1;
      addr0 = 4'h2;
      data_i = 8'hAA;
      addr1 = 4'h2;

      #4 we = 1;
      addr0 = 4'h7;
      data_i = 8'hEF;
      addr1 = 4'hA;

      #4 we = 1;
      addr0 = 4'h1;
      data_i = 8'hAE;
      addr1 = 4'h2;

      #4 we = 0;
      addr0 = 4'hA;
      addr1 = 4'h1;

      #4 we = 0;
      addr0 = 4'h2;
      addr1 = 4'h7;

      #4 we = 0;
      addr0 = 4'h7;
      addr1 = 4'h2;

      #4 we = 0;
      addr0 = 4'h1;
      addr1 = 4'hA;
   end // initial begin

   RAMDP #(.AddrSize(4),
           .DataSize(8))
   ramdp (.clock(clock),
          .reset(reset),

          .we(we),
          .addr0(addr0),
          .addr1(addr1),
          .data_i(data_i),

          .data_o0(data_o0),
          .data_o1(data_o1));
endmodule // RAMDPSim
