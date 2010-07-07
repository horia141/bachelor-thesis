`timescale 1ns/10ps

module RAMSPSim;
   reg        clock;
   reg        reset;

   reg        we;
   reg [3:0]  addr;
   reg [7:0]  data_i;

   wire [7:0] data_o;

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
      addr = 4'h2;
      data_i = 8'hAF;

      #4 we = 0;

      #4 we = 1;
      addr = 4'h4;
      data_i = 8'hBE;

      #4 we = 0;
      addr = 4'h2;

      #4 we = 0;
      addr = 4'h4;

      #4 we = 1;
      addr = 4'hF;
      data_i = 8'h2C;

      #4 we = 0;

      #4 we = 0;
      addr = 4'h2;

      #4 we = 0;
      addr = 4'h4;

      #4 we = 0;
      addr = 4'hF;
   end // initial begin

   RAMSP #(.AddrSize(4),
           .DataSize(8))
   ramsp (.clock(clock),
          .reset(reset),

          .we(we),
          .addr(addr),
          .data_i(data_i),

          .data_o(data_o));
endmodule // RAMSPSim
