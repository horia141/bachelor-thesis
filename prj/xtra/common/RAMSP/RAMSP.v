module RAMSP(clock,reset,we,addr,data_i,data_o);
   parameter                  AddrSize = 8;
   parameter                  DataSize = 8;
   
   input wire                 clock;
   input wire                 reset;

   input wire                 we;
   input wire [AddrSize-1:0]  addr;
   input wire [DataSize-1:0]  data_i;

   output reg [DataSize-1:0]  data_o;

   reg [DataSize-1:0]         s_Data[2**AddrSize-1:0];

`ifdef SIM
   reg [AddrSize:0]           k;
              
   initial begin
      for (k = 0; k < 2**AddrSize; k = k + 1) begin
         s_Data[k] = 0;
      end
   end
`endif

   always @ (posedge clock) begin
      if (reset) begin
         data_o <= 0;
      end
      else begin
         if (we) begin
            s_Data[addr] <= data_i;
         end

         data_o <= s_Data[addr];
      end
   end // always @ (posedge clock)
endmodule // RAMSP
