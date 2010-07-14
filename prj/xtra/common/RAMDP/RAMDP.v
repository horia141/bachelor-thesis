module RAMDP(clock,reset,we,addr0,addr1,data_i,data_o0,data_o1);
   parameter                  AddrSize = 8;
   parameter                  DataSize = 8;
   
   input wire                 clock;
   input wire                 reset;

   input wire                 we;
   input wire [AddrSize-1:0]  addr0;
   input wire [AddrSize-1:0]  addr1;
   input wire [DataSize-1:0]  data_i;


`ifdef SIM
   output reg [DataSize-1:0]  data_o0;
   output reg [DataSize-1:0]  data_o1;

   reg [DataSize-1:0]         s_Data[2**AddrSize-1:0];
   reg [AddrSize:0]           k;
   
   initial begin
      for (k = 0; k < 2**AddrSize; k = k + 1) begin
         s_Data[k] = 0;
      end
   end

   always @ (posedge clock) begin
      if (reset) begin
         data_o0 <= 0;
         data_o1 <= 0;
      end
      else begin
         if (we) begin
            s_Data[addr0] <= data_i;
         end

         data_o0 <= s_Data[addr0];
         data_o1 <= s_Data[addr1];
      end
   end // always @ (posedge clock)
`endif //  `ifdef SIM

`ifdef FPGA
   output wire [DataSize-1:0]  data_o0;
   output wire [DataSize-1:0]  data_o1;

   RAMB16_S1_S1
   ramse (.CLKA(clock),
	  .CLKB(clock),
	  .SSRA(reset),
	  .SSRB(reset),
	  
	  .ENA(~reset),
	  .WEA(we),
	  .ADDRA(addr0),
	  .DIA(data_i),
	  .DOA(data_o0),

	  .ENB(~reset),
	  .WEB(0),
	  .ADDRB(addr1),
	  .DIB(1'b0),
	  .DOB(data_o1));
`endif
endmodule // RAMDP
