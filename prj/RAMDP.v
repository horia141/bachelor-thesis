module RAMDP(clock,reset,we,addr0,addr1,data_i,data_o0,data_o1);
   parameter                  AddrSize = 8;
   parameter                  DataSize = 8;
   
   input wire                 clock;
   input wire                 reset;

   input wire                 we;
   input wire [AddrSize-1:0]  addr0;
   input wire [AddrSize-1:0]  addr1;
   input wire [DataSize-1:0]  data_i;

   output reg [DataSize-1:0]  data_o0;
   output reg [DataSize-1:0]  data_o1;

   reg [DataSize-1:0]         s_Data[2**AddrSize-1:0];

`ifdef SIM
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
   generate
      if (AddrSize <= 9 && DataSize <= 32) begin: ramS36
	 RAMB16_S36_S36 #(.WRITE_MODE(READ_FIRST))
	 ram (.CLKA(clock),
	      .CLKB(clock),
	      .SSRA(reset),
	      .SSRB(reset),
	      
	      .WEA(wen),
	      .ENA(1),
	      .ADDRA(addr0),
	      .DIA(data_i),
	      .WEB(wen),
	      .ENB(1),
	      .ADDRB(addr1),
	      .DIB({32{0}}),

	      .DOA(data_o0),
	      .DOB(data_o1));
      end // block: ramS36
      else if (AddrSize <= 10 && DataSize <= 16) begin: ramS18
	 RAMB16_S18_S18 #(.WRITE_MODE(READ_FIRST))
	 ram (.CLKA(clock),
	      .CLKB(clock),
	      .SSRA(reset),
	      .SSRB(reset),
	      
	      .WEA(wen),
	      .ENA(1),
	      .ADDRA(addr0),
	      .DIA(data_i),
	      .WEB(wen),
	      .ENB(1),
	      .ADDRB(addr1),
	      .DIB({16{0}}),

	      .DOA(data_o0),
	      .DOB(data_o1));
      end // block: ramS18
      else if (AddrSize <= 11 && DataSize <= 8) begin: ramS9
	 RAMB16_S9_S9 #(.WRITE_MODE(READ_FIRST))
	 ram (.CLKA(clock),
	      .CLKB(clock),
	      .SSRA(reset),
	      .SSRB(reset),
	      
	      .WEA(wen),
	      .ENA(1),
	      .ADDRA(addr0),
	      .DIA(data_i),
	      .WEB(wen),
	      .ENB(1),
	      .ADDRB(addr1),
	      .DIB({8{0}}),

	      .DOA(data_o0),
	      .DOB(data_o1));
      end // block: ramS9
      else if (AddrSize <= 12 && DataSize <= 4) begin: ramS4
	 RAMB16_S4_S4 #(.WRITE_MODE(READ_FIRST))
	 ram (.CLKA(clock),
	      .CLKB(clock),
	      .SSRA(reset),
	      .SSRB(reset),
	      
	      .WEA(wen),
	      .ENA(1),
	      .ADDRA(addr0),
	      .DIA(data_i),
	      .WEB(wen),
	      .ENB(1),
	      .ADDRB(addr1),
	      .DIB({8{0}}),

	      .DOA(data_o0),
	      .DOB(data_o1));
      end // block: ramS4
      else if (AddrSize <= 13 && DataSize <= 2) begin: ramS2
	 RAMB16_S2_S2 #(.WRITE_MODE(READ_FIRST))
	 ram (.CLKA(clock),
	      .CLKB(clock),
	      .SSRA(reset),
	      .SSRB(reset),
	      
	      .WEA(wen),
	      .ENA(1),
	      .ADDRA(addr0),
	      .DIA(data_i),
	      .WEB(wen),
	      .ENB(1),
	      .ADDRB(addr1),
	      .DIB({8{0}}),

	      .DOA(data_o0),
	      .DOB(data_o1));
      end // block: ramS2
      else if (AddrSize <= 14 && DataSize <= 1) begin: ramS1
	 RAMB16_S2_S2 #(.WRITE_MODE(READ_FIRST))
	 ram (.CLKA(clock),
	      .CLKB(clock),
	      .SSRA(reset),
	      .SSRB(reset),
	      
	      .WEA(wen),
	      .ENA(1),
	      .ADDRA(addr0),
	      .DIA(data_i),
	      .WEB(wen),
	      .ENB(1),
	      .ADDRB(addr1),
	      .DIB({8{0}}),

	      .DOA(data_o0),
	      .DOB(data_o1));
      end // block: ramS1
      else begin: ramSX
      end
   endgenerate
`endif
endmodule // RAMDP
