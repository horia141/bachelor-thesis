module Rom16x12(addr,data_o);
   parameter Word_00 = 12'h000;
   parameter Word_01 = 12'h000;
   parameter Word_02 = 12'h000;
   parameter Word_03 = 12'h000;
   parameter Word_04 = 12'h000;
   parameter Word_05 = 12'h000;
   parameter Word_06 = 12'h000;
   parameter Word_07 = 12'h000;
   parameter Word_08 = 12'h000;
   parameter Word_09 = 12'h000;
   parameter Word_0A = 12'h000;
   parameter Word_0B = 12'h000;
   parameter Word_0C = 12'h000;
   parameter Word_0D = 12'h000;
   parameter Word_0E = 12'h000;
   parameter Word_0F = 12'h000;
   
   input wire [3:0]  addr;
   output reg [11:0] data_o;

   always @ * begin
      case (addr)
	4'h0: data_o = Word_00;
	4'h1: data_o = Word_01;
	4'h2: data_o = Word_02;
	4'h3: data_o = Word_03;
	4'h4: data_o = Word_04;
	4'h5: data_o = Word_05;
	4'h6: data_o = Word_06;
	4'h7: data_o = Word_07;
	4'h8: data_o = Word_08;
	4'h9: data_o = Word_09;
	4'hA: data_o = Word_0A;
	4'hB: data_o = Word_0B;
	4'hC: data_o = Word_0C;
	4'hD: data_o = Word_0D;
	4'hE: data_o = Word_0E;
	4'hF: data_o = Word_0F;
	default: data_o = {12{1'bz}};
      endcase // case (addr)
   end // always @ *
endmodule // Rom
