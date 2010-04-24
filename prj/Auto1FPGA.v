module Auto1FPGA(reset,leds);
   input wire       reset;
   output reg [7:0] leds;

   always @ * begin
      if (reset) begin
	 leds = 8'b11111111;
      end
      else begin
	 leds = 8'b11010101;
      end
   end
endmodule // Auto1FPGA
