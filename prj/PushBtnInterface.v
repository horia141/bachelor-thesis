module PushBtnInterface(clock,reset,button,button_pressed);
   input wire clock;
   input wire reset;
   input wire button;

   output reg button_pressed;

   reg 	      c_Button;
   
   always @ (posedge clock) begin
      if (reset) begin
	 c_Button <= 0;
      end
      else begin
	 c_Button <= button;
      end
   end

   always @ (posedge clock) begin
      if (reset) begin
	 button_pressed = 0;
      end
      else begin
	 if (button == 1 && c_Button == 0) begin
	    button_pressed = 1;
	 end
	 else begin
	    button_pressed = 0;
	 end
      end
   end
endmodule // PushBtnInterface
