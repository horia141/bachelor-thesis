module PushBtnInterface(clock,reset,button,button_pressed);
   parameter      Wait = 40000;
   parameter      Size = 16;
   
   input wire     clock;
   input wire 	  reset;
   
   input wire 	  button;

   output wire 	  button_pressed;

   reg [Size-1:0] s_Counter;
   reg 		  s_DoneCount;
   reg 		  s_ButtonPressed;

   assign button_pressed = s_ButtonPressed;

   always @ (posedge clock) begin
      if (reset) begin
	 s_Counter       <= 0;
	 s_DoneCount     <= 0;
	 s_ButtonPressed <= 0;
      end
      else begin
	 if (button) begin
	    if (!s_DoneCount) begin
	       if (s_Counter < Wait) begin
		  s_Counter       <= s_Counter + 1;
		  s_DoneCount     <= 0;
		  s_ButtonPressed <= 0;
	       end
	       else begin
		  s_Counter       <= 0;
		  s_DoneCount     <= 1;
		  s_ButtonPressed <= 1;
	       end
	    end
	    else begin
	       s_Counter       <= 0;
	       s_DoneCount     <= 1;
	       s_ButtonPressed <= 0;
	    end // else: !if(!s_DoneCount)
	 end // if (button)
	 else begin
	    s_Counter       <= 0;
	    s_DoneCount     <= 0;
	    s_ButtonPressed <= 0;
	 end // else: !if(button)
      end // else: !if(reset)
   end // always @ (posedge clock)
endmodule // PushBtnInterface
