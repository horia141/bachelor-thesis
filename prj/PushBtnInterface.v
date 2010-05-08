module PushBtnInterface(clock,reset,button,button_pressed);
   parameter      Wait = 40000;
   parameter      Size = 16;
   
   input wire     clock;
   input wire 	  reset;
   input wire 	  button;

   output wire 	  button_pressed;

   reg [Size-1:0] c_Counter;
   reg 		  c_DoneCount;

   reg [Size-1:0] n_Counter;
   reg 	          n_DoneCount;

   assign button_pressed = n_Counter == Wait;

   always @ (posedge clock) begin
      c_Counter   <= n_Counter;
      c_DoneCount <= n_DoneCount;
   end

   always @ * begin
      if (reset) begin
	 n_Counter   = 0;
	 n_DoneCount = 0;
      end
      else begin
	 if (button) begin
	    if (!c_DoneCount) begin
	       if (c_Counter < Wait) begin
		  n_Counter   = c_Counter + 1;
		  n_DoneCount = 0;
	       end
	       else begin
		  n_Counter   = 0;
		  n_DoneCount = 1;
	       end
	    end
	    else begin
	       n_Counter   = 0;
	       n_DoneCount = 1;
	    end // else: !if(!c_DoneCount)
	 end // if (button)
	 else begin
	    n_Counter   = 0;
	    n_DoneCount = 0;
	 end // else: !if(button)
      end // else: !if(reset)
   end // always @ *
endmodule // PushBtnInterface
