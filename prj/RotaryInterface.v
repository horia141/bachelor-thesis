module RotaryInterface(clock,reset,rotary,rotary_left,rotary_right);
   input wire       clock;
   input wire       reset;
   
   input wire [1:0] rotary;

   output wire 	    rotary_left;
   output wire 	    rotary_right;

   reg 		    s_Filter1;
   reg 		    s_Filter2;
   reg 		    s_Filter1Delay;
   reg 		    s_RotaryLeft;
   reg 		    s_DoneRotaryLeft;
   reg 		    s_RotaryRight;
   reg 		    s_DoneRotaryRight;

   assign rotary_left = s_RotaryLeft;
   assign rotary_right = s_RotaryRight;

   always @ (posedge clock) begin
      if (reset) begin
	 s_Filter1 <= 0;
	 s_Filter2 <= 0;
      end
      else begin
	 case (rotary)
	   2'b00: begin
	      s_Filter1 <= 0;
	      s_Filter2 <= s_Filter2;
	   end

	   2'b01: begin
	      s_Filter1 <= s_Filter1;
	      s_Filter2 <= 0;
	   end

	   2'b10: begin
	      s_Filter1 <= s_Filter1;
	      s_Filter2 <= 1;
	   end

	   2'b11: begin
	      s_Filter1 <= 1;
	      s_Filter2 <= s_Filter2;
	   end

	   default: begin
	      s_Filter1 <= s_Filter1;
	      s_Filter2 <= s_Filter2;
	   end
	 endcase // case (rotary)
      end
   end // always @ (posedge clock)

   always @ (posedge clock) begin
      if (reset) begin
	 s_Filter1Delay    <= 0;
	 s_RotaryLeft      <= 0;
	 s_DoneRotaryLeft  <= 0;
	 s_RotaryRight     <= 0;
	 s_DoneRotaryRight <= 0;
      end
      else begin
	 if (s_Filter1 && !s_Filter1Delay) begin
	    if (s_Filter2) begin
	       if (!s_DoneRotaryRight) begin
		  s_Filter1Delay    <= s_Filter1;
		  s_RotaryLeft      <= 0;
		  s_DoneRotaryLeft  <= 0;
		  s_RotaryRight     <= 1;
		  s_DoneRotaryRight <= 1;
	       end
	       else begin
		  s_Filter1Delay    <= s_Filter1;
		  s_RotaryLeft      <= 0;
		  s_DoneRotaryLeft  <= 0;
		  s_RotaryRight     <= 0;
		  s_DoneRotaryRight <= 1;
	       end // else: !if(!s_DoneRotaryRight)
	    end // if (s_Filter2)
	    else begin
	       if (!s_DoneRotaryLeft) begin
		  s_Filter1Delay    <= s_Filter1;
		  s_RotaryLeft      <= 1;
		  s_DoneRotaryLeft  <= 1;
		  s_RotaryRight     <= 0;
		  s_DoneRotaryRight <= 0;
	       end
	       else begin
		  s_Filter1Delay    <= s_Filter1;
		  s_RotaryLeft      <= 0;
		  s_DoneRotaryLeft  <= 1;
		  s_RotaryRight     <= 0;
		  s_DoneRotaryRight <= 0;
	       end // else: !if(!s_DoneRotaryLeft)
	    end // else: !if(s_Filter2)
	 end // if (s_Filter1 && !s_Filter1Delay)
	 else begin
	    s_Filter1Delay    <= s_Filter1;
	    s_RotaryLeft      <= 0;
	    s_DoneRotaryLeft  <= 0;
	    s_RotaryRight     <= 0;
	    s_DoneRotaryRight <= 0;
	 end // else: !if(s_Filter1 && !s_Filter1Delay)
      end // else: !if(reset)
   end // always @ (posedge clock)
endmodule // RotaryInterface
