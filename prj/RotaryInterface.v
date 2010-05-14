module RotaryInterface(clock,reset,rotary,rotary_left,rotary_right);
   input wire       clock;
   input wire 	    reset;
   input wire [1:0] rotary;

   output wire 	    rotary_left;
   output wire 	    rotary_right;

   reg 		    c_RotaryLeft;
   reg 		    c_RotaryRight;
   reg 		    c_DoneLeft;
   reg 		    c_DoneRight;

   reg 		    n_RotaryLeft;
   reg 		    n_RotaryRight;
   reg 		    n_DoneLeft;
   reg 		    n_DoneRight;

   assign rotary_left = c_RotaryLeft;
   assign rotary_right = c_RotaryRight;

   always @ (posedge clock) begin
      c_RotaryLeft  <= n_RotaryLeft;
      c_RotaryRight <= n_RotaryRight;
      c_DoneLeft    <= n_DoneLeft;
      c_DoneRight   <= n_DoneRight;
   end

   always @ * begin
      if (reset) begin
	 n_RotaryLeft  = 0;
	 n_RotaryRight = 0;
	 n_DoneLeft    = 0;
	 n_DoneRight   = 0;
      end
      else begin
	 case (rotary)
	   2'b01: begin
	      if (!c_DoneLeft) begin
		 n_RotaryLeft  = 1;
		 n_RotaryRight = 0;
		 n_DoneLeft    = 1;
		 n_DoneRight   = 0;
	      end
	      else begin
		 n_RotaryLeft  = 0;
		 n_RotaryRight = 0;
		 n_DoneLeft    = 1;
		 n_DoneRight   = 0;
	      end
	   end

	   2'b10: begin
	      if (!c_DoneRight) begin
		 n_RotaryLeft  = 0;
		 n_RotaryRight = 1;
		 n_DoneLeft    = 0;
		 n_DoneRight   = 1;
	      end
	      else begin
		 n_RotaryLeft  = 0;
		 n_RotaryRight = 0;
		 n_DoneLeft    = 0;
		 n_DoneRight   = 1;
	      end
	   end

	   default: begin
	      n_RotaryLeft  = 0;
	      n_RotaryRight = 0;
	      n_DoneLeft    = 0;
	      n_DoneRight   = 0;
	   end
	 endcase // case ({rotary_a,rotary_b})
      end // else: !if(reset)
   end // always @ *
endmodule // RotaryInterface
