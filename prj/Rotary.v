`define Rotary_NOP  4'h0
`define Rotary_RDLS 4'h1
`define Rotary_RDRS 4'h2

`define Rotary_State_Reset 2'h0
`define Rotary_State_Ready 2'h1
`define Rotary_State_Error 2'h2

module Rotary(clock,reset,inst,inst_en,rotary,rotary_left_status,rotary_right_status);
   input wire        clock;
   input wire        reset;

   input wire [11:0] inst;
   input wire 	     inst_en;
   input wire [1:0]  rotary;

   output wire 	     rotary_left_status;
   output wire 	     rotary_right_status;

   reg [1:0] 	     s_State;
   reg 		     s_IntRotaryLeftStatus;
   reg 		     s_OutRotaryLeftStatus;
   reg 		     s_IntRotaryRightStatus;
   reg 		     s_OutRotaryRightStatus;

   wire [3:0] 	     w_InstCode;

   wire 	     rotaryint_rotary_left;
   wire 	     rotaryint_rotary_right;

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign rotary_left_status = s_OutRotaryLeftStatus;
   assign rotary_right_status = s_OutRotaryRightStatus;

   assign w_InstCode = inst[11:8];

   RotaryInterface
   rotaryint (.clock(clock),
	      .reset(reset),

	      .rotary(rotary),

	      .rotary_left(rotaryint_rotary_left),
	      .rotary_right(rotaryint_rotary_right));

   always @ (posedge clock) begin
      if (reset) begin
	 s_State                <= `Rotary_State_Reset;
	 s_IntRotaryLeftStatus  <= 0;
	 s_OutRotaryLeftStatus  <= 0;
	 s_IntRotaryRightStatus <= 0;
	 s_OutRotaryRightStatus <= 0;
      end
      else begin
	 case (s_State)
	   `Rotary_State_Reset: begin
	      s_State                <= `Rotary_State_Ready;
	      s_IntRotaryLeftStatus  <= 0;
	      s_OutRotaryLeftStatus  <= 0;
	      s_IntRotaryRightStatus <= 0;
	      s_OutRotaryRightStatus <= 0;
	   end

	   `Rotary_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `Rotary_NOP: begin
		      s_State                <= `Rotary_State_Ready;
		      s_IntRotaryLeftStatus  <= rotaryint_rotary_left | s_IntRotaryLeftStatus;
		      s_OutRotaryLeftStatus  <= s_OutRotaryLeftStatus;
		      s_IntRotaryRightStatus <= rotaryint_rotary_right | s_IntRotaryRightStatus;
		      s_OutRotaryRightStatus <= s_OutRotaryRightStatus;
		   end

		   `Rotary_RDLS: begin
		      s_State                <= `Rotary_State_Ready;
		      s_IntRotaryLeftStatus  <= rotaryint_rotary_left | 0;
		      s_OutRotaryLeftStatus  <= s_IntRotaryLeftStatus;
		      s_IntRotaryRightStatus <= rotaryint_rotary_right | s_IntRotaryRightStatus;
		      s_OutRotaryRightStatus <= s_OutRotaryRightStatus;
		   end

		   `Rotary_RDRS: begin
		      s_State                <= `Rotary_State_Ready;
		      s_IntRotaryLeftStatus  <= rotaryint_rotary_left | s_IntRotaryLeftStatus;
		      s_OutRotaryLeftStatus  <= s_OutRotaryLeftStatus;
		      s_IntRotaryRightStatus <= rotaryint_rotary_right | 0;
		      s_OutRotaryRightStatus <= s_IntRotaryRightStatus;
		   end

		   default: begin
		      s_State                <= `Rotary_State_Error;
		      s_IntRotaryLeftStatus  <= 0;
		      s_OutRotaryLeftStatus  <= 0;
		      s_IntRotaryRightStatus <= 0;
		      s_OutRotaryRightStatus <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 s_State                <= `Rotary_State_Ready;
		 s_IntRotaryLeftStatus  <= rotaryint_rotary_left | s_IntRotaryLeftStatus;
		 s_OutRotaryLeftStatus  <= s_OutRotaryLeftStatus;
		 s_IntRotaryRightStatus <= rotaryint_rotary_right | s_IntRotaryRightStatus;
		 s_OutRotaryRightStatus <= s_OutRotaryRightStatus;
	      end // else: !if(inst_en)
	   end // case: `Rotary_State_Ready

	   `Rotary_State_Error: begin
	      s_State                <= `Rotary_State_Error;
	      s_IntRotaryLeftStatus  <= 0;
	      s_OutRotaryLeftStatus  <= 0;
	      s_IntRotaryRightStatus <= 0;
	      s_OutRotaryRightStatus <= 0;
	   end

	   default: begin
	      s_State                <= `Rotary_State_Error;
	      s_IntRotaryLeftStatus  <= 0;
	      s_OutRotaryLeftStatus  <= 0;
	      s_IntRotaryRightStatus <= 0;
	      s_OutRotaryRightStatus <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      if (inst_en) begin
	 case (w_InstCode)
	   `Rotary_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `Rotary_RDLS: begin
	      $sformat(d_Input,"EN RDLS");
	   end

	   `Rotary_RDRS: begin
	      $sformat(d_Input,"EN RDRS");
	   end

	   default: begin
	      $sformat(d_Input,"EN (? %2X)",inst[7:0]);
	   end
	 endcase // case (w_InstCode)
      end // if (inst_en)
      else begin
	 $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *

   always @ * begin
      case (s_State)
	`Rotary_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`Rotary_State_Ready: begin
	   case ({s_IntRotaryLeftStatus,s_OutRotaryLeftStatus,s_IntRotaryRightStatus,s_OutRotaryRightStatus})
	     4'b0000: begin
		$sformat(d_State,"R F F F F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b0001: begin
		$sformat(d_State,"R F F F T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b0010: begin
		$sformat(d_State,"R F F T F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b0011: begin
		$sformat(d_State,"R F F T T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b0100: begin
		$sformat(d_State,"R F T F F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b0101: begin
		$sformat(d_State,"R F T F T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b0110: begin
		$sformat(d_State,"R F T T F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b0111: begin
		$sformat(d_State,"R F T T T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1000: begin
		$sformat(d_State,"R T F F F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1001: begin
		$sformat(d_State,"R T F F T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1010: begin
		$sformat(d_State,"R T F T F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1011: begin
		$sformat(d_State,"R T F T T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1100: begin
		$sformat(d_State,"R T T F F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1101: begin
		$sformat(d_State,"R T T F T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1110: begin
		$sformat(d_State,"R T T T F %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end

	     4'b1111: begin
		$sformat(d_State,"R T T T T %2B %1B %1B",rotary,rotaryint_rotary_left,rotaryint_rotary_right);
	     end
	   endcase // case ({s_IntRotaryLeftStatus,s_OutRotaryLeftStatus,s_IntRotaryRightStatus,s_OutRotaryRightStatus})
	end // case: `Rotary_State_Ready

	`Rotary_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // Rotary
