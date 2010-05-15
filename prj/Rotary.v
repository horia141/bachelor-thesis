`define Rotary_NOP 4'h0
`define Rotary_RDL 4'h1
`define Rotary_RDR 4'h2

`define Rotary_State_Reset 2'h0
`define Rotary_State_Ready 2'h1
`define Rotary_State_Error 2'h2

module Rotary(clock,reset,inst,inst_en,rotary,rotary_left,rotary_right);
   input wire        clock;
   input wire        reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   input wire [1:0]  rotary;

   output wire 	     rotary_left;
   output wire 	     rotary_right;

   reg [1:0] 	     s_State;
   reg 		     s_IntRotaryLeftStatus;
   reg 		     s_OutRotaryLeftStatus;
   reg 		     s_IntRotaryRightStatus;
   reg 		     s_OutRotaryRightStatus;

   wire [3:0] 	     w_inst_code;

   wire 	     rotaryint_rotary_left;
   wire 	     rotaryint_rotary_right;

   reg [64*8-1:0]    d_s_State;
   reg [64*8-1:0]    d_s_IntRotaryLeftStatus;
   reg [64*8-1:0]    d_s_OutRotaryLeftStatus;
   reg [64*8-1:0]    d_s_IntRotaryRightStatus;
   reg [64*8-1:0]    d_s_OutRotaryRightStatus;
   reg [64*8-1:0]    d_w_inst_code;

   assign rotary_left = s_OutRotaryLeftStatus;
   assign rotary_right = s_OutRotaryRightStatus;

   assign w_inst_code = inst[11:8];

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
		 case (w_inst_code)
		   `Rotary_NOP: begin
		      s_State                <= `Rotary_State_Ready;
		      s_IntRotaryLeftStatus  <= rotaryint_rotary_left | s_IntRotaryLeftStatus;
		      s_OutRotaryLeftStatus  <= s_OutRotaryLeftStatus;
		      s_IntRotaryRightStatus <= rotaryint_rotary_right | s_IntRotaryRightStatus;
		      s_OutRotaryRightStatus <= s_OutRotaryRightStatus;
		   end

		   `Rotary_RDL: begin
		      s_State                <= `Rotary_State_Ready;
		      s_IntRotaryLeftStatus  <= rotaryint_rotary_left | 0;
		      s_OutRotaryLeftStatus  <= s_IntRotaryLeftStatus;
		      s_IntRotaryRightStatus <= rotaryint_rotary_right | s_IntRotaryRightStatus;
		      s_OutRotaryRightStatus <= s_OutRotaryRightStatus;
		   end

		   `Rotary_RDR: begin
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
		 endcase // case (w_inst_code)
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
      case (s_State)
	`Rotary_State_Reset: d_s_State = "Reset";
	`Rotary_State_Ready: d_s_State = "Ready";
	`Rotary_State_Error: d_s_State = "Error";
	default:             d_s_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (s_State)
   end

   always @ * begin
      case (s_IntRotaryLeftStatus)
	1: d_s_IntRotaryLeftStatus = "Triggered";
	0: d_s_IntRotaryLeftStatus = "Free";
      endcase // case (s_IntRotaryLeftStatus)
   end

   always @ * begin
      case (s_OutRotaryLeftStatus)
	1: d_s_OutRotaryLeftStatus = "Triggered";
	0: d_s_OutRotaryLeftStatus = "Free";
      endcase // case (s_OutRotaryLeftStatus)
   end

   always @ * begin
      case (s_IntRotaryRightStatus)
	1: d_s_IntRotaryRightStatus = "Triggered";
	0: d_s_IntRotaryRightStatus = "Free";
      endcase // case (s_IntRotaryRightStatus)
   end

   always @ * begin
      case (s_OutRotaryRightStatus)
	1: d_s_OutRotaryRightStatus = "Triggered";
	0: d_s_OutRotaryRightStatus = "Free";
      endcase // case (s_OutRotaryRightStatus)
   end

   always @ * begin
      case (w_inst_code)
	`Rotary_NOP: d_w_inst_code = "NOP";
	`Rotary_RDL: d_w_inst_code = "RDL";
	`Rotary_RDR: d_w_inst_code = "RDR";
	default:     d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end
endmodule // Rotary
