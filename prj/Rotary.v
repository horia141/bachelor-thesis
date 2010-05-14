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

   reg [1:0] 	     c_State;
   reg 		     c_IntRotaryLeftStatus;
   reg 		     c_OutRotaryLeftStatus;
   reg 		     c_IntRotaryRightStatus;
   reg 		     c_OutRotaryRightStatus;

   reg [1:0] 	     n_State;
   reg 		     n_IntRotaryLeftStatus;
   reg 		     n_OutRotaryLeftStatus;
   reg 		     n_IntRotaryRightStatus;
   reg 		     n_OutRotaryRightStatus;

   wire [3:0] 	     w_inst_code;

   wire 	     rotaryint_rotary_left;
   wire 	     rotaryint_rotary_right;

   reg [64*8-1:0]    d_c_State;
   reg [64*8-1:0]    d_n_State;
   reg [64*8-1:0]    d_c_IntRotaryLeftStatus;
   reg [64*8-1:0]    d_n_IntRotaryLeftStatus;
   reg [64*8-1:0]    d_c_OutRotaryLeftStatus;
   reg [64*8-1:0]    d_n_OutRotaryLeftStatus;
   reg [64*8-1:0]    d_c_IntRotaryRightStatus;
   reg [64*8-1:0]    d_n_IntRotaryRightStatus;
   reg [64*8-1:0]    d_c_OutRotaryRightStatus;
   reg [64*8-1:0]    d_n_OutRotaryRightStatus;
   reg [64*8-1:0]    d_w_inst_code;

   assign rotary_left = n_OutRotaryLeftStatus;
   assign rotary_right = n_OutRotaryRightStatus;

   assign w_inst_code = inst[11:8];

   RotaryInterface
   rotaryint (.clock(clock),
	      .reset(reset),

	      .rotary(rotary),

	      .rotary_left(rotaryint_rotary_left),
	      .rotary_right(rotaryint_rotary_right));

   always @ (posedge clock) begin
      c_State                <= n_State;
      c_IntRotaryLeftStatus  <= n_IntRotaryLeftStatus;
      c_OutRotaryLeftStatus  <= n_OutRotaryLeftStatus;
      c_IntRotaryRightStatus <= n_IntRotaryRightStatus;
      c_OutRotaryRightStatus <= n_OutRotaryRightStatus;
   end

   always @ * begin
      if (reset) begin
	 n_State                = `Rotary_State_Reset;
	 n_IntRotaryLeftStatus  = 0;
	 n_OutRotaryLeftStatus  = 0;
	 n_IntRotaryRightStatus = 0;
	 n_OutRotaryRightStatus = 0;
      end
      else begin
	 case (c_State)
	   `Rotary_State_Reset: begin
	      n_State                = `Rotary_State_Ready;
	      n_IntRotaryLeftStatus  = 0;
	      n_OutRotaryLeftStatus  = 0;
	      n_IntRotaryRightStatus = 0;
	      n_OutRotaryRightStatus = 0;
	   end

	   `Rotary_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `Rotary_NOP: begin
		      n_State                = `Rotary_State_Ready;
		      n_IntRotaryLeftStatus  = rotaryint_rotary_left | c_IntRotaryLeftStatus;
		      n_OutRotaryLeftStatus  = c_OutRotaryLeftStatus;
		      n_IntRotaryRightStatus = rotaryint_rotary_right | c_IntRotaryRightStatus;
		      n_OutRotaryRightStatus = c_OutRotaryRightStatus;
		   end

		   `Rotary_RDL: begin
		      n_State                = `Rotary_State_Ready;
		      n_IntRotaryLeftStatus  = rotaryint_rotary_left | 0;
		      n_OutRotaryLeftStatus  = c_IntRotaryLeftStatus;
		      n_IntRotaryRightStatus = rotaryint_rotary_right | c_IntRotaryRightStatus;
		      n_OutRotaryRightStatus = c_OutRotaryRightStatus;
		   end

		   `Rotary_RDR: begin
		      n_State                = `Rotary_State_Ready;
		      n_IntRotaryLeftStatus  = rotaryint_rotary_left | c_IntRotaryLeftStatus;
		      n_OutRotaryLeftStatus  = c_OutRotaryLeftStatus;
		      n_IntRotaryRightStatus = rotaryint_rotary_right | 0;
		      n_OutRotaryRightStatus = c_IntRotaryRightStatus;
		   end

		   default: begin
		      n_State                = `Rotary_State_Error;
		      n_IntRotaryLeftStatus  = 0;
		      n_OutRotaryLeftStatus  = 0;
		      n_IntRotaryRightStatus = 0;
		      n_OutRotaryRightStatus = 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 n_State                = `Rotary_State_Ready;
		 n_IntRotaryLeftStatus  = rotaryint_rotary_left | c_IntRotaryLeftStatus;
		 n_OutRotaryLeftStatus  = c_OutRotaryLeftStatus;
		 n_IntRotaryRightStatus = rotaryint_rotary_right | c_IntRotaryRightStatus;
		 n_OutRotaryRightStatus = c_OutRotaryRightStatus;
	      end // else: !if(inst_en)
	   end // case: `Rotary_State_Ready

	   `Rotary_State_Error: begin
	      n_State                = `Rotary_State_Error;
	      n_IntRotaryLeftStatus  = 0;
	      n_OutRotaryLeftStatus  = 0;
	      n_IntRotaryRightStatus = 0;
	      n_OutRotaryRightStatus = 0;
	   end

	   default: begin
	      n_State                = `Rotary_State_Error;
	      n_IntRotaryLeftStatus  = 0;
	      n_OutRotaryLeftStatus  = 0;
	      n_IntRotaryRightStatus = 0;
	      n_OutRotaryRightStatus = 0;
	   end
	 endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`Rotary_State_Reset: d_c_State = "Reset";
	`Rotary_State_Ready: d_c_State = "Ready";
	`Rotary_State_Error: d_c_State = "Error";
	default:             d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`Rotary_State_Reset: d_n_State = "Reset";
	`Rotary_State_Ready: d_n_State = "Ready";
	`Rotary_State_Error: d_n_State = "Error";
	default:             d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
   end

   always @ * begin
      case (c_IntRotaryLeftStatus)
	1: d_c_IntRotaryLeftStatus = "Triggered";
	0: d_c_IntRotaryLeftStatus = "Free";
      endcase // case (c_IntRotaryLeftStatus)
   end

   always @ * begin
      case (n_IntRotaryLeftStatus)
	1: d_n_IntRotaryLeftStatus = "Triggered";
	0: d_n_IntRotaryLeftStatus = "Free";
      endcase // case (n_IntRotaryLeftStatus)
   end

   always @ * begin
      case (c_OutRotaryLeftStatus)
	1: d_c_OutRotaryLeftStatus = "Triggered";
	0: d_c_OutRotaryLeftStatus = "Free";
      endcase // case (c_OutRotaryLeftStatus)
   end

   always @ * begin
      case (n_OutRotaryLeftStatus)
	1: d_n_OutRotaryLeftStatus = "Triggered";
	0: d_n_OutRotaryLeftStatus = "Free";
      endcase // case (n_OutRotaryLeftStatus)
   end

   always @ * begin
      case (c_IntRotaryRightStatus)
	1: d_c_IntRotaryRightStatus = "Triggered";
	0: d_c_IntRotaryRightStatus = "Free";
      endcase // case (c_IntRotaryRightStatus)
   end

   always @ * begin
      case (n_IntRotaryRightStatus)
	1: d_n_IntRotaryRightStatus = "Triggered";
	0: d_n_IntRotaryRightStatus = "Free";
      endcase // case (n_IntRotaryRightStatus)
   end

   always @ * begin
      case (c_OutRotaryRightStatus)
	1: d_c_OutRotaryRightStatus = "Triggered";
	0: d_c_OutRotaryRightStatus = "Free";
      endcase // case (c_OutRotaryRightStatus)
   end

   always @ * begin
      case (n_OutRotaryRightStatus)
	1: d_n_OutRotaryRightStatus = "Triggered";
	0: d_n_OutRotaryRightStatus = "Free";
      endcase // case (n_OutRotaryRightStatus)
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
