`define Alu_NOP 4'h0
`define Alu_LDI 4'h1
`define Alu_ADD 4'h2
`define Alu_SUB 4'h3
`define Alu_NOT 4'h4
`define Alu_AND 4'h5
`define Alu_IOR 4'h6
`define Alu_XOR 4'h7
`define Alu_SHL 4'h8
`define Alu_SHR 4'h9

`define Alu_State_Reset 2'h0
`define Alu_State_Ready 2'h1
`define Alu_State_Error 2'h2

module Alu(clock,reset,inst,inst_en,result);
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   output wire [7:0] result;

   reg [1:0] 	     s_State;
   reg [7:0] 	     s_Accum;

   wire [3:0] 	     w_inst_code;
   wire [7:0] 	     w_inst_imm;

   reg [64*8-1:0]    d_s_State;
   reg [64*8-1:0]    d_w_inst_code;

   assign result = s_Accum;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm = inst[7:0];

   always @ (posedge clock) begin
      if (reset) begin
	 s_State <= `Alu_State_Reset;
	 s_Accum <= 0;
      end
      else begin
	case (s_State)
	  `Alu_State_Reset: begin
	     s_State <= `Alu_State_Ready;
	     s_Accum <= 0;
	  end

	  `Alu_State_Ready: begin
	     if (inst_en) begin
		case (w_inst_code)
		  `Alu_NOP: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum;
		  end

		  `Alu_LDI: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= w_inst_imm;
		  end

		  `Alu_ADD: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum + w_inst_imm;
		  end

		  `Alu_SUB: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum - w_inst_imm;
		  end

		  `Alu_NOT: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= ~s_Accum;
		  end

		  `Alu_AND: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum & w_inst_imm;
		  end
		  
		  `Alu_IOR: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum | w_inst_imm;
		  end

		  `Alu_XOR: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum ^ w_inst_imm;
		  end

		  `Alu_SHL: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum << w_inst_imm[2:0];
		  end

		  `Alu_SHR: begin
		     s_State <= `Alu_State_Ready;
		     s_Accum <= s_Accum >> w_inst_imm[2:0];
		  end

		  default: begin
		     s_State <= `Alu_State_Error;
		     s_Accum <= 0;
		  end
		endcase // case (w_inst_code)
	     end // if (inst_en)
	     else begin
		s_State <= `Alu_State_Ready;
		s_Accum <= s_Accum;
	     end // else: !if(inst_en)
	  end // case: `Alu_State_Ready

	  `Alu_State_Error: begin
	     s_State <= `Alu_State_Error;
	     s_Accum <= 0;
	  end

	  default: begin
	     s_State <= `Alu_State_Error;
	     s_Accum <= 0;
	  end
	endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      case (s_State)
	`Alu_State_Reset: d_s_State = "Reset";
	`Alu_State_Ready: d_s_State = "Ready";
	`Alu_State_Error: d_s_State = "Error";
	default:          d_s_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (s_State)
   end

   always @ * begin
      case (w_inst_code)
	`Alu_NOP: d_w_inst_code = "NOP";
	`Alu_LDI: d_w_inst_code = "LDI";
	`Alu_ADD: d_w_inst_code = "ADD";
	`Alu_SUB: d_w_inst_code = "SUB";
	`Alu_NOT: d_w_inst_code = "NOT";
	`Alu_AND: d_w_inst_code = "AND";
	`Alu_IOR: d_w_inst_code = "IOR";
	`Alu_XOR: d_w_inst_code = "XOR";
	`Alu_SHL: d_w_inst_code = "SHL";
	`Alu_SHR: d_w_inst_code = "SHR";
	default:  d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end // always @ *
endmodule // Alu
