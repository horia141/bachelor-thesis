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

module Alu(clock,reset,inst,inst_wen,result);
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_wen;

   output wire [7:0] result;

   reg [1:0] 	     c_State;
   reg [7:0] 	     c_Accum;

   reg [1:0] 	     n_State;
   reg [7:0] 	     n_Accum;

   wire [3:0] 	     w_inst_code;
   wire [7:0] 	     w_inst_imm;

   reg [64*8-1:0]    d_c_State;
   reg [64*8-1:0]    d_n_State;
   reg [64*8-1:0]    d_w_inst_code;

   assign result = n_Accum;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm  = inst[7:0];

   always @ (posedge clock) begin
      c_State <= n_State;
      c_Accum <= n_Accum;
   end

   always @ * begin
      if (reset) begin
	 n_State = `Alu_State_Reset;
	 n_Accum = {8{1'bz}};
      end
      else begin
	case (c_State)
	  `Alu_State_Reset: begin
	     n_State = `Alu_State_Ready;
	     n_Accum = 0;
	  end

	  `Alu_State_Ready: begin
	     if (inst_wen) begin
		case (w_inst_code)
		  `Alu_NOP: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum;
		  end

		  `Alu_LDI: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = w_inst_imm;
		  end

		  `Alu_ADD: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum + w_inst_imm;
		  end

		  `Alu_SUB: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum - w_inst_imm;
		  end

		  `Alu_NOT: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = ~c_Accum;
		  end

		  `Alu_AND: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum & w_inst_imm;
		  end
		  
		  `Alu_IOR: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum | w_inst_imm;
		  end

		  `Alu_XOR: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum ^ w_inst_imm;
		  end

		  `Alu_SHL: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum << w_inst_imm;
		  end

		  `Alu_SHR: begin
		     n_State = `Alu_State_Ready;
		     n_Accum = c_Accum >> w_inst_imm;
		  end

		  default: begin
		     n_State = `Alu_State_Error;
		     n_Accum = {8{1'bz}};
		  end
		endcase // case (w_inst_code)
	     end // if (inst_wen)
	     else begin
		n_State = `Alu_State_Ready;
		n_Accum = c_Accum;
	     end // else: !if(inst_wen)
	  end // case: `Alu_State_Ready

	  `Alu_State_Error: begin
	     n_State = `Alu_State_Error;
	     n_Accum = {8{1'bz}};
	  end

	  default: begin
	     n_State = `Alu_State_Error;
	     n_Accum = {8{1'bz}};
	  end
	endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`Alu_State_Reset: d_c_State = "Reset";
	`Alu_State_Ready: d_c_State = "Ready";
	`Alu_State_Error: d_c_State = "Error";
	default:          d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`Alu_State_Reset: d_n_State = "Reset";
	`Alu_State_Ready: d_n_State = "Ready";
	`Alu_State_Error: d_n_State = "Error";
	default:          d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
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
