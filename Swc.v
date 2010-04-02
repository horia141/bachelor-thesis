`define Swc_NOP 4'h0
`define Swc_LD0 4'h1
`define Swc_LD1 4'h2
`define Swc_LD2 4'h3
`define Swc_COU 4'h4
`define Swc_COD 4'h5
`define Swc_CCU 4'h6
`define Swc_CCD 4'h7
`define Swc_CCS 4'h8

`define Swc_State_Reset 2'h0
`define Swc_State_Ready 2'h1
`define Swc_State_Error 2'h2

module Swc(clock,reset,inst,inst_en,counter,ready);
   input wire         clock;
   input wire 	      reset;

   input wire [11:0]  inst;
   input wire 	      inst_en;

   output wire [23:0] counter;
   output wire 	      ready;

   reg [1:0] 	      c_State;
   reg [3:0] 	      c_ContInst;
   reg [23:0] 	      c_Counter;
   reg 		      c_Ready;

   reg [1:0] 	      n_State;
   reg [3:0] 	      n_ContInst;
   reg [23:0] 	      n_Counter;
   reg 		      n_Ready;

   wire [3:0] 	      w_inst_code;
   wire [7:0] 	      w_inst_imm;

   reg [64*8-1:0]     d_c_State;
   reg [64*8-1:0]     d_n_State;
   reg [64*8-1:0]     d_c_ContInst;
   reg [64*8-1:0]     d_n_ContInst;
   reg [64*8-1:0]     d_w_inst_code;

   assign counter = n_Counter;
   assign ready = n_Ready;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm  = inst[7:0];

   always @ (posedge clock) begin
      c_State    <= n_State;
      c_ContInst <= n_ContInst;
      c_Counter  <= n_Counter;
      c_Ready    <= n_Ready;
   end

   always @ * begin
      if (reset) begin
	 n_State    = `Swc_State_Reset;
	 n_ContInst = {4{1'bz}};
	 n_Counter  = {24{1'bz}};
	 n_Ready    = 1'bz;
      end
      else begin
	 case (c_State)
	   `Swc_State_Reset: begin
	      n_State    = `Swc_State_Ready;
	      n_ContInst = `Swc_NOP;
	      n_Counter  = 0;
	      n_Ready    = 1;
	   end

	   `Swc_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `Swc_NOP: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = c_Counter;
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_LD0: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = {c_Counter[23:8],w_inst_imm};
		      n_Ready    = n_Counter == 0;
		   end
		   
		   `Swc_LD1: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = {c_Counter[23:16],w_inst_imm,c_Counter[7:0]};
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_LD2: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = {w_inst_imm,c_Counter[15:0]};
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_COU: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = c_Counter + 1;
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_COD: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = c_Counter - 1;
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_CCU: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_CCU;
		      n_Counter  = c_Counter + 1;
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_CCD: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_CCD;
		      n_Counter  = c_Counter - 1;
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_CCS: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = c_Counter;
		      n_Ready    = n_Counter == 0;
		   end

		   default: begin
		      n_State    = `Swc_State_Error;
		      n_ContInst = {4{1'bz}};
		      n_Counter  = {24{1'bz}};
		      n_Ready    = 1'bz;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 case (c_ContInst)
		   `Swc_NOP: begin
		      n_State    = `Swc_State_Ready;
		      n_ContInst = `Swc_NOP;
		      n_Counter  = c_Counter;
		      n_Ready    = n_Counter == 0;
		   end

		   `Swc_CCU: begin
		      if (c_Ready) begin
			 n_State    = `Swc_State_Ready;
			 n_ContInst = `Swc_NOP;
			 n_Counter  = c_Counter;
			 n_Ready    = n_Counter == 0;
		      end
		      else begin
			 n_State    = `Swc_State_Ready;
			 n_ContInst = `Swc_CCU;
			 n_Counter  = c_Counter + 1;
			 n_Ready    = n_Counter == 0;
		      end // else: !if(c_Ready)
		   end // case: `Swc_CCU

		   `Swc_CCD: begin
		      if (c_Ready) begin
			 n_State    = `Swc_State_Ready;
			 n_ContInst = `Swc_NOP;
			 n_Counter  = c_Counter;
			 n_Ready    = n_Counter == 0;
		      end
		      else begin
			 n_State    = `Swc_State_Ready;
			 n_ContInst = `Swc_CCD;
			 n_Counter  = c_Counter - 1;
			 n_Ready    = n_Counter == 0;
		      end // else: !if(c_Ready)
		   end // case: `Swc_CCD

		   default: begin
		      n_State    = `Swc_State_Error;
		      n_ContInst = {4{1'bz}};
		      n_Counter  = {24{1'bz}};
		      n_Ready    = 1'bz;
		   end
		 endcase // case (c_ContInst)
	      end // else: !if(inst_en)
	   end // case: `Swc_State_Reset

	   `Swc_State_Error: begin
	      n_State    = `Swc_State_Error;
	      n_ContInst = {4{1'bz}};
	      n_Counter  = {24{1'bz}};
	      n_Ready    = 1'bz;
	   end

	   default: begin
	      n_State    = `Swc_State_Error;
	      n_ContInst = {4{1'bz}};
	      n_Counter  = {24{1'bz}};
	      n_Ready    = 1'bz;
	   end
	 endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`Swc_State_Reset: d_c_State = "Reset";
	`Swc_State_Ready: d_c_State = "Ready";
	`Swc_State_Error: d_c_State = "Error";
	default:          d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`Swc_State_Reset: d_n_State = "Reset";
	`Swc_State_Ready: d_n_State = "Ready";
	`Swc_State_Error: d_n_State = "Error";
	default:          d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
   end
   
   always @ * begin
      case (c_ContInst)
	`Swc_NOP: d_c_ContInst = "NOP";
	`Swc_CCU: d_c_ContInst = "CCU";
	`Swc_CCD: d_c_ContInst = "CCD";
	default:  d_c_ContInst = "Undefined Continous Instruction ~ Serious Error or PreReset!";
      endcase // case (c_ContInst)
   end

   always @ * begin
      case (n_ContInst)
	`Swc_NOP: d_n_ContInst = "NOP";
	`Swc_CCU: d_n_ContInst = "CCU";
	`Swc_CCD: d_n_ContInst = "CCD";
	default:  d_n_ContInst = "Undefined Continous Instruction ~ Serious Error or PreReset!";
      endcase // case (n_ContInst)
   end

   always @ * begin
      case (w_inst_code)
	`Swc_NOP: d_w_inst_code = "NOP";
	`Swc_LD0: d_w_inst_code = "LD0";
	`Swc_LD1: d_w_inst_code = "LD1";
	`Swc_LD2: d_w_inst_code = "LD2";
	`Swc_COU: d_w_inst_code = "COU";
	`Swc_COD: d_w_inst_code = "COD";
	`Swc_CCU: d_w_inst_code = "CCU";
	`Swc_CCD: d_w_inst_code = "CCD";
	`Swc_CCS: d_w_inst_code = "CCS";
	default:  d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end // always @ *
endmodule // Swc
