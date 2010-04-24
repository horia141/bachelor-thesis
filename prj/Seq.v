`define Seq_NOP 4'h0
`define Seq_LDI 4'h1
`define Seq_LDR 4'h2
`define Seq_CMD 4'h3
`define Seq_DMP 4'h4
`define Seq_EQI 4'h5
`define Seq_EQR 4'h6
`define Seq_JXI 4'h7
`define Seq_JXR 4'h8
`define Seq_JZI 4'h9
`define Seq_JZR 4'hA

`define Seq_State_Reset 2'h0
`define Seq_State_Ready 2'h1
`define Seq_State_Error 2'h2

module Seq(clock,reset,inst,inst_en,ireg_0,ireg_1,ireg_2,ireg_3,next,oreg,oreg_wen);
   input wire         clock;
   input wire 	      reset;

   input wire [11:0]  inst;
   input wire 	      inst_en;
   input wire [7:0]   ireg_0;
   input wire [7:0]   ireg_1;
   input wire [7:0]   ireg_2;
   input wire [7:0]   ireg_3;

   output wire [7:0]  next;
   output wire [11:0] oreg;
   output wire [7:0]  oreg_wen;

   reg [1:0] 	      c_State;
   reg [7:0] 	      c_Transfer;
   reg [7:0] 	      c_Address;
   reg [11:0] 	      c_OReg;
   reg [7:0] 	      c_ORegWen;

   reg [1:0] 	      n_State;
   reg [7:0] 	      n_Transfer;
   reg [7:0] 	      n_Address;
   reg [11:0] 	      n_OReg;
   reg [7:0] 	      n_ORegWen;

   wire [3:0] 	      w_inst_code;
   wire [7:0] 	      w_inst_imm;
   wire [1:0] 	      w_inst_src;
   wire [3:0] 	      w_inst_cmd;
   wire [2:0] 	      w_inst_dst;
   wire [7:0] 	      w_ireg_mux;
   wire [7:0] 	      w_oreg_wen;

   reg [64*8-1:0]     d_c_State;
   reg [64*8-1:0]     d_n_State;
   reg [64*8-1:0]     d_w_inst_code;

   assign next = c_Address;
   assign oreg = n_OReg;
   assign oreg_wen = n_ORegWen;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm  = inst[7:0];
   assign w_inst_src  = inst[5:4];
   assign w_inst_cmd  = inst[7:4];
   assign w_inst_dst  = inst[2:0];
   assign w_ireg_mux  = w_inst_src == 0 ? ireg_0 : 
			w_inst_src == 1 ? ireg_1 : 
			w_inst_src == 2 ? ireg_2 : 
			                  ireg_3;
   assign w_oreg_wen  = w_inst_dst == 0 ? 8'b00000001 :
			w_inst_dst == 1 ? 8'b00000010 :
			w_inst_dst == 2 ? 8'b00000100 :
			w_inst_dst == 3 ? 8'b00001000 :
			w_inst_dst == 4 ? 8'b00010000 :
			w_inst_dst == 5 ? 8'b00100000 :
			w_inst_dst == 6 ? 8'b01000000 :
			                  8'b10000000;

   always @ (posedge clock) begin
      c_State    = n_State;
      c_Transfer = n_Transfer;
      c_Address  = n_Address;
      c_OReg     = n_OReg;
      c_ORegWen  = n_ORegWen;
   end

   always @ * begin
      if (reset) begin
	 n_State    = `Seq_State_Reset;
	 n_Transfer = {8{1'b0}};
	 n_Address  = {8{1'b0}};
	 n_OReg     = {12{1'b0}};
	 n_ORegWen  = {8{1'b0}};
      end
      else begin
	 case (c_State)
	   `Seq_State_Reset: begin
	      n_State    = `Seq_State_Ready;
	      n_Transfer = 0;
	      n_Address  = 0;
	      n_OReg     = 0;
	      n_ORegWen  = 0;
	   end

	   `Seq_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `Seq_NOP: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer;
		      n_Address  = c_Address + 1;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_LDI: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = w_inst_imm;
		      n_Address  = c_Address + 1;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_LDR: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = w_ireg_mux;
		      n_Address  = c_Address + 1;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_CMD: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer;
		      n_Address  = c_Address + 1;
 		      n_OReg     = {w_inst_cmd,c_Transfer};
		      n_ORegWen  = w_oreg_wen;
		   end

		   `Seq_DMP: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer;
		      n_Address  = c_Address + 1;
		      n_OReg     = {4'b0000,c_Transfer};
		      n_ORegWen  = w_oreg_wen;
		   end

		   `Seq_EQI: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer == w_inst_imm;
		      n_Address  = c_Address + 1;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_EQR: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer == w_ireg_mux;
		      n_Address  = c_Address + 1;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_JXI: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer;
		      n_Address  = w_inst_imm;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_JXR: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer;
		      n_Address  = w_ireg_mux;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_JZI: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer;
		      n_Address  = c_Transfer == 0 ? w_inst_imm : c_Address + 1;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   `Seq_JZR: begin
		      n_State    = `Seq_State_Ready;
		      n_Transfer = c_Transfer;
		      n_Address  = c_Transfer == 0 ? w_ireg_mux : c_Address + 1;
		      n_OReg     = 0;
		      n_ORegWen  = 0;
		   end

		   default: begin
		      n_State    = `Seq_State_Error;
		      n_Transfer = {8{1'b0}};
		      n_Address  = {8{1'b0}};
		      n_OReg     = {12{1'b0}};
		      n_ORegWen  = {8{1'b0}};
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 n_State    = `Seq_State_Ready;
		 n_Transfer = c_Transfer;
		 n_Address  = c_Address;
		 n_OReg     = 0;
		 n_ORegWen  = 0;
	      end // else: !if(inst_en)
	   end // case: `Seq_State_Ready

	   `Seq_State_Error: begin
	      n_State    = `Seq_State_Error;
	      n_Transfer = {8{1'b0}};
	      n_Address  = {8{1'b0}};
	      n_OReg     = {12{1'b0}};
	      n_ORegWen  = {8{1'b0}};
	   end

	   default: begin
	      n_State    = `Seq_State_Error;
	      n_Transfer = {8{1'b0}};
	      n_Address  = {8{1'b0}};
	      n_OReg     = {12{1'b0}};
	      n_ORegWen  = {8{1'b0}};
	   end
	 endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`Seq_State_Reset: d_c_State = "Reset";
	`Seq_State_Ready: d_c_State = "Ready";
	`Seq_State_Error: d_c_State = "Error";
	default:          d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`Seq_State_Reset: d_n_State = "Reset";
	`Seq_State_Ready: d_n_State = "Ready";
	`Seq_State_Error: d_n_State = "Error";
	default:          d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
   end

   always @ * begin
      case (w_inst_code)
	`Seq_NOP: d_w_inst_code = "NOP";
	`Seq_LDI: d_w_inst_code = "LDI";
	`Seq_LDR: d_w_inst_code = "LDR";
	`Seq_CMD: d_w_inst_code = "CMD";
	`Seq_DMP: d_w_inst_code = "DMP";
	`Seq_EQI: d_w_inst_code = "EQI";
	`Seq_EQR: d_w_inst_code = "EQR";
	`Seq_JXI: d_w_inst_code = "JXI";
	`Seq_JXR: d_w_inst_code = "JZR";
	`Seq_JZI: d_w_inst_code = "JZI";
	`Seq_JZR: d_w_inst_code = "JZR";
	default:  d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end // always @ *
endmodule // Seq
