`define Seq2_NO 4'h0
`define Seq2_CI 4'h1
`define Seq2_CR 4'h2
`define Seq2_JI 4'h3
`define Seq2_JR 4'h4
`define Seq2_JZ 4'h5

`define Seq2_State_Reset       2'h0
`define Seq2_State_Ready       2'h1
`define Seq2_State_Error       2'h2

module Seq2(clock,reset,inst,inst_en,ireg_0,ireg_1,ireg_2,ireg_3,next,oreg,oreg_wen);
   input wire         clock;
   input wire         reset;

   input wire [19:0]  inst;
   input wire 	      inst_en;
   input wire [7:0]   ireg_0;
   input wire [7:0]   ireg_1;
   input wire [7:0]   ireg_2;
   input wire [7:0]   ireg_3;

   output wire [7:0]  next;
   output wire [11:0] oreg;
   output wire [7:0]  oreg_wen;

   reg [1:0] 	      c_State;
   reg [7:0] 	      c_Address;
   reg [11:0] 	      c_OReg;
   reg [7:0] 	      c_ORegWen;

   reg [1:0] 	      n_State;
   reg [7:0] 	      n_Address;
   reg [11:0] 	      n_OReg;
   reg [7:0] 	      n_ORegWen;

   wire [3:0] 	      w_inst_code;
   wire [2:0] 	      w_inst_dst;
   wire [3:0] 	      w_inst_dstcmd;
   wire [7:0] 	      w_inst_imm0;
   wire [7:0] 	      w_inst_imm1;
   wire [1:0] 	      w_inst_src;
   wire [7:0] 	      w_ireg_mux;
   wire [7:0] 	      w_oreg_wen;

   reg [64*8-1:0]     d_c_State;
   reg [64*8-1:0]     d_n_State;
   reg [64*8-1:0]     d_w_inst_code;

   assign next = c_Address;
   assign oreg = n_OReg;
   assign oreg_wen = n_ORegWen;

   assign w_inst_code   = inst[19:16];
   assign w_inst_dst    = inst[14:12];
   assign w_inst_dstcmd = inst[11:8];
   assign w_inst_imm0   = inst[15:8];
   assign w_inst_imm1   = inst[7:0];
   assign w_inst_src    = inst[1:0];

   assign w_ireg_mux    = w_inst_src == 0 ? ireg_0 :
			  w_inst_src == 1 ? ireg_1 :
			  w_inst_src == 2 ? ireg_2 :
			                    ireg_3;
   assign w_oreg_wen    = w_inst_dst == 0 ? 8'b00000001 :
			  w_inst_dst == 1 ? 8'b00000010 :
			  w_inst_dst == 2 ? 8'b00000100 :
			  w_inst_dst == 3 ? 8'b00001000 :
			  w_inst_dst == 4 ? 8'b00010000 :
			  w_inst_dst == 5 ? 8'b00100000 :
			  w_inst_dst == 6 ? 8'b01000000 :
			                    8'b10000000;

   always @ (posedge clock) begin
      c_State   = n_State;
      c_Address = n_Address;
      c_OReg    = n_OReg;
      c_ORegWen = n_ORegWen;
   end

   always @ * begin
      if (reset) begin
	 n_State   = `Seq2_State_Reset;
	 n_Address = 0;
	 n_OReg    = 0;
	 n_ORegWen = 0;
      end
      else begin
	 case (c_State)
	   `Seq2_State_Reset: begin
	      n_State   = `Seq2_State_Ready;
	      n_Address = 0;
	      n_OReg    = 0;
	      n_ORegWen = 0;
	   end

	   `Seq2_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `Seq2_NO: begin
		      n_State   = `Seq2_State_Ready;
		      n_Address = c_Address + 1;
		      n_OReg    = 0;
		      n_ORegWen = 0;
		   end

		   `Seq2_CI: begin
		      n_State   = `Seq2_State_Ready;
		      n_Address = c_Address + 1;
		      n_OReg    = {w_inst_dstcmd,w_inst_imm1};
		      n_ORegWen = w_oreg_wen;
		   end

		   `Seq2_CR: begin
		      n_State   = `Seq2_State_Ready;
		      n_Address = c_Address + 1;
		      n_OReg    = {w_inst_dstcmd,w_ireg_mux};
		      n_ORegWen = w_oreg_wen;
		   end

		   `Seq2_JI: begin
		      n_State   = `Seq2_State_Ready;
		      n_Address = w_inst_imm0;
		      n_OReg    = 0;
		      n_ORegWen = 0;
		   end

		   `Seq2_JR: begin
		      n_State   = `Seq2_State_Ready;
		      n_Address = w_ireg_mux;
		      n_OReg    = 0;
		      n_ORegWen = 0;
		   end

		   `Seq2_JZ: begin
		      n_State   = `Seq2_State_Ready;
		      n_Address = w_ireg_mux == 0 ? w_inst_imm0 : c_Address + 1;
		      n_OReg    = 0;
		      n_ORegWen = 0;
		   end

		   default: begin
		      n_State   = `Seq2_State_Error;
		      n_Address = 0;
		      n_OReg    = 0;
		      n_ORegWen = 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 n_State   = `Seq2_State_Ready;
		 n_Address = c_Address;
		 n_OReg    = 0;
		 n_ORegWen = 0;
	      end // else: !if(inst_en)
	   end // case: `Seq2_State_Ready

	   `Seq2_State_Error: begin
	      n_State   = `Seq2_State_Error;
	      n_Address = 0;
	      n_OReg    = 0;
	      n_ORegWen = 0;
	   end

	   default: begin
	      n_State   = `Seq2_State_Error;
	      n_Address = 0;
	      n_OReg    = 0;
	      n_ORegWen = 0;
	   end
	 endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`Seq2_State_Reset: d_c_State = "Reset";
	`Seq2_State_Ready: d_c_State = "Ready";
	`Seq2_State_Error: d_c_State = "Error";
	default:           d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`Seq2_State_Reset: d_n_State = "Reset";
	`Seq2_State_Ready: d_n_State = "Ready";
	`Seq2_State_Error: d_n_State = "Error";
	default:           d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
   end

   always @ * begin
      case (w_inst_code)
	`Seq2_NO: d_w_inst_code = "NO";
	`Seq2_CI: d_w_inst_code = "CI";
	`Seq2_CR: d_w_inst_code = "CR";
	`Seq2_JI: d_w_inst_code = "JI";
	`Seq2_JR: d_w_inst_code = "JR";
	`Seq2_JZ: d_w_inst_code = "JZ";
	default:  d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end // always @ *
endmodule // Seq2
