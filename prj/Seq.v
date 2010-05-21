`define Seq_NO 4'h0
`define Seq_CI 4'h1
`define Seq_CR 4'h2
`define Seq_JI 4'h3
`define Seq_JR 4'h4
`define Seq_JZ 4'h5

`define Seq_State_Reset 2'h0
`define Seq_State_Ready 2'h1
`define Seq_State_Error 2'h2

// Instruction Format
// no                      -> 0000_xxxx_xxxx_xxxx_xxxx
// ci dev(d):cmd(c)        -> 0001_xddd_cccc_xxxx_xxxx
// ci dev(d):cmd(c) arg(a) -> 0001_xddd_cccc_iiii_iiii
// cr dev(d):cmd(c) src(s) -> 0010_xddd_cccc_xxxx_xxss
// ji label(l)             -> 0011_aaaa_aaaa_xxxx_xxxx
// jr src(s)               -> 0100_xxxx_xxxx_xxxx_xxss
// jz label(l) src(s)      -> 0101_aaaa_aaaa_xxxx_xxss

module Seq(clock,reset,inst,inst_en,ireg_0,ireg_1,ireg_2,ireg_3,next,oreg,oreg_wen);
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

   reg [1:0] 	      s_State;
   reg [7:0] 	      s_Address;
   reg [11:0] 	      s_OReg;
   reg [7:0] 	      s_ORegWen;

   wire [3:0] 	      w_inst_code;
   wire [2:0] 	      w_inst_dst;
   wire [3:0] 	      w_inst_dstcmd;
   wire [7:0] 	      w_inst_imm0;
   wire [7:0] 	      w_inst_imm1;
   wire [1:0] 	      w_inst_src;
   wire [7:0] 	      w_ireg_mux;
   wire [7:0] 	      w_oreg_wen;

   reg [256*8-1:0]    d_Input;
   reg [256*8-1:0]    d_State;

   assign next = s_Address;
   assign oreg = s_OReg;
   assign oreg_wen = s_ORegWen;

   assign w_inst_code = inst[19:16];
   assign w_inst_dst = inst[14:12];
   assign w_inst_dstcmd = inst[11:8];
   assign w_inst_imm0 = inst[15:8];
   assign w_inst_imm1 = inst[7:0];
   assign w_inst_src = inst[1:0];

   assign w_ireg_mux = w_inst_src == 0 ? ireg_0 :
		       w_inst_src == 1 ? ireg_1 :
		       w_inst_src == 2 ? ireg_2 :
			                 ireg_3;
   assign w_oreg_wen = w_inst_dst == 0 ? 8'b00000001 :
	               w_inst_dst == 1 ? 8'b00000010 :
		       w_inst_dst == 2 ? 8'b00000100 :
		       w_inst_dst == 3 ? 8'b00001000 :
		       w_inst_dst == 4 ? 8'b00010000 :
		       w_inst_dst == 5 ? 8'b00100000 :
		       w_inst_dst == 6 ? 8'b01000000 :
			                 8'b10000000;

   always @ (posedge clock) begin
      if (reset) begin
	 s_State   <= `Seq_State_Reset;
	 s_Address <= 0;
	 s_OReg    <= 0;
	 s_ORegWen <= 0;
      end
      else begin
	 case (s_State)
	   `Seq_State_Reset: begin
	      s_State   <= `Seq_State_Ready;
	      s_Address <= 0;
	      s_OReg    <= 0;
	      s_ORegWen <= 0;
	   end

	   `Seq_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `Seq_NO: begin
		      s_State   <= `Seq_State_Ready;
		      s_Address <= s_Address + 1;
		      s_OReg    <= 0;
		      s_ORegWen <= 0;
		   end

		   `Seq_CI: begin
		      s_State   <= `Seq_State_Ready;
		      s_Address <= s_Address + 1;
		      s_OReg    <= {w_inst_dstcmd,w_inst_imm1};
		      s_ORegWen <= w_oreg_wen;
		   end

		   `Seq_CR: begin
		      s_State   <= `Seq_State_Ready;
		      s_Address <= s_Address + 1;
		      s_OReg    <= {w_inst_dstcmd,w_ireg_mux};
		      s_ORegWen <= w_oreg_wen;
		   end

		   `Seq_JI: begin
		      s_State   <= `Seq_State_Ready;
		      s_Address <= w_inst_imm0;
		      s_OReg    <= 0;
		      s_ORegWen <= 0;
		   end

		   `Seq_JR: begin
		      s_State   <= `Seq_State_Ready;
		      s_Address <= w_ireg_mux;
		      s_OReg    <= 0;
		      s_ORegWen <= 0;
		   end

		   `Seq_JZ: begin
		      s_State   <= `Seq_State_Ready;
		      s_Address <= w_ireg_mux == 0 ? w_inst_imm0 : s_Address + 1;
		      s_OReg    <= 0;
		      s_ORegWen <= 0;
		   end

		   default: begin
		      s_State   <= `Seq_State_Error;
		      s_Address <= 0;
		      s_OReg    <= 0;
		      s_ORegWen <= 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 s_State   <= `Seq_State_Ready;
		 s_Address <= s_Address;
		 s_OReg    <= 0;
		 s_ORegWen <= 0;
	      end // else: !if(inst_en)
	   end // case: `Seq_State_Ready

	   `Seq_State_Error: begin
	      s_State   <= `Seq_State_Error;
	      s_Address <= 0;
	      s_OReg    <= 0;
	      s_ORegWen <= 0;
	   end

	   default: begin
	      s_State   <= `Seq_State_Error;
	      s_Address <= 0;
	      s_OReg    <= 0;
	      s_ORegWen <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      if (inst_en) begin
	 case (w_inst_code)
	   `Seq_NO: begin
	      $sformat(d_Input,"EN NO %2X %2X %2X %2X",ireg_0,ireg_1,ireg_2,ireg_3);
	   end

	   `Seq_CI: begin
	      $sformat(d_Input,"EN (CI %1D %1X %2X) %2X %2X %2X %2X",w_inst_dst,w_inst_dstcmd,w_inst_imm1,ireg_0,ireg_1,ireg_2,ireg_3);
	   end

	   `Seq_CR: begin
	      $sformat(d_Input,"EN (CR %1D %1X %1D) %2X %2X %2X %2X",w_inst_dst,w_inst_dstcmd,w_inst_src,ireg_0,ireg_1,ireg_2,ireg_3);
	   end

	   `Seq_JI: begin
	      $sformat(d_Input,"EN (JI %2X) %2X %2X %2X %2X",w_inst_imm0,ireg_0,ireg_1,ireg_2,ireg_3);
	   end

	   `Seq_JR: begin
	      $sformat(d_Input,"EN (JR %1D) %2X %2X %2X %2X",w_inst_src,ireg_0,ireg_1,ireg_2,ireg_3);
	   end

	   `Seq_JZ: begin
	      $sformat(d_Input,"EN (JZ %2X %1D) %2X %2X %2X %2X",w_inst_imm0,w_inst_src,ireg_0,ireg_1,ireg_2,ireg_3);
	   end

	   default: begin
	      $sformat(d_Input,"EN (? %4X) %2X %2X %2X %2X",inst[15:0],ireg_0,ireg_1,ireg_2,ireg_3);
	   end
	 endcase // case (w_inst_code)
      end // if (inst_en)
      else begin
	 $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *
   
   always @ * begin
      case (s_State)
	`Seq_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`Seq_State_Ready: begin
	   $sformat(d_State,"R %2X %3X %8B",s_Address,s_OReg,s_ORegWen);
	end

	`Seq_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // Seq
