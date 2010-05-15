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

   reg [1:0] 	      s_State;
   reg [3:0] 	      s_ContInst;
   reg [23:0] 	      s_Counter;
   reg 		      s_Ready;

   wire [3:0] 	      w_inst_code;
   wire [7:0] 	      w_inst_imm;

   reg [64*8-1:0]     d_s_State;
   reg [64*8-1:0]     d_s_ContInst;
   reg [64*8-1:0]     d_w_inst_code;

   assign counter = s_Counter;
   assign ready = s_Ready;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm = inst[7:0];

   always @ (posedge clock) begin
      if (reset) begin
	 s_State    <= `Swc_State_Reset;
	 s_ContInst <= 0;
	 s_Counter  <= 0;
	 s_Ready    <= 0;
      end
      else begin
	 case (s_State)
	   `Swc_State_Reset: begin
	      s_State    <= `Swc_State_Ready;
	      s_ContInst <= `Swc_NOP;
	      s_Counter  <= 0;
	      s_Ready    <= 1;
	   end

	   `Swc_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `Swc_NOP: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter;
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_LD0: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= {s_Counter[23:8],w_inst_imm};
		      s_Ready    <= s_Counter == 0;
		   end
		   
		   `Swc_LD1: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= {s_Counter[23:16],w_inst_imm,s_Counter[7:0]};
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_LD2: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= {w_inst_imm,s_Counter[15:0]};
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_COU: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter + 1;
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_COD: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter - 1;
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_CCU: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_CCU;
		      s_Counter  <= s_Counter + 1;
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_CCD: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_CCD;
		      s_Counter  <= s_Counter - 1;
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_CCS: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter;
		      s_Ready    <= s_Counter == 0;
		   end

		   default: begin
		      s_State    <= `Swc_State_Error;
		      s_ContInst <= 0;
		      s_Counter  <= 0;
		      s_Ready    <= 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 case (s_ContInst)
		   `Swc_NOP: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter;
		      s_Ready    <= s_Counter == 0;
		   end

		   `Swc_CCU: begin
		      if (s_Ready) begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_NOP;
			 s_Counter  <= s_Counter;
			 s_Ready    <= s_Counter == 0;
		      end
		      else begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_CCU;
			 s_Counter  <= s_Counter + 1;
			 s_Ready    <= s_Counter == 0;
		      end // else: !if(s_Ready)
		   end // case: `Swc_CCU

		   `Swc_CCD: begin
		      if (s_Ready) begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_NOP;
			 s_Counter  <= s_Counter;
			 s_Ready    <= s_Counter == 0;
		      end
		      else begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_CCD;
			 s_Counter  <= s_Counter - 1;
			 s_Ready    <= s_Counter == 0;
		      end // else: !if(s_Ready)
		   end // case: `Swc_CCD

		   default: begin
		      s_State    <= `Swc_State_Error;
		      s_ContInst <= 0;
		      s_Counter  <= 0;
		      s_Ready    <= 0;
		   end
		 endcase // case (s_ContInst)
	      end // else: !if(inst_en)
	   end // case: `Swc_State_Reset

	   `Swc_State_Error: begin
	      s_State    <= `Swc_State_Error;
	      s_ContInst <= 0;
	      s_Counter  <= 0;
	      s_Ready    <= 0;
	   end

	   default: begin
	      s_State    <= `Swc_State_Error;
	      s_ContInst <= 0;
	      s_Counter  <= 0;
	      s_Ready    <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      case (s_State)
	`Swc_State_Reset: d_s_State = "Reset";
	`Swc_State_Ready: d_s_State = "Ready";
	`Swc_State_Error: d_s_State = "Error";
	default:          d_s_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (s_State)
   end

   always @ * begin
      case (s_ContInst)
	`Swc_NOP: d_s_ContInst = "NOP";
	`Swc_CCU: d_s_ContInst = "CCU";
	`Swc_CCD: d_s_ContInst = "CCD";
	default:  d_s_ContInst = "Undefined Continous Instruction ~ Serious Error or PreReset!";
      endcase // case (s_ContInst)
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
