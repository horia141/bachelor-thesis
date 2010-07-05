`define RegBankS4_NOP 4'h0
`define RegBankS4_RDO 4'h1
`define RegBankS4_LD0 4'h2
`define RegBankS4_LD1 4'h3
`define RegBankS4_LD2 4'h4
`define RegBankS4_LD3 4'h5

`define RegBankS4_State_Reset 2'h0
`define RegBankS4_State_Ready 2'h1
`define RegBankS4_State_Error 2'h2

module RegBankS4(clock,reset,inst,inst_en,out);
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   output wire [7:0] out;

   reg [1:0] 	     s_State;
   reg [1:0] 	     s_OutSelect;
   reg [7:0] 	     s_Reg0;
   reg [7:0] 	     s_Reg1;
   reg [7:0] 	     s_Reg2;
   reg [7:0] 	     s_Reg3;
   wire [7:0] 	     i_Out;

   wire [3:0] 	     w_InstCode;
   wire [7:0] 	     w_InstImm;
   wire [1:0] 	     w_InstOutSelect;

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign out = i_Out;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];
   assign w_InstOutSelect = inst[1:0];

   assign i_Out = s_OutSelect == 0 ? s_Reg0 :
		  s_OutSelect == 1 ? s_Reg1 :
		  s_OutSelect == 2 ? s_Reg2 :
		                     s_Reg3;

   always @ (posedge clock) begin
      if (reset) begin
	 s_State     <= `RegBankS4_State_Reset;
	 s_OutSelect <= 0;
	 s_Reg0      <= 0;
	 s_Reg1      <= 0;
	 s_Reg2      <= 0;
	 s_Reg3      <= 0;
      end
      else begin
	 case (s_State)
	   `RegBankS4_State_Reset: begin
	      s_State     <= `RegBankS4_State_Ready;
	      s_OutSelect <= 0;
	      s_Reg0      <= 0;
	      s_Reg1      <= 0;
	      s_Reg2      <= 0;
	      s_Reg3      <= 0;
	   end

	   `RegBankS4_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `RegBankS4_NOP: begin
		      s_State     <= `RegBankS4_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		   end

		   `RegBankS4_RDO: begin
		      s_State     <= `RegBankS4_State_Ready;
		      s_OutSelect <= w_InstOutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		   end

		   `RegBankS4_LD0: begin
		      s_State     <= `RegBankS4_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= w_InstImm;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		   end

		   `RegBankS4_LD1: begin
		      s_State     <= `RegBankS4_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= w_InstImm;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		   end

		   `RegBankS4_LD2: begin
		      s_State     <= `RegBankS4_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= w_InstImm;
		      s_Reg3      <= s_Reg3;
		   end

		   `RegBankS4_LD3: begin
		      s_State     <= `RegBankS4_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= w_InstImm;
		   end

		   default: begin
		      s_State     <= `RegBankS4_State_Error;
		      s_OutSelect <= 0;
		      s_Reg0      <= 0;
		      s_Reg1      <= 0;
		      s_Reg2      <= 0;
		      s_Reg3      <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 s_State     <= `RegBankS4_State_Ready;
		 s_OutSelect <= s_OutSelect;
		 s_Reg0      <= s_Reg0;
		 s_Reg1      <= s_Reg1;
		 s_Reg2      <= s_Reg2;
		 s_Reg3      <= s_Reg3;
	      end // else: !if(inst_en)
	   end // case: `RegBankS4_State_Ready

	   `RegBankS4_State_Error: begin
	      s_State     <= `RegBankS4_State_Error;
	      s_OutSelect <= 0;
	      s_Reg0      <= 0;
	      s_Reg1      <= 0;
	      s_Reg2      <= 0;
	      s_Reg3      <= 0;
	   end

	   default: begin
	      s_State     <= `RegBankS4_State_Error;
	      s_OutSelect <= 0;
	      s_Reg0      <= 0;
	      s_Reg1      <= 0;
	      s_Reg2      <= 0;
	      s_Reg3      <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

`ifdef SIM
   always @ * begin
      if (inst_en) begin
	 case (w_InstCode)
	   `RegBankS4_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `RegBankS4_RDO: begin
	      $sformat(d_Input,"EN (RDO %1X)",w_InstOutSelect);
	   end

	   `RegBankS4_LD0: begin
	      $sformat(d_Input,"EN (LD0 %2X)",w_InstImm);
	   end

	   `RegBankS4_LD1: begin
	      $sformat(d_Input,"EN (LD1 %2X)",w_InstImm);
	   end

	   `RegBankS4_LD2: begin
	      $sformat(d_Input,"EN (LD2 %2X)",w_InstImm);
	   end

	   `RegBankS4_LD3: begin
	      $sformat(d_Input,"EN (LD3 %2X)",w_InstImm);
	   end

	   default: begin
	      $sformat(d_Input,"EN (? %2X)",w_InstImm);
	   end
	 endcase // case (w_InstCode)
      end // if (inst_en)
      else begin
	 $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *

   always @ * begin
      case (s_State)
	`RegBankS4_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`RegBankS4_State_Ready: begin
	   $sformat(d_State,"R %1X %2X %2X %2X %2X",s_OutSelect,s_Reg0,s_Reg1,s_Reg2,s_Reg3);
	end

	`RegBankS4_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
`endif //  `ifdef SIM
endmodule // RegBankS4
