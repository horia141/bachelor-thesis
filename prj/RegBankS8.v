`define RegBankS8_NOP 4'h0
`define RegBankS8_RDO 4'h1
`define RegBankS8_LD0 4'h2
`define RegBankS8_LD1 4'h3
`define RegBankS8_LD2 4'h4
`define RegBankS8_LD3 4'h5
`define RegBankS8_LD4 4'h2
`define RegBankS8_LD5 4'h3
`define RegBankS8_LD6 4'h4
`define RegBankS8_LD7 4'h5

`define RegBankS8_State_Reset 2'h0
`define RegBankS8_State_Ready 2'h1
`define RegBankS8_State_Error 2'h2

module RegBankS8(clock,reset,inst,inst_en,out);
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   output wire [7:0] out;

   reg [1:0] 	     s_State;
   reg [2:0] 	     s_OutSelect;
   reg [7:0] 	     s_Reg0;
   reg [7:0] 	     s_Reg1;
   reg [7:0] 	     s_Reg2;
   reg [7:0] 	     s_Reg3;
   reg [7:0] 	     s_Reg4;
   reg [7:0] 	     s_Reg5;
   reg [7:0] 	     s_Reg6;
   reg [7:0] 	     s_Reg7;
   reg [7:0] 	     i_Out;

   wire [3:0] 	     w_InstCode;
   wire [7:0] 	     w_InstImm;
   wire [2:0] 	     w_InstOutSelect;

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign out = i_Out;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];
   assign w_InstOutSelect = inst[0:0];

   assign i_Out = s_OutSelect == 0 ? s_Reg0 :
		  s_OutSelect == 1 ? s_Reg1 :
		  s_OutSelect == 2 ? s_Reg2 :
		  s_OutSelect == 3 ? s_Reg3 :
		  s_OutSelect == 4 ? s_Reg4 :
		  s_OutSelect == 5 ? s_Reg5 :
		  s_OutSelect == 6 ? s_Reg6 :
		                     s_Reg7;

   always @ (posedge clock) begin
      if (reset) begin
	 s_State     <= `RegBankS8_State_Reset;
	 s_OutSelect <= 0;
	 s_Reg0      <= 0;
	 s_Reg1      <= 0;
	 s_Reg2      <= 0;
	 s_Reg3      <= 0;
	 s_Reg4      <= 0;
	 s_Reg5      <= 0;
	 s_Reg6      <= 0;
	 s_Reg7      <= 0;
      end
      else begin
	 case (s_State)
	   `RegBankS8_State_Reset: begin
	      s_State     <= `RegBankS8_State_Ready;
	      s_OutSelect <= 0;
	      s_Reg0      <= 0;
	      s_Reg1      <= 0;
	      s_Reg2      <= 0;
	      s_Reg3      <= 0;
	      s_Reg4      <= 0;
	      s_Reg5      <= 0;
	      s_Reg6      <= 0;
	      s_Reg7      <= 0;
	   end

	   `RegBankS8_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `RegBankS8_NOP: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end

		   `RegBankS8_RDO: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= w_InstOutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end

		   `RegBankS8_LD0: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= w_InstImm;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end

		   `RegBankS8_LD1: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= w_InstImm;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end

		   `RegBankS8_LD2: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= w_InstImm;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end

		   `RegBankS8_LD3: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= w_InstImm;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end // case: `RegBankS8_LD3

		   `RegBankS8_LD4: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= w_InstImm;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end // case: `RegBankS8_LD3

		   `RegBankS8_LD5: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= w_InstImm;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= s_Reg7;
		   end // case: `RegBankS8_LD3

		   `RegBankS8_LD6: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= w_InstImm;
		      s_Reg7      <= s_Reg7;
		   end // case: `RegBankS8_LD3

		   `RegBankS8_LD3: begin
		      s_State     <= `RegBankS8_State_Ready;
		      s_OutSelect <= s_OutSelect;
		      s_Reg0      <= s_Reg0;
		      s_Reg1      <= s_Reg1;
		      s_Reg2      <= s_Reg2;
		      s_Reg3      <= s_Reg3;
		      s_Reg4      <= s_Reg4;
		      s_Reg5      <= s_Reg5;
		      s_Reg6      <= s_Reg6;
		      s_Reg7      <= w_InstImm;
		   end

		   default: begin
		      s_State     <= `RegBankS8_State_Error;
		      s_OutSelect <= 0;
		      s_Reg0      <= 0;
		      s_Reg1      <= 0;
		      s_Reg2      <= 0;
		      s_Reg3      <= 0;
		      s_Reg4      <= 0;
		      s_Reg5      <= 0;
		      s_Reg6      <= 0;
		      s_Reg7      <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 s_State     <= `RegBankS8_State_Ready;
		 s_OutSelect <= s_OutSelect;
		 s_Reg0      <= s_Reg0;
		 s_Reg1      <= s_Reg1;
		 s_Reg2      <= s_Reg2;
		 s_Reg3      <= s_Reg3;
		 s_Reg4      <= s_Reg4;
		 s_Reg5      <= s_Reg5;
		 s_Reg6      <= s_Reg6;
		 s_Reg7      <= s_Reg7;
	      end // else: !if(inst_en)
	   end // case: `RegBankS8_State_Ready

	   `RegBankS8_State_Error: begin
	      s_State     <= `RegBankS8_State_Error;
	      s_OutSelect <= 0;
	      s_Reg0      <= 0;
	      s_Reg1      <= 0;
	      s_Reg2      <= 0;
	      s_Reg3      <= 0;
	      s_Reg4      <= 0;
	      s_Reg5      <= 0;
	      s_Reg6      <= 0;
	      s_Reg7      <= 0;
	   end

	   default: begin
	      s_State     <= `RegBankS8_State_Error;
	      s_OutSelect <= 0;
	      s_Reg0      <= 0;
	      s_Reg1      <= 0;
	      s_Reg2      <= 0;
	      s_Reg3      <= 0;
	      s_Reg4      <= 0;
	      s_Reg5      <= 0;
	      s_Reg6      <= 0;
	      s_Reg7      <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      if (inst_en) begin
	 case (w_InstCode)
	   `RegBankS8_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `RegBankS8_RDO: begin
	      $sformat(d_Input,"EN (RDO %1X)",w_InstOutSelect);
	   end

	   `RegBankS8_LD0: begin
	      $sformat(d_Input,"EN (LD0 %2X)",w_InstImm);
	   end

	   `RegBankS8_LD1: begin
	      $sformat(d_Input,"EN (LD1 %2X)",w_InstImm);
	   end

	   `RegBankS8_LD2: begin
	      $sformat(d_Input,"EN (LD2 %2X)",w_InstImm);
	   end

	   `RegBankS8_LD3: begin
	      $sformat(d_Input,"EN (LD3 %2X)",w_InstImm);
	   end

	   `RegBankS8_LD4: begin
	      $sformat(d_Input,"EN (LD4 %2X)",w_InstImm);
	   end

	   `RegBankS8_LD5: begin
	      $sformat(d_Input,"EN (LD5 %2X)",w_InstImm);
	   end

	   `RegBankS8_LD6: begin
	      $sformat(d_Input,"EN (LD6 %2X)",w_InstImm);
	   end

	   `RegBankS8_LD7: begin
	      $sformat(d_Input,"EN (LD7 %2X)",w_InstImm);
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
	`RegBankS8_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`RegBankS8_State_Ready: begin
	   $sformat(d_State,"R %1X %2X %2X %2X %2X %2X %2X %2X %2X",s_OutSelect,s_Reg0,s_Reg1,s_Reg2,s_Reg3,s_Reg4,s_Reg5,s_Reg6,s_Reg7);
	end

	`RegBankS8_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // RegBankS8
