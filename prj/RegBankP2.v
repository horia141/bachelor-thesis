`define RegBankP2_NOP 4'h0
`define RegBankP2_LD0 4'h1
`define RegBankP2_LD1 4'h2

`define RegBankP2_State_Reset 2'h0
`define RegBankP2_State_Ready 2'h1
`define RegBankP2_State_Error 2'h2

module RegBankP2(clock,reset,inst,inst_en,out_0,out_1);
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   output wire [7:0] out_0;
   output wire [7:0] out_1;

   reg [1:0] 	     s_State;
   reg [7:0] 	     s_Reg0;
   reg [7:0] 	     s_Reg1;

   wire [3:0] 	     w_InstCode;
   wire [7:0] 	     w_InstImm;

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign out_0 = s_Reg0;
   assign out_1 = s_Reg1;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   always @ (posedge clock) begin
      if (reset) begin
	 s_State <= `RegBankP2_State_Reset;
	 s_Reg0  <= 0;
	 s_Reg1  <= 0;
      end
      else begin
	 case (s_State)
	   `RegBankP2_State_Reset: begin
	      s_State <= `RegBankP2_State_Ready;
	      s_Reg0  <= 0;
	      s_Reg1  <= 0;
	   end

	   `RegBankP2_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `RegBankP2_NOP: begin
		      s_State <= `RegBankP2_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		   end

		   `RegBankP2_LD0: begin
		      s_State <= `RegBankP2_State_Ready;
		      s_Reg0  <= w_InstImm;
		      s_Reg1  <= s_Reg1;
		   end

		   `RegBankP2_LD1: begin
		      s_State <= `RegBankP2_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= w_InstImm;
		   end

		   default: begin
		      s_State <= `RegBankP2_State_Error;
		      s_Reg0  <= 0;
		      s_Reg1  <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 s_State <= `RegBankP2_State_Ready;
		 s_Reg0  <= s_Reg0;
		 s_Reg1  <= s_Reg1;
	      end // else: !if(inst_en)
	   end // case: `RegBankP2_State_Ready

	   `RegBankP2_State_Error: begin
	      s_State <= `RegBankP2_State_Error;
	      s_Reg0  <= 0;
	      s_Reg1  <= 0;
	   end

	   default: begin
	      s_State <= `RegBankP2_State_Error;
	      s_Reg0  <= 0;
	      s_Reg1  <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      if (inst_en) begin
	 case (w_InstCode)
	   `RegBankP2_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `RegBankP2_LD0: begin
	      $sformat(d_Input,"EN (LD0 %2X)",w_InstImm);
	   end

	   `RegBankP2_LD1: begin
	      $sformat(d_Input,"EN (LD1 %2X)",w_InstImm);
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
	`RegBankP2_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`RegBankP2_State_Ready: begin
	   $sformat(d_State,"R %2X %2X",s_Reg0,s_Reg1);
	end

	`RegBankP2_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // RegBankP2
