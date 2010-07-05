`define RegBankP8_NOP 4'h0
`define RegBankP8_LD0 4'h1
`define RegBankP8_LD1 4'h2
`define RegBankP8_LD2 4'h3
`define RegBankP8_LD3 4'h4
`define RegBankP8_LD4 4'h5
`define RegBankP8_LD5 4'h6
`define RegBankP8_LD6 4'h7
`define RegBankP8_LD7 4'h8

`define RegBankP8_State_Reset 2'h0
`define RegBankP8_State_Ready 2'h1
`define RegBankP8_State_Error 2'h2

module RegBankP8(clock,reset,inst,inst_en,out_0,out_1,out_2,out_3,out_4,out_5,out_6,out_7);
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   output wire [7:0] out_0;
   output wire [7:0] out_1;
   output wire [7:0] out_2;
   output wire [7:0] out_3;
   output wire [7:0] out_4;
   output wire [7:0] out_5;
   output wire [7:0] out_6;
   output wire [7:0] out_7;

   reg [1:0] 	     s_State;
   reg [7:0] 	     s_Reg0;
   reg [7:0] 	     s_Reg1;
   reg [7:0] 	     s_Reg2;
   reg [7:0] 	     s_Reg3;
   reg [7:0] 	     s_Reg4;
   reg [7:0] 	     s_Reg5;
   reg [7:0] 	     s_Reg6;
   reg [7:0] 	     s_Reg7;

   wire [3:0] 	     w_InstCode;
   wire [7:0] 	     w_InstImm;

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign out_0 = s_Reg0;
   assign out_1 = s_Reg1;
   assign out_2 = s_Reg2;
   assign out_3 = s_Reg3;
   assign out_4 = s_Reg4;
   assign out_5 = s_Reg5;
   assign out_6 = s_Reg6;
   assign out_7 = s_Reg7;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   always @ (posedge clock) begin
      if (reset) begin
	 s_State <= `RegBankP8_State_Reset;
	 s_Reg0  <= 0;
	 s_Reg1  <= 0;
	 s_Reg2  <= 0;
	 s_Reg3  <= 0;
	 s_Reg4  <= 0;
	 s_Reg5  <= 0;
	 s_Reg6  <= 0;
	 s_Reg7  <= 0;
      end
      else begin
	 case (s_State)
	   `RegBankP8_State_Reset: begin
	      s_State <= `RegBankP8_State_Ready;
	      s_Reg0  <= 0;
	      s_Reg1  <= 0;
	      s_Reg2  <= 0;
	      s_Reg3  <= 0;
	      s_Reg4  <= 0;
	      s_Reg5  <= 0;
	      s_Reg6  <= 0;
	      s_Reg7  <= 0;
	   end

	   `RegBankP8_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `RegBankP8_NOP: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_NOP

		   `RegBankP8_LD0: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= w_InstImm;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_LD0

		   `RegBankP8_LD1: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= w_InstImm;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_LD1

		   `RegBankP8_LD2: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= w_InstImm;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_LD2

		   `RegBankP8_LD3: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= w_InstImm;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_LD3

		   `RegBankP8_LD4: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= w_InstImm;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_LD3

		   `RegBankP8_LD5: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= w_InstImm;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_LD3

		   `RegBankP8_LD6: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= w_InstImm;
		      s_Reg7  <= s_Reg7;
		   end // case: `RegBankP8_LD6

		   `RegBankP8_LD7: begin
		      s_State <= `RegBankP8_State_Ready;
		      s_Reg0  <= s_Reg0;
		      s_Reg1  <= s_Reg1;
		      s_Reg2  <= s_Reg2;
		      s_Reg3  <= s_Reg3;
		      s_Reg4  <= s_Reg4;
		      s_Reg5  <= s_Reg5;
		      s_Reg6  <= s_Reg6;
		      s_Reg7  <= w_InstImm;
		   end // case: `RegBankP8_LD6

		   default: begin
		      s_State <= `RegBankP8_State_Error;
		      s_Reg0  <= 0;
		      s_Reg1  <= 0;
		      s_Reg2  <= 0;
		      s_Reg3  <= 0;
		      s_Reg4  <= 0;
		      s_Reg5  <= 0;
		      s_Reg6  <= 0;
		      s_Reg7  <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 s_State <= `RegBankP8_State_Ready;
		 s_Reg0  <= s_Reg0;
		 s_Reg1  <= s_Reg1;
		 s_Reg2  <= s_Reg2;
		 s_Reg3  <= s_Reg3;
		 s_Reg4  <= s_Reg4;
		 s_Reg5  <= s_Reg5;
		 s_Reg6  <= s_Reg6;
		 s_Reg7  <= s_Reg7;
	      end // else: !if(inst_en)
	   end // case: `RegBankP8_State_Ready

	   `RegBankP8_State_Error: begin
	      s_State <= `RegBankP8_State_Error;
	      s_Reg0  <= 0;
	      s_Reg1  <= 0;
	      s_Reg2  <= 0;
	      s_Reg3  <= 0;
	      s_Reg4  <= 0;
	      s_Reg5  <= 0;
	      s_Reg6  <= 0;
	      s_Reg7  <= 0;
	   end

	   default: begin
	      s_State <= `RegBankP8_State_Error;
	      s_Reg0  <= 0;
	      s_Reg1  <= 0;
	      s_Reg2  <= 0;
	      s_Reg3  <= 0;
	      s_Reg4  <= 0;
	      s_Reg5  <= 0;
	      s_Reg6  <= 0;
	      s_Reg7  <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

`ifdef SIM
   always @ * begin
      if (inst_en) begin
	 case (w_InstCode)
	   `RegBankP8_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `RegBankP8_LD0: begin
	      $sformat(d_Input,"EN (LD0 %2X)",w_InstImm);
	   end

	   `RegBankP8_LD1: begin
	      $sformat(d_Input,"EN (LD1 %2X)",w_InstImm);
	   end

	   `RegBankP8_LD2: begin
	      $sformat(d_Input,"EN (LD2 %2X)",w_InstImm);
	   end

	   `RegBankP8_LD3: begin
	      $sformat(d_Input,"EN (LD3 %2X)",w_InstImm);
	   end

	   `RegBankP8_LD4: begin
	      $sformat(d_Input,"EN (LD4 %2X)",w_InstImm);
	   end

	   `RegBankP8_LD5: begin
	      $sformat(d_Input,"EN (LD5 %2X)",w_InstImm);
	   end

	   `RegBankP8_LD6: begin
	      $sformat(d_Input,"EN (LD6 %2X)",w_InstImm);
	   end

	   `RegBankP8_LD7: begin
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
	`RegBankP8_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`RegBankP8_State_Ready: begin
	   $sformat(d_State,"R %2X %2X %2X %2X %2X %2X %2X %2X",s_Reg0,s_Reg1,s_Reg2,s_Reg3,s_Reg4,s_Reg5,s_Reg6,s_Reg7);
	end

	`RegBankP8_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
`endif //  `ifdef SIM
endmodule // RegBankP8
