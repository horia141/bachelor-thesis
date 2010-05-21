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
   wire 	      i_Ready;

   wire [3:0] 	      w_inst_code;
   wire [7:0] 	      w_inst_imm;

   reg [256*8-1:0]    d_Input;
   reg [256*8-1:0]    d_State;

   assign counter = s_Counter;
   assign ready = i_Ready;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm = inst[7:0];

   assign i_Ready = s_Counter == 0;

   always @ (posedge clock) begin
      if (reset) begin
	 s_State    <= `Swc_State_Reset;
	 s_ContInst <= 0;
	 s_Counter  <= 0;
      end
      else begin
	 case (s_State)
	   `Swc_State_Reset: begin
	      s_State    <= `Swc_State_Ready;
	      s_ContInst <= `Swc_NOP;
	      s_Counter  <= 0;
	   end

	   `Swc_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `Swc_NOP: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter;
		   end

		   `Swc_LD0: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= {s_Counter[23:8],w_inst_imm};
		   end
		   
		   `Swc_LD1: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= {s_Counter[23:16],w_inst_imm,s_Counter[7:0]};
		   end

		   `Swc_LD2: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= {w_inst_imm,s_Counter[15:0]};
		   end

		   `Swc_COU: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter + 1;
		   end

		   `Swc_COD: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter - 1;
		   end

		   `Swc_CCU: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_CCU;
		      s_Counter  <= s_Counter + 1;
		   end

		   `Swc_CCD: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_CCD;
		      s_Counter  <= s_Counter - 1;
		   end

		   `Swc_CCS: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter;
		   end

		   default: begin
		      s_State    <= `Swc_State_Error;
		      s_ContInst <= 0;
		      s_Counter  <= 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 case (s_ContInst)
		   `Swc_NOP: begin
		      s_State    <= `Swc_State_Ready;
		      s_ContInst <= `Swc_NOP;
		      s_Counter  <= s_Counter;
		   end

		   `Swc_CCU: begin
		      if (i_Ready) begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_NOP;
			 s_Counter  <= s_Counter;
		      end
		      else begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_CCU;
			 s_Counter  <= s_Counter + 1;
		      end // else: !if(i_Ready)
		   end // case: `Swc_CCU

		   `Swc_CCD: begin
		      if (i_Ready) begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_NOP;
			 s_Counter  <= s_Counter;
		      end
		      else begin
			 s_State    <= `Swc_State_Ready;
			 s_ContInst <= `Swc_CCD;
			 s_Counter  <= s_Counter - 1;
		      end // else: !if(i_Ready)
		   end // case: `Swc_CCD

		   default: begin
		      s_State    <= `Swc_State_Error;
		      s_ContInst <= 0;
		      s_Counter  <= 0;
		   end
		 endcase // case (s_ContInst)
	      end // else: !if(inst_en)
	   end // case: `Swc_State_Reset

	   `Swc_State_Error: begin
	      s_State    <= `Swc_State_Error;
	      s_ContInst <= 0;
	      s_Counter  <= 0;
	   end

	   default: begin
	      s_State    <= `Swc_State_Error;
	      s_ContInst <= 0;
	      s_Counter  <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      if (inst_en) begin
	 case (w_inst_code)
	   `Swc_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `Swc_LD0: begin
	      $sformat(d_Input,"EN (LD0 %2X)",w_inst_imm);
	   end

	   `Swc_LD1: begin
	      $sformat(d_Input,"EN (LD1 %2X)",w_inst_imm);
	   end

	   `Swc_LD2: begin
	      $sformat(d_Input,"EN (LD2 %2X)",w_inst_imm);
	   end

	   `Swc_COU: begin
	      $sformat(d_Input,"EN COU");
	   end

	   `Swc_COD: begin
	      $sformat(d_Input,"EN COD");
	   end

	   `Swc_CCU: begin
	      $sformat(d_Input,"EN CCU");
	   end

	   `Swc_CCD: begin
	      $sformat(d_Input,"EN CCD");
	   end

	   `Swc_CCS: begin
	      $sformat(d_Input,"EN CCS");
	   end

	   default: begin
	      $sformat(d_Input,"EN (? %2X)",w_inst_imm);
	   end
	 endcase // case (w_inst_code)
      end // if (inst_en)
      else begin
	 $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *

   always @ * begin
      case (s_State)
	`Swc_State_Reset: begin
	   $sformat(d_State,"X");
	end
	
	`Swc_State_Ready: begin
	   case (s_ContInst)
	     `Swc_NOP: begin
		if (i_Ready) begin
		   $sformat(d_State,"R NOP %6X Ready",s_Counter);
		end
		else begin
		   $sformat(d_State,"R NOP %6X NotReady",s_Counter);
		end
	     end

	     `Swc_CCU: begin
		if (i_Ready) begin
		   $sformat(d_State,"R CCU %6X Ready",s_Counter);
		end
		else begin
		   $sformat(d_State,"R CCU %6X NotReady",s_Counter);
		end
	     end

	     `Swc_CCD: begin
		if (i_Ready) begin
		   $sformat(d_State,"R CCD %6X Ready",s_Counter);
		end
		else begin
		   $sformat(d_State,"R CCD %6X NotReady",s_Counter);
		end
	     end

	     default: begin
		if (i_Ready) begin
		   $sformat(d_State,"R ? %6X Ready",s_Counter);
		end
		else begin
		   $sformat(d_State,"R ? %6X NotReady",s_Counter);
		end
	     end
	   endcase // case (s_ContInst)
	end // case: `Swc_State_Ready
	
	`Swc_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // Swc
