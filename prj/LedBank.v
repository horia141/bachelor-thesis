`define LedBank_NOP 4'h0
`define LedBank_LDI 4'h1
`define LedBank_LD0 4'h2
`define LedBank_LD1 4'h3
`define LedBank_LD2 4'h4
`define LedBank_LD3 4'h5
`define LedBank_LD4 4'h6
`define LedBank_LD5 4'h7
`define LedBank_LD6 4'h8
`define LedBank_LD7 4'h9

`define LedBank_State_Reset 2'h0
`define LedBank_State_Ready 2'h1
`define LedBank_State_Error 2'h2

module LedBank(clock,reset,inst,inst_en,leds);
   input wire        clock;
   input wire        reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   output wire [7:0] leds;

   reg [1:0] 	     c_State;
   reg [7:0] 	     c_Leds;

   reg [1:0] 	     n_State;
   reg [7:0] 	     n_Leds;

   wire [3:0] 	     w_inst_code;
   wire [7:0] 	     w_inst_imm;

   reg [64*8-1:0]    d_c_State;
   reg [64*8-1:0]    d_n_State;
   reg [64*8-1:0]    d_w_inst_code;

   assign leds = n_Leds;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm = inst[7:0];

   always @ (posedge clock) begin
      c_State <= n_State;
      c_Leds  <= n_Leds;
   end

   always @ * begin
      if (reset) begin
	 n_State = `LedBank_State_Reset;
	 n_Leds  = 0;
      end
      else begin
	 case (c_State)
	   `LedBank_State_Reset: begin
	      n_State = `LedBank_State_Ready;
	      n_Leds  = 0;
	   end

	   `LedBank_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `LedBank_NOP: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = c_Leds;
		   end

		   `LedBank_LDI: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = w_inst_imm;
		   end

		   `LedBank_LD0: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {c_Leds[7:1],w_inst_imm[0]};
		   end

		   `LedBank_LD1: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {c_Leds[7:2],w_inst_imm[0],c_Leds[0:0]};
		   end

		   `LedBank_LD2: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {c_Leds[7:3],w_inst_imm[0],c_Leds[1:0]};
		   end

		   `LedBank_LD3: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {c_Leds[7:4],w_inst_imm[0],c_Leds[2:0]};
		   end

		   `LedBank_LD4: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {c_Leds[7:5],w_inst_imm[0],c_Leds[3:0]};
		   end

		   `LedBank_LD5: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {c_Leds[7:6],w_inst_imm[0],c_Leds[4:0]};
		   end

		   `LedBank_LD6: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {c_Leds[7:7],w_inst_imm[0],c_Leds[5:0]};
		   end

		   `LedBank_LD7: begin
		      n_State = `LedBank_State_Ready;
		      n_Leds  = {w_inst_imm[0],c_Leds[6:0]};
		   end

		   default: begin
		      n_State = `LedBank_State_Error;
		      n_Leds  = 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 n_State = `LedBank_State_Ready;
		 n_Leds  = c_Leds;
	      end // else: !if(inst_en)
	   end // case: `LedBank_State_Ready

	   `LedBank_State_Error: begin
	      n_State = `LedBank_State_Error;
	      n_Leds  = 0;
	   end

	   default: begin
	      n_State = `LedBank_State_Error;
	      n_Leds  = 0;
	   end
	 endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`LedBank_State_Reset: d_c_State = "Reset";
	`LedBank_State_Ready: d_c_State = "Ready";
	`LedBank_State_Error: d_c_State = "Error";
	default:              d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`LedBank_State_Reset: d_n_State = "Reset";
	`LedBank_State_Ready: d_n_State = "Ready";
	`LedBank_State_Error: d_n_State = "Error";
	default:              d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
   end

   always @ * begin
      case (w_inst_code)
	`LedBank_NOP: d_w_inst_code = "NOP";
	`LedBank_LDI: d_w_inst_code = "LDI";
	`LedBank_LD0: d_w_inst_code = "LD0";
	`LedBank_LD1: d_w_inst_code = "LD1";
	`LedBank_LD2: d_w_inst_code = "LD2";
	`LedBank_LD3: d_w_inst_code = "LD3";
	`LedBank_LD4: d_w_inst_code = "LD4";
	`LedBank_LD5: d_w_inst_code = "LD5";
	`LedBank_LD6: d_w_inst_code = "LD6";
	`LedBank_LD7: d_w_inst_code = "LD7";
	default: d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end // always @ *
endmodule // LedBank
