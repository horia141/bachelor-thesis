`define PushBtnBank_NOP 4'h0
`define PushBtnBank_RSA 4'h1
`define PushBtnBank_RS0 4'h2
`define PushBtnBank_RS1 4'h3
`define PushBtnBank_RS2 4'h4
`define PushBtnBank_RS3 4'h5

`define PushBtnBank_State_Reset 4'h0
`define PushBtnBank_State_Ready 4'h1
`define PushBtnBank_State_Error 4'h2

module PushBtnBank(clock,reset,inst,inst_en,btn0,btn1,btn2,btn3,btn_state);
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_en;
   input wire        btn0;
   input wire 	     btn1;
   input wire 	     btn2;
   input wire 	     btn3;

   output wire [3:0] btn_state;

   reg [1:0] 	     c_State;
   reg 		     c_Btn0Pushed;
   reg 		     c_Btn1Pushed;
   reg 		     c_Btn2Pushed;
   reg 		     c_Btn3Pushed;
   reg [3:0] 	     c_BtnState;

   reg [1:0] 	     n_State;
   reg 		     n_Btn0Pushed;
   reg 		     n_Btn1Pushed;
   reg 		     n_Btn2Pushed;
   reg 		     n_Btn3Pushed;
   reg [3:0] 	     n_BtnState;

   wire [3:0] 	     w_inst_code;

   reg [64*8-1:0]    d_c_State;
   reg [64*8-1:0]    d_n_State;
   reg [64*8-1:0]    d_c_Btn0Pushed;
   reg [64*8-1:0]    d_n_Btn0Pushed;
   reg [64*8-1:0]    d_c_Btn1Pushed;
   reg [64*8-1:0]    d_n_Btn1Pushed;
   reg [64*8-1:0]    d_c_Btn2Pushed;
   reg [64*8-1:0]    d_n_Btn2Pushed;
   reg [64*8-1:0]    d_c_Btn3Pushed;
   reg [64*8-1:0]    d_n_Btn3Pushed;
   reg [64*8-1:0]    d_w_inst_code;
 		     
   assign btn_state = n_BtnState;

   assign w_inst_code = inst[11:8];

   always @ (posedge clock) begin
      c_State      <= n_State;
      c_Btn0Pushed <= n_Btn0Pushed;
      c_Btn1Pushed <= n_Btn1Pushed;
      c_Btn2Pushed <= n_Btn2Pushed;
      c_Btn3Pushed <= n_Btn3Pushed;
      c_BtnState   <= n_BtnState;
   end

   always @ * begin
      if (reset) begin
	 n_State    = `PushBtnBank_State_Reset;
	 n_Btn0Pushed = 0;
	 n_Btn1Pushed = 0;
	 n_Btn2Pushed = 0;
	 n_Btn3Pushed = 0;
	 n_BtnState   = 0;
      end
      else begin
	 case (c_State)
	   `PushBtnBank_State_Reset: begin
	      n_State      = `PushBtnBank_State_Ready;
	      n_Btn0Pushed = 0;
	      n_Btn1Pushed = 0;
	      n_Btn2Pushed = 0;
	      n_Btn3Pushed = 0;
	      n_BtnState   = 0;
	   end

	   `PushBtnBank_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `PushBtnBank_NOP: begin
		      n_State      = `PushBtnBank_State_Ready;
		      n_Btn0Pushed = btn0 | c_Btn0Pushed;
		      n_Btn1Pushed = btn1 | c_Btn1Pushed;
		      n_Btn2Pushed = btn2 | c_Btn2Pushed;
		      n_Btn3Pushed = btn3 | c_Btn3Pushed;
		      n_BtnState   = c_BtnState;
		   end

		   `PushBtnBank_RSA: begin
		      n_State      = `PushBtnBank_State_Ready;
		      n_Btn0Pushed = btn0 | 0;
		      n_Btn1Pushed = btn1 | 0;
		      n_Btn2Pushed = btn2 | 0;
		      n_Btn3Pushed = btn3 | 0;
		      n_BtnState   = {c_Btn3Pushed,c_Btn2Pushed,c_Btn1Pushed,c_Btn0Pushed};
		   end

		   `PushBtnBank_RS0: begin
		      n_State      = `PushBtnBank_State_Ready;
		      n_Btn0Pushed = btn0 | 0;
		      n_Btn1Pushed = btn1 | c_Btn1Pushed;
		      n_Btn2Pushed = btn2 | c_Btn2Pushed;
		      n_Btn3Pushed = btn3 | c_Btn3Pushed;
		      n_BtnState   = {c_BtnState[3:1],c_Btn0Pushed};
		   end

		   `PushBtnBank_RS1: begin
		      n_State      = `PushBtnBank_State_Ready;
		      n_Btn0Pushed = btn0 | c_Btn0Pushed;
		      n_Btn1Pushed = btn1 | 0;
		      n_Btn2Pushed = btn2 | c_Btn2Pushed;
		      n_Btn3Pushed = btn3 | c_Btn3Pushed;
		      n_BtnState   = {c_BtnState[3:2],c_Btn1Pushed,c_BtnState[0]};
		   end

		   `PushBtnBank_RS2: begin
		      n_State      = `PushBtnBank_State_Ready;
		      n_Btn0Pushed = btn0 | c_Btn0Pushed;
		      n_Btn1Pushed = btn1 | c_Btn1Pushed;
		      n_Btn2Pushed = btn2 | 0;
		      n_Btn3Pushed = btn3 | c_Btn3Pushed;
		      n_BtnState   = {c_BtnState[3],c_Btn2Pushed,c_BtnState[1:0]};
		   end

		   `PushBtnBank_RS3: begin
		      n_State      = `PushBtnBank_State_Ready;
		      n_Btn0Pushed = btn0 | c_Btn0Pushed;
		      n_Btn1Pushed = btn1 | c_Btn1Pushed;
		      n_Btn2Pushed = btn2 | c_Btn2Pushed;
		      n_Btn3Pushed = btn3 | 0;
		      n_BtnState   = {c_Btn3Pushed,c_BtnState[2:0]};
		   end

		   default: begin
		      n_State      = `PushBtnBank_State_Error;
		      n_Btn0Pushed = 0;
		      n_Btn1Pushed = 0;
		      n_Btn2Pushed = 0;
		      n_Btn3Pushed = 0;
		      n_BtnState   = 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 n_State      = `PushBtnBank_State_Ready;
		 n_Btn0Pushed = btn0 | c_Btn0Pushed;
		 n_Btn1Pushed = btn1 | c_Btn1Pushed;
		 n_Btn2Pushed = btn2 | c_Btn2Pushed;
		 n_Btn3Pushed = btn3 | c_Btn3Pushed;
		 n_BtnState   = c_BtnState;
	      end // else: !if(inst_en)
	   end // case: `PushBtnBank_State_Ready

	   `PushBtnBank_State_Error: begin
	      n_State      = `PushBtnBank_State_Error;
	      n_Btn0Pushed = 0;
	      n_Btn1Pushed = 0;
	      n_Btn2Pushed = 0;
	      n_Btn3Pushed = 0;
	      n_BtnState   = 0;
	   end

	   default: begin
	      n_State      = `PushBtnBank_State_Error;
	      n_Btn0Pushed = 0;
	      n_Btn1Pushed = 0;
	      n_Btn2Pushed = 0;
	      n_Btn3Pushed = 0;
	      n_BtnState   = 0;
	   end
	 endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`PushBtnBank_State_Reset: d_c_State = "Reset";
	`PushBtnBank_State_Ready: d_c_State = "Ready";
	`PushBtnBank_State_Error: d_c_State = "Error";
	default:                  d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`PushBtnBank_State_Reset: d_n_State = "Reset";
	`PushBtnBank_State_Ready: d_n_State = "Ready";
	`PushBtnBank_State_Error: d_n_State = "Error";
	default:                  d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
   end

   always @ * begin
      case (c_Btn0Pushed)
	0: d_c_Btn0Pushed = "Released";
	1: d_c_Btn0Pushed = "Pushed";
      endcase // case (c_Btn0Pushed)
   end

   always @ * begin
      case (n_Btn0Pushed)
	0: d_n_Btn0Pushed = "Released";
	1: d_n_Btn0Pushed = "Pushed";
      endcase // case (n_Btn0Pushed)
   end

   always @ * begin
      case (c_Btn1Pushed)
	0: d_c_Btn1Pushed = "Released";
	1: d_c_Btn1Pushed = "Pushed";
      endcase // case (c_Btn1Pushed)
   end

   always @ * begin
      case (n_Btn1Pushed)
	0: d_n_Btn1Pushed = "Released";
	1: d_n_Btn1Pushed = "Pushed";
      endcase // case (n_Btn1Pushed)
   end

   always @ * begin
      case (c_Btn2Pushed)
	0: d_c_Btn2Pushed = "Released";
	1: d_c_Btn2Pushed = "Pushed";
      endcase // case (c_Btn2Pushed)
   end

   always @ * begin
      case (n_Btn2Pushed)
	0: d_n_Btn2Pushed = "Released";
	1: d_n_Btn2Pushed = "Pushed";
      endcase // case (n_Btn2Pushed)
   end

   always @ * begin
      case (c_Btn3Pushed)
	0: d_c_Btn3Pushed = "Released";
	1: d_c_Btn3Pushed = "Pushed";
      endcase // case (c_Btn3Pushed)
   end

   always @ * begin
      case (n_Btn3Pushed)
	0: d_n_Btn3Pushed = "Released";
	1: d_n_Btn3Pushed = "Pushed";
      endcase // case (n_Btn3Pushed)
   end
   
   always @ * begin
      case (w_inst_code)
	`PushBtnBank_NOP: d_w_inst_code = "NOP";
	`PushBtnBank_RSA: d_w_inst_code = "RSA";
	`PushBtnBank_RS0: d_w_inst_code = "RS0";
	`PushBtnBank_RS1: d_w_inst_code = "RS1";
	`PushBtnBank_RS2: d_w_inst_code = "RS2";
	`PushBtnBank_RS3: d_w_inst_code = "RS3";
	default:          d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end // always @ *
endmodule // PushBtnBank
