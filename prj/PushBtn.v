`define PushBtn_NOP 4'h0
`define PushBtn_RBS 4'h1

`define PushBtn_State_Reset 2'h0
`define PushBtn_State_Ready 2'h1
`define PushBtn_State_Error 2'h2 

module PushBtn(clock,reset,inst,inst_en,button,button_status);
   parameter         DebounceWait = 40000;
   parameter         DebounceSize = 16;
   
   input wire        clock;
   input wire 	     reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   input wire 	     button;

   output wire 	     button_status;

   reg [1:0] 	     c_State;
   reg 		     c_IntButtonStatus;
   reg 		     c_OutButtonStatus;

   reg [1:0] 	     n_State;
   reg 		     n_IntButtonStatus;
   reg 		     n_OutButtonStatus;

   wire [3:0] 	     w_inst_code;

   wire 	     pushbtnint_button_pressed;

   reg [64*8-1:0]    d_c_State;
   reg [64*8-1:0]    d_n_State;
   reg [64*8-1:0]    d_c_IntButtonStatus;
   reg [64*8-1:0]    d_n_IntButtonStatus;
   reg [64*8-1:0]    d_c_OutButtonStatus;
   reg [64*8-1:0]    d_n_OutButtonStatus;
   reg [64*8-1:0]    d_w_inst_code;

   assign button_status = n_OutButtonStatus;

   assign w_inst_code = inst[11:8];

   PushBtnInterface #(.Wait(DebounceWait),
		      .Size(DebounceSize))
   pushbtnint (.clock(clock),
	       .reset(reset),

	       .button(button),

	       .button_pressed(pushbtnint_button_pressed));

   always @ (posedge clock) begin
      c_State <= n_State;
      c_IntButtonStatus <= n_IntButtonStatus;
      c_OutButtonStatus <= n_OutButtonStatus;
   end

   always @ * begin
      if (reset) begin
	 n_State           = `PushBtn_State_Reset;
	 n_IntButtonStatus = 0;
	 n_OutButtonStatus = 0;
      end
      else begin
	 case (c_State)
	   `PushBtn_State_Reset: begin
	      n_State           = `PushBtn_State_Ready;
	      n_IntButtonStatus = 0;
	      n_OutButtonStatus = 0;
	   end

	   `PushBtn_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `PushBtn_NOP: begin
		      n_State           = `PushBtn_State_Ready;
		      n_IntButtonStatus = pushbtnint_button_pressed | c_IntButtonStatus;
		      n_OutButtonStatus = c_OutButtonStatus;
		   end

		   `PushBtn_RBS: begin
		      n_State           = `PushBtn_State_Ready;
		      n_IntButtonStatus = pushbtnint_button_pressed | 0;
		      n_OutButtonStatus = c_IntButtonStatus;
		   end

		   default: begin
		      n_State           = `PushBtn_State_Error;
		      n_IntButtonStatus = 0;
		      n_OutButtonStatus = 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 n_State           = `PushBtn_State_Ready;
		 n_IntButtonStatus = pushbtnint_button_pressed | c_IntButtonStatus;
		 n_OutButtonStatus = c_OutButtonStatus;
	      end // else: !if(inst_en)
	   end // case: `PushBtn_State_Ready

	   `PushBtn_State_Error: begin
	      n_State           = `PushBtn_State_Error;
	      n_IntButtonStatus = 0;
	      n_OutButtonStatus = 0;
	   end

	   default: begin
	      n_State           = `PushBtn_State_Error;
	      n_IntButtonStatus = 0;
	      n_OutButtonStatus = 0;
	   end
	 endcase // case (c_State)
      end // else: !if(reset)
   end // always @ *

   always @ * begin
      case (c_State)
	`PushBtn_State_Reset: d_c_State = "Reset";
	`PushBtn_State_Ready: d_c_State = "Ready";
	`PushBtn_State_Error: d_c_State = "Error";
	default:              d_c_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (c_State)
   end

   always @ * begin
      case (n_State)
	`PushBtn_State_Reset: d_n_State = "Reset";
	`PushBtn_State_Ready: d_n_State = "Ready";
	`PushBtn_State_Error: d_n_State = "Error";
	default:              d_n_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (n_State)
   end

   always @ * begin
      case (c_IntButtonStatus)
	1: d_c_IntButtonStatus = "Pressed";
	0: d_c_IntButtonStatus = "Released";
      endcase // case (c_IntButtonStatus)
   end
   
   always @ * begin
      case (n_IntButtonStatus)
	1: d_n_IntButtonStatus = "Pressed";
	0: d_n_IntButtonStatus = "Released";
      endcase // case (n_IntButtonStatus)
   end
   
   always @ * begin
      case (c_OutButtonStatus)
	1: d_c_OutButtonStatus = "Pressed";
	0: d_c_OutButtonStatus = "Released";
      endcase // case (c_OutButtonStatus)
   end
   
   always @ * begin
      case (n_OutButtonStatus)
	1: d_n_OutButtonStatus = "Pressed";
	0: d_n_OutButtonStatus = "Released";
      endcase // case (n_OutButtonStatus)
   end

   always @ * begin
      case (w_inst_code)
	`PushBtn_NOP: d_w_inst_code = "NOP";
	`PushBtn_RBS: d_w_inst_code = "RBS";
	default:      d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end
endmodule // PushBtn
