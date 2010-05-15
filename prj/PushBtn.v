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

   reg [1:0] 	     s_State;
   reg 		     s_IntButtonStatus;
   reg 		     s_OutButtonStatus;

   wire [3:0] 	     w_inst_code;

   wire 	     pushbtnint_button_pressed;

   reg [64*8-1:0]    d_s_State;
   reg [64*8-1:0]    d_s_IntButtonStatus;
   reg [64*8-1:0]    d_s_OutButtonStatus;
   reg [64*8-1:0]    d_w_inst_code;

   assign button_status = s_OutButtonStatus;

   assign w_inst_code = inst[11:8];

   PushBtnInterface #(.Wait(DebounceWait),
		      .Size(DebounceSize))
   pushbtnint (.clock(clock),
	       .reset(reset),

	       .button(button),

	       .button_pressed(pushbtnint_button_pressed));

   always @ (posedge clock) begin
      if (reset) begin
	 s_State           <= `PushBtn_State_Reset;
	 s_IntButtonStatus <= 0;
	 s_OutButtonStatus <= 0;
      end
      else begin
	 case (s_State)
	   `PushBtn_State_Reset: begin
	      s_State           <= `PushBtn_State_Ready;
	      s_IntButtonStatus <= 0;
	      s_OutButtonStatus <= 0;
	   end

	   `PushBtn_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `PushBtn_NOP: begin
		      s_State           <= `PushBtn_State_Ready;
		      s_IntButtonStatus <= pushbtnint_button_pressed | s_IntButtonStatus;
		      s_OutButtonStatus <= s_OutButtonStatus;
		   end

		   `PushBtn_RBS: begin
		      s_State           <= `PushBtn_State_Ready;
		      s_IntButtonStatus <= pushbtnint_button_pressed | 0;
		      s_OutButtonStatus <= s_IntButtonStatus;
		   end

		   default: begin
		      s_State           <= `PushBtn_State_Error;
		      s_IntButtonStatus <= 0;
		      s_OutButtonStatus <= 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 s_State           <= `PushBtn_State_Ready;
		 s_IntButtonStatus <= pushbtnint_button_pressed | s_IntButtonStatus;
		 s_OutButtonStatus <= s_OutButtonStatus;
	      end // else: !if(inst_en)
	   end // case: `PushBtn_State_Ready

	   `PushBtn_State_Error: begin
	      s_State           <= `PushBtn_State_Error;
	      s_IntButtonStatus <= 0;
	      s_OutButtonStatus <= 0;
	   end

	   default: begin
	      s_State           <= `PushBtn_State_Error;
	      s_IntButtonStatus <= 0;
	      s_OutButtonStatus <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      case (s_State)
	`PushBtn_State_Reset: d_s_State = "Reset";
	`PushBtn_State_Ready: d_s_State = "Ready";
	`PushBtn_State_Error: d_s_State = "Error";
	default:              d_s_State = "Undefined State ~ Serious Error or PreReset!";
      endcase // case (s_State)
   end

   always @ * begin
      case (s_IntButtonStatus)
	1: d_s_IntButtonStatus = "Triggered";
	0: d_s_IntButtonStatus = "Free";
      endcase // case (s_IntButtonStatus)
   end
   
   always @ * begin
      case (s_OutButtonStatus)
	1: d_s_OutButtonStatus = "Triggered";
	0: d_s_OutButtonStatus = "Free";
      endcase // case (s_OutButtonStatus)
   end
   
   always @ * begin
      case (w_inst_code)
	`PushBtn_NOP: d_w_inst_code = "NOP";
	`PushBtn_RBS: d_w_inst_code = "RBS";
	default:      d_w_inst_code = "Undefined Instruction ~ Serious Error or PreReset!";
      endcase // case (w_inst_code)
   end
endmodule // PushBtn
