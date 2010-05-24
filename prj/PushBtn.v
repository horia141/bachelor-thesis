`define PushBtn_NOP  4'h0
`define PushBtn_RDBS 4'h1

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

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

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

		   `PushBtn_RDBS: begin
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
      if (inst_en) begin
	 case (w_inst_code)
	   `PushBtn_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `PushBtn_RDBS: begin
	      $sformat(d_Input,"EN RDBS");
	   end

	   default: begin
	      $sformat(d_Input,"EN (? %2X)",inst[7:0]);
	   end
	 endcase // case (w_inst_code)
      end // if (inst_en)
      else begin
	 $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *

   always @ * begin
      case (s_State)
	`PushBtn_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`PushBtn_State_Ready: begin
	   case ({s_IntButtonStatus,s_OutButtonStatus})
	     2'b00: begin
		$sformat(d_State,"R F F %1B %1B",button,pushbtnint_button_pressed);
	     end

	     2'b01: begin
		$sformat(d_State,"R F T %1B %1B",button,pushbtnint_button_pressed);
	     end

	     2'b10: begin
		$sformat(d_State,"R T F %1B %1B",button,pushbtnint_button_pressed);
	     end

	     2'b11: begin
		$sformat(d_State,"R T T %1B %1B",button,pushbtnint_button_pressed);
	     end
	   endcase // case ({s_IntButtonStatus,s_OutButtonStatus})
	end // case: `PushBtn_State_Ready
	

	`PushBtn_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // PushBtn
