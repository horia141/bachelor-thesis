`define VGA_NOP 4'h0
`define VGA_LDI 4'h1
`define VGA_LD0 4'h2
`define VGA_LD1 4'h3
`define VGA_LD2 4'h4
`define VGA_LD3 4'h5
`define VGA_LD4 4'h6
`define VGA_LD5 4'h7
`define VGA_LD6 4'h8
`define VGA_LD7 4'h9

`define VGA_State_Reset 2'h0
`define VGA_State_Ready 2'h1
`define VGA_State_Error 2'h2

module VGA(clock,reset,inst,inst_en,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire        clock;
   input wire        reset;

   input wire [11:0] inst;
   input wire 	     inst_en;

   output wire 	     vga_hsync;
   output wire 	     vga_vsync;
   output wire 	     vga_r;
   output wire 	     vga_g;
   output wire 	     vga_b;

   reg [1:0] 	     s_State;
   reg [7:0] 	     s_FrameBuffer;

   wire [3:0] 	     w_inst_code;
   wire [7:0] 	     w_inst_imm;

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign w_inst_code = inst[11:8];
   assign w_inst_imm = inst[7:0];

   VGAInterfaceBad
   vgaint (.clock(clock),
	   .reset(reset),

	   .framebuffer(s_FrameBuffer),
	   .vga_hsync(vga_hsync),
	   .vga_vsync(vga_vsync),
	   .vga_r(vga_r),
	   .vga_g(vga_g),
	   .vga_b(vga_b));

   always @ (posedge clock) begin
      if (reset) begin
	 s_State       <= `VGA_State_Reset;
	 s_FrameBuffer <= 0;
      end
      else begin
	 case (s_State)
	   `VGA_State_Reset: begin
	      s_State       <= `VGA_State_Ready;
	      s_FrameBuffer <= 0;
	   end

	   `VGA_State_Ready: begin
	      if (inst_en) begin
		 case (w_inst_code)
		   `VGA_NOP: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= s_FrameBuffer;
		   end

		   `VGA_LDI: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= w_inst_imm;
		   end

		   `VGA_LD0: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {s_FrameBuffer[7:1],w_inst_imm[0]};
		   end

		   `VGA_LD1: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {s_FrameBuffer[7:2],w_inst_imm[0],s_FrameBuffer[0:0]};
		   end

		   `VGA_LD2: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {s_FrameBuffer[7:3],w_inst_imm[0],s_FrameBuffer[1:0]};
		   end

		   `VGA_LD3: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {s_FrameBuffer[7:4],w_inst_imm[0],s_FrameBuffer[2:0]};
		   end

		   `VGA_LD4: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {s_FrameBuffer[7:5],w_inst_imm[0],s_FrameBuffer[3:0]};
		   end

		   `VGA_LD5: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {s_FrameBuffer[7:6],w_inst_imm[0],s_FrameBuffer[4:0]};
		   end

		   `VGA_LD6: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {s_FrameBuffer[7:7],w_inst_imm[0],s_FrameBuffer[5:0]};
		   end

		   `VGA_LD7: begin
		      s_State       <= `VGA_State_Ready;
		      s_FrameBuffer <= {w_inst_imm[0],s_FrameBuffer[6:0]};
		   end

		   default: begin
		      s_State       <= `VGA_State_Error;
		      s_FrameBuffer <= 0;
		   end
		 endcase // case (w_inst_code)
	      end // if (inst_en)
	      else begin
		 s_State       <= `VGA_State_Ready;
		 s_FrameBuffer <= s_FrameBuffer;
	      end // else: !if(inst_en)
	   end // case: `VGA_State_Ready

	   `VGA_State_Error: begin
	      s_State       <= `VGA_State_Error;
	      s_FrameBuffer <= 0;
	   end

	   default: begin
	      s_State       <= `VGA_State_Error;
	      s_FrameBuffer <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ * begin
      if (inst_en) begin
	 case (w_inst_code)
	   `VGA_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `VGA_LDI: begin
	      $sformat(d_Input,"EN (LDI %8B)",w_inst_imm);
	   end

	   `VGA_LD0: begin
	      $sformat(d_Input,"EN (LD0 %1B)",w_inst_imm[0]);
	   end

	   `VGA_LD1: begin
	      $sformat(d_Input,"EN (LD1 %1B)",w_inst_imm[0]);
	   end

	   `VGA_LD2: begin
	      $sformat(d_Input,"EN (LD2 %1B)",w_inst_imm[0]);
	   end

	   `VGA_LD3: begin
	      $sformat(d_Input,"EN (LD3 %1B)",w_inst_imm[0]);
	   end

	   `VGA_LD4: begin
	      $sformat(d_Input,"EN (LD4 %1B)",w_inst_imm[0]);
	   end

	   `VGA_LD5: begin
	      $sformat(d_Input,"EN (LD5 %1B)",w_inst_imm[0]);
	   end

	   `VGA_LD6: begin
	      $sformat(d_Input,"EN (LD6 %1B)",w_inst_imm[0]);
	   end

	   `VGA_LD7: begin
	      $sformat(d_Input,"EN (LD7 %1B)",w_inst_imm[0]);
	   end

	   default: begin
	      $sformat(d_Input,"EN (? %8B)",w_inst_imm);
	   end
	 endcase // case (w_inst_code)
      end // if (inst_en)
      else begin
	 $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *

   always @ * begin
      case (s_State)
	`VGA_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`VGA_State_Ready: begin
	   $sformat(d_State,"R %8B",s_FrameBuffer);
	end

	`VGA_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // VGA
