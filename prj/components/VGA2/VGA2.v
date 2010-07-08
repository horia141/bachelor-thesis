`define VGA2_NOP 4'h0
`define VGA2_LDR 4'h1
`define VGA2_LDC 4'h2
`define VGA2_LDD 4'h3
`define VGA2_LDI 4'h4

`define VGA2_State_Reset 2'h0
`define VGA2_State_Ready 2'h1
`define VGA2_State_Error 2'h2

module VGA2(clock,reset,inst,inst_en,vga_hsync,vga_vsync,vga_r,vga_g,vga_b);
   input wire        clock;
   input wire        reset;

   input wire [11:0] inst;
   input wire        inst_en;

   output wire       vga_hsync;
   output wire       vga_vsync;
   output wire       vga_r;
   output wire       vga_g;
   output wire       vga_b;

   reg [1:0]         s_State;
   reg [7:0]         s_FrameBufferRow;
   reg [7:0]         s_FrameBufferCol;
   reg [3:0]         s_FrameBufferData;
   reg               s_WriteData;
   wire [15:0]       i_FrameBufferAddr;
   wire [21:0]       i_VGAIntfAddr;

   wire [3:0]        w_InstCode;
   wire [7:0]        w_InstImm;

   wire              rama0_data_o1;
   wire              rama1_data_o1;
   wire              ramr0_data_o1;
   wire              ramr1_data_o1;
   wire              ramg0_data_o1;
   wire              ramg1_data_o1;
   wire              ramb0_data_o1;
   wire              ramb1_data_o1;

   wire [10:0]       vgaintf_fb_addr_h;
   wire [10:0]       vgaintf_fb_addr_v;
   

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   assign i_FrameBufferAddr = s_FrameBufferRow * 200 + s_FrameBufferCol;
   assign i_VGAIntfAddr = vgaintf_fb_addr_v * 800 + vgaintf_fb_addr_h;

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   rama0 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & ~i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[3]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(rama0_data_o1));

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   rama1 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[3]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(rama1_data_o1));

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   ramr0 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & ~i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[2]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(ramr0_data_o1));

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   ramr1 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[2]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(ramr1_data_o1));

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   ramg0 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & ~i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[1]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(ramg0_data_o1));

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   ramg1 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[1]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(ramg1_data_o1));

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   ramb0 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & ~i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[0]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(ramb0_data_o1));

   RAMDP #(.AddrSize(14),
           .DataSize(1))
   ramb1 (.clock(clock),
          .reset(reset),

          .we(s_WriteData & i_FrameBufferAddr[0]),
          .data_i(s_FrameBufferData[0]),
          .addr0(i_FrameBufferAddr[14:1]),
          .addr1(i_VGAIntfAddr[18:5]),

          .data_o1(ramb1_data_o1));

   VGA2Interface #(.HAddrSize(11),
                   .HVisibleArea(800),
                   .HFrontPorch(56),
                   .HSyncPulse(120),
                   .HBackPorch(64),
                   .VAddrSize(11),
                   .VVisibleArea(600),
                   .VFrontPorch(37),
                   .VSyncPulse(6),
                   .VBackPorch(23))
   vgaintf (.clock(clock),
            .reset(reset),

            .color_r(~i_VGAIntfAddr[0] ? 
                     (rama0_data_o1 ? (~vgaintf_fb_addr_h[0]) & (~vgaintf_fb_addr_v[0]) & ramr0_data_o1 : ramr0_data_o1) :
                     (rama0_data_o1 ? (~vgaintf_fb_addr_h[0]) & (~vgaintf_fb_addr_v[0]) & ramr1_data_o1 : ramr1_data_o1)),
            .color_g(~i_VGAIntfAddr[0] ? 
                     (rama0_data_o1 ? (~vgaintf_fb_addr_h[0]) & (~vgaintf_fb_addr_v[0]) & ramg0_data_o1 : ramg0_data_o1) :
                     (rama0_data_o1 ? (~vgaintf_fb_addr_h[0]) & (~vgaintf_fb_addr_v[0]) & ramg1_data_o1 : ramg1_data_o1)),
            .color_b(~i_VGAIntfAddr[0] ? 
                     (rama0_data_o1 ? (~vgaintf_fb_addr_h[0]) & (~vgaintf_fb_addr_v[0]) & ramb0_data_o1 : ramb0_data_o1) :
                     (rama0_data_o1 ? (~vgaintf_fb_addr_h[0]) & (~vgaintf_fb_addr_v[0]) & ramb1_data_o1 : ramb1_data_o1)),

            .fb_addr_h(vgaintf_fb_addr_h),
            .fb_addr_v(vgaintf_fb_addr_v),

            .vga_hsync(vga_hsync),
            .vga_vsync(vga_vsync),
            .vga_r(vga_r),
            .vga_g(vga_g),
            .vga_b(vga_b));

   always @ (posedge clock) begin
      if (reset) begin
         s_State           <= `VGA2_State_Reset;
         s_FrameBufferCol  <= 0;
         s_FrameBufferRow  <= 0;
         s_FrameBufferData <= 0;
         s_WriteData       <= 0;
      end
      else begin
         case (s_State)
           `VGA2_State_Reset: begin
              s_State           <= `VGA2_State_Ready;
              s_FrameBufferRow  <= 0;
              s_FrameBufferCol  <= 0;
              s_FrameBufferData <= 0;
              s_WriteData       <= 0;
           end

           `VGA2_State_Ready: begin
              if (inst_en) begin
                 case (w_InstCode)
                   `VGA2_NOP: begin
                      s_State           <= `VGA2_State_Ready;
                      s_FrameBufferRow  <= s_FrameBufferRow;
                      s_FrameBufferCol  <= s_FrameBufferCol;
                      s_FrameBufferData <= s_FrameBufferData;
                      s_WriteData       <= 0;
                   end

                   `VGA2_LDR: begin
                      s_State           <= `VGA2_State_Ready;
                      s_FrameBufferRow  <= w_InstImm;
                      s_FrameBufferCol  <= s_FrameBufferCol;
                      s_FrameBufferData <= s_FrameBufferData;
                      s_WriteData       <= 0;
                   end

                   `VGA2_LDC: begin
                      s_State           <= `VGA2_State_Ready;
                      s_FrameBufferRow  <= s_FrameBufferRow;
                      s_FrameBufferCol  <= w_InstImm;
                      s_FrameBufferData <= s_FrameBufferData;
                      s_WriteData       <= 0;
                   end

                   `VGA2_LDD: begin
                      s_State           <= `VGA2_State_Ready;
                      s_FrameBufferRow  <= s_FrameBufferRow;
                      s_FrameBufferCol  <= s_FrameBufferCol;
                      s_FrameBufferData <= w_InstImm[3:0];
                      s_WriteData       <= 1;
                   end

                   `VGA2_LDI: begin
                      s_State           <= `VGA2_State_Ready;
                      s_FrameBufferRow  <= s_FrameBufferRow;
                      s_FrameBufferCol  <= s_FrameBufferCol + 1;
                      s_FrameBufferData <= w_InstImm[3:0];
                      s_WriteData       <= 1;
                   end

                   default: begin
                      s_State           <= `VGA2_State_Error;
                      s_FrameBufferRow  <= 0;
                      s_FrameBufferCol  <= 0;
                      s_FrameBufferData <= 0;
                      s_WriteData       <= 0;
                   end
                 endcase // case (w_InstCode)
              end // if (inst_en)
              else begin
                 s_State           <= `VGA2_State_Ready;
                 s_FrameBufferRow  <= s_FrameBufferRow;
                 s_FrameBufferCol  <= s_FrameBufferCol;
                 s_FrameBufferData <= s_FrameBufferData;
                 s_WriteData       <= 0;
              end // else: !if(inst_en)
           end // case: `VGA2_State_Ready

           `VGA2_State_Error: begin
              s_State           <= `VGA2_State_Error;
              s_FrameBufferRow  <= 0;
              s_FrameBufferCol  <= 0;
              s_FrameBufferData <= 0;
              s_WriteData       <= 0;
           end

           default: begin
              s_State           <= `VGA2_State_Error;
              s_FrameBufferRow  <= 0;
              s_FrameBufferCol  <= 0;
              s_FrameBufferData <= 0;
              s_WriteData       <= 0;
           end
         endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

`ifdef SIM
   always @ * begin
      if (inst_en) begin
         case (w_InstCode)
           `VGA2_NOP: begin
              $sformat(d_Input,"EN NOP");
           end

           `VGA2_LDR: begin
              $sformat(d_Input,"EN (LDR %D)",w_InstImm);
           end

           `VGA2_LDC: begin
              $sformat(d_Input,"EN (LDC %D)",w_InstImm);
           end

           `VGA2_LDD: begin
              $sformat(d_Input,"EN (LDD %4B)",w_InstImm[3:0]);
           end

           `VGA2_LDI: begin
              $sformat(d_Input,"EN (LDI %4B)",w_InstImm[3:0]);
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
        `VGA2_State_Reset: begin
           $sformat(d_State,"X");
        end

        `VGA2_State_Ready: begin
 `ifdef SIM_FullState
           $sformat(d_State,"R (%D,%D)@%6X (%D,%D)@%4X %3B %1B",
                    vgaintf_fb_addr_v,
                    vgaintf_fb_addr_h,
                    i_VGAIntfAddr,
                    s_FrameBufferRow,
                    s_FrameBufferCol,
                    i_FrameBufferAddr,
                    s_FrameBufferData,
                    s_WriteData);
 `else
           $sformat(d_State,"R");
 `endif
        end // case: `VGA2_State_Ready

        `VGA2_State_Error: begin
           $sformat(d_State,"E");
        end

        default: begin
           $sformat(d_State,"?");
        end
      endcase // case (s_State)
   end // always @ *
`endif //  `ifdef SIM
endmodule // VGA2
