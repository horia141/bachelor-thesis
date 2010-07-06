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
   input wire        inst_en;

   output wire [7:0] leds;

   reg [1:0]         s_State;
   reg [7:0]         s_Leds;

   wire [3:0]        w_InstCode;
   wire [7:0]        w_InstImm;

   reg [256*8-1:0]   d_Input;
   reg [256*8-1:0]   d_State;

   assign leds = s_Leds;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   always @ (posedge clock) begin
      if (reset) begin
         s_State <= `LedBank_State_Reset;
         s_Leds  <= 0;
      end
      else begin
         case (s_State)
           `LedBank_State_Reset: begin
              s_State <= `LedBank_State_Ready;
              s_Leds  <= 0;
           end

           `LedBank_State_Ready: begin
              if (inst_en) begin
                 case (w_InstCode)
                   `LedBank_NOP: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= s_Leds;
                   end

                   `LedBank_LDI: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= w_InstImm;
                   end

                   `LedBank_LD0: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {s_Leds[7:1],w_InstImm[0]};
                   end

                   `LedBank_LD1: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {s_Leds[7:2],w_InstImm[0],s_Leds[0:0]};
                   end

                   `LedBank_LD2: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {s_Leds[7:3],w_InstImm[0],s_Leds[1:0]};
                   end

                   `LedBank_LD3: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {s_Leds[7:4],w_InstImm[0],s_Leds[2:0]};
                   end

                   `LedBank_LD4: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {s_Leds[7:5],w_InstImm[0],s_Leds[3:0]};
                   end

                   `LedBank_LD5: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {s_Leds[7:6],w_InstImm[0],s_Leds[4:0]};
                   end

                   `LedBank_LD6: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {s_Leds[7:7],w_InstImm[0],s_Leds[5:0]};
                   end

                   `LedBank_LD7: begin
                      s_State <= `LedBank_State_Ready;
                      s_Leds  <= {w_InstImm[0],s_Leds[6:0]};
                   end

                   default: begin
                      s_State <= `LedBank_State_Error;
                      s_Leds  <= 0;
                   end
                 endcase // case (w_InstCode)
              end // if (inst_en)
              else begin
                 s_State <= `LedBank_State_Ready;
                 s_Leds  <= s_Leds;
              end // else: !if(inst_en)
           end // case: `LedBank_State_Ready

           `LedBank_State_Error: begin
              s_State <= `LedBank_State_Error;
              s_Leds  <= 0;
           end

           default: begin
              s_State <= `LedBank_State_Error;
              s_Leds  <= 0;
           end
         endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

`ifdef SIM
   always @ * begin
      if (inst_en) begin
         case (w_InstCode)
           `LedBank_NOP: begin
              $sformat(d_Input,"EN NOP");
           end

           `LedBank_LDI: begin
              $sformat(d_Input,"EN (LDI %8B)",w_InstImm);
           end

           `LedBank_LD0: begin
              $sformat(d_Input,"EN (LD0 %1B)",w_InstImm[0]);
           end

           `LedBank_LD1: begin
              $sformat(d_Input,"EN (LD1 %1B)",w_InstImm[0]);
           end

           `LedBank_LD2: begin
              $sformat(d_Input,"EN (LD2 %1B)",w_InstImm[0]);
           end

           `LedBank_LD3: begin
              $sformat(d_Input,"EN (LD3 %1B)",w_InstImm[0]);
           end

           `LedBank_LD4: begin
              $sformat(d_Input,"EN (LD4 %1B)",w_InstImm[0]);
           end

           `LedBank_LD5: begin
              $sformat(d_Input,"EN (LD5 %1B)",w_InstImm[0]);
           end

           `LedBank_LD6: begin
              $sformat(d_Input,"EN (LD6 %1B)",w_InstImm[0]);
           end

           `LedBank_LD7: begin
              $sformat(d_Input,"EN (LD7 %1B)",w_InstImm[0]);
           end

           default: begin
              $sformat(d_Input,"EN (? %8B)",w_InstImm);
           end
         endcase // case (w_InstCode)
      end // if (inst_en)
      else begin
         $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *

   always @ * begin
      case (s_State)
        `LedBank_State_Reset: begin
           $sformat(d_State,"X");
        end

        `LedBank_State_Ready: begin
           $sformat(d_State,"R %8B",s_Leds);
        end

        `LedBank_State_Error: begin
           $sformat(d_State,"E");
        end

        default: begin
           $sformat(d_State,"?");
        end
      endcase // case (s_State)
   end // always @ *
`endif //  `ifdef SIM
endmodule // LedBank
