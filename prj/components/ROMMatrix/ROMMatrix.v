`define ROMMatrix_NOP 4'h0
`define ROMMatrix_LDR 4'h1
`define ROMMatrix_LDC 4'h2

`define ROMMatrix_State_Reset 2'h0
`define ROMMatrix_State_Ready 2'h1
`define ROMMatrix_State_Error 2'h2

module ROMMatrix(clock,reset,inst,inst_en,out,rom_addr,rom_data_o);
   parameter                     ROMRows = 8;
   parameter                     ROMCols = 8;
   parameter                     ROMDataSize = 8;
   
   input wire                    clock;
   input wire                    reset;

   input wire [11:0]             inst;
   input wire                    inst_en;

   output wire [ROMDataSize-1:0] out;

   output wire [15:0]            rom_addr;
   input wire [ROMDataSize-1:0]  rom_data_o;

   reg [1:0]                     s_State;
   reg [7:0]                     s_ROMRow;
   reg [7:0]                     s_ROMCol;

   wire [3:0]                    w_InstCode;
   wire [7:0]                    w_InstImm;

   reg [256*8-1:0]               d_Input;
   reg [256*8-1:0]               d_State;

   assign rom_addr = s_ROMRow * ROMCols + s_ROMCol;
   assign out = rom_data_o;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   always @ (posedge clock) begin
      if (reset) begin
         s_State  <= `ROMMatrix_State_Reset;
         s_ROMRow <= 0;
         s_ROMCol <= 0;
      end
      else begin
         case (s_State)
           `ROMMatrix_State_Reset: begin
              s_State  <= `ROMMatrix_State_Ready;
              s_ROMRow <= 0;
              s_ROMCol <= 0;
           end

           `ROMMatrix_State_Ready: begin
              if (inst_en) begin
                 case (w_InstCode)
                   `ROMMatrix_NOP: begin
                      s_State  <= `ROMMatrix_State_Ready;
                      s_ROMRow <= s_ROMRow;
                      s_ROMCol <= s_ROMCol;
                   end

                   `ROMMatrix_LDR: begin
                      s_State  <= `ROMMatrix_State_Ready;
                      s_ROMRow <= w_InstImm;
                      s_ROMCol <= s_ROMCol;
                   end

                   `ROMMatrix_LDC: begin
                      s_State  <= `ROMMatrix_State_Ready;
                      s_ROMRow <= s_ROMRow;
                      s_ROMCol <= w_InstImm;
                   end

                   default: begin
                      s_State  <= `ROMMatrix_State_Error;
                      s_ROMRow <= 0;
                      s_ROMCol <= 0;
                   end
                 endcase // case (w_InstCode)
              end // if (inst_en)
              else begin
                 s_State  <= `ROMMatrix_State_Ready;
                 s_ROMRow <= s_ROMRow;
                 s_ROMCol <= s_ROMCol;
              end // else: !if(inst_en)
           end // case: `ROMMatrix_State_Ready

           `ROMMatrix_State_Error: begin
              s_State  <= `ROMMatrix_State_Error;
              s_ROMRow <= 0;
              s_ROMCol <= 0;
           end

           default: begin
              s_State  <= `ROMMatrix_State_Error;
              s_ROMRow <= 0;
              s_ROMCol <= 0;
           end
         endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

`ifdef SIM
   always @ * begin
      if (inst_en) begin
         case (w_InstCode)
           `ROMMatrix_NOP: begin
              $sformat(d_Input,"EN NOP");
           end

           `ROMMatrix_LDR: begin
              $sformat(d_Input,"EN (LDR %D)",w_InstImm);
           end

           `ROMMatrix_LDC: begin
              $sformat(d_Input,"EN (LDC %D)",w_InstImm);
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
        `ROMMatrix_State_Reset: begin
           $sformat(d_State,"X");
        end
        
        `ROMMatrix_State_Ready: begin
           $sformat(d_State,"R %D %D %D",s_ROMRow,s_ROMCol,rom_addr);
        end
        
        `ROMMatrix_State_Error: begin
           $sformat(d_State,"E");
        end
        
        default: begin
           $sformat(d_State,"?");
        end
      endcase // case (s_State)
   end // always @ *
`endif //  `ifdef SIM
endmodule // ROMMatrix
