`timescale 1ns/10ps

module SwcSim;
   reg         clock;
   reg         reset;

   reg [11:0]  inst;
   reg         inst_en;

   wire [23:0] counter;
   wire        ready;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #1000 $finish;
   end

   initial begin
      #0 clock = 1;
      forever #2 clock = ~clock;
   end

   initial begin
      #0 reset = 0;
      #1 reset = 1;
      #4 reset = 0;
   end

   initial begin
      #0.1 inst_en = 0;

      // Test each instruction.
      #8 inst = {`Swc_LD0,8'hA0};
      inst_en = 1;

      #4 inst = {`Swc_LD1,8'h0E};
      inst_en = 1;

      #4 inst = {`Swc_LD2,8'hBB};
      inst_en = 1;

      #4 inst = {`Swc_COU,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Swc_COD,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Swc_LD1,8'h00};
      inst_en = 1;

      #4 inst = {`Swc_LD2,8'h00};
      inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      inst_en = 1;

      #16 inst = {`Swc_CCS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      inst_en = 1;

      #16 inst = {`Swc_CCS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 inst = {`Swc_LD0,8'hEE};
      inst_en = 0;

      #4 inst = {`Swc_LD0,8'hAE};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'h10};
      inst_en = 1;

      #4 inst = {`Swc_LD1,8'hAE};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`Swc_LD1,8'hB0};
      inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test a complete count up.
      #4 inst = {`Swc_LD0,8'hF0};
      inst_en = 1;

      #4 inst = {`Swc_LD1,8'hFF};
      inst_en = 1;

      #4 inst = {`Swc_LD2,8'hFF};
      inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst_en = 0;

      // Test a complete count down.
      #72 inst = {`Swc_LD0,8'h0F};
      inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst_en = 0;

      // Test an incomplete count up.
      #72 inst = {`Swc_LD0,8'hF0};
      inst_en = 1;

      #4 inst = {`Swc_LD1,8'hFF};
      inst_en = 1;

      #4 inst = {`Swc_LD2,8'hFF};
      inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst_en = 0;

      #20 inst = {`Swc_LD0,8'hF0};
      inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test an incomplete count down.
      #4 inst = {`Swc_LD0,8'h0F};
      inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst_en = 0;

      #20 inst = {`Swc_LD0,8'hF0};
      inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end

   Swc
   swc (.clock(clock),
        .reset(reset),

        .inst(inst),
        .inst_en(inst_en),

        .counter(counter),
        .ready(ready));
endmodule // SwcSim
