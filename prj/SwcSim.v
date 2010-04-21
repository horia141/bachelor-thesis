`timescale 1ns/10ps

module SwcSim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;

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
      // Test for all instructions after reset.
      #8 inst = {`Swc_LD0,8'hF0};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD1,8'hF1};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD2,8'hF2};
      #0 inst_en = 1;

      #4 inst = {`Swc_COU,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_COD,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      #0 inst_en = 1;

      #20 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      #0 inst_en = 1;

      #20 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      #0 inst_en = 1;

      #8 inst = {`Swc_CCS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      #0 inst_en = 1;

      // A little delay here, before we deassert the instruction
      // enable. We'll continue to count down, even with no
      // specifc enable.
      #5 inst_en = 0;

      #19 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // An invalid instruction.
      #4 inst = {4'hF,8'bxxxxxxxx};
      #0 inst_en = 1;

      // This instruction won't execute, because we're in the error state.
      #4 inst = {`Swc_LD0,8'hD0};
      #0 inst_en = 0;

      // We need to reset the system to be able to pull it out of
      // the error state.
      #6 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #2 inst = {`Swc_LD1,8'hFA};
      #0 inst_en = 1;

      #4 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Count up to zero and stop afterwards.
      #4 inst = {`Swc_LD0,8'hFA};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD1,8'hFF};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD2,8'hFF};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Deassert the instruction enable line to permit
      // the counting to continue normally (if we don't do this,
      // because the last instruction is CCU, we'll work fine
      // until we reach 0, then, instead of full-stopping, we'll
      // continue counting up).
      #5 inst_en = 0;

      // Count up to zero and be interrupted.
      #23 inst = {`Swc_LD0,8'hFA};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD1,8'hFF};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD2,8'hFF};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCU,8'bxxxxxxxx};
      #0 inst_en = 1;

      #5 inst_en = 0;

      // Interrupting the countdown.
      #15 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Count down to zero and stop afterwards.
      #4 inst = {`Swc_LD0,8'h06};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD1,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD2,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      #0 inst_en = 1;

      #5 inst_en = 0;

      // Count down to zero and be interrupted.
      #23 inst = {`Swc_LD0,8'h06};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD1,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_LD2,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Swc_CCD,8'bxxxxxxxx};
      #0 inst_en = 1;

      #5 inst_en = 0;

      // Interrupting the countdown.
      #15 inst = {`Swc_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
   end // initial begin

   Swc #()
   swc (.clock(clock),
	.reset(reset),

	.inst(inst),
	.inst_en(inst_en),

	.counter(counter),
	.ready(ready));
endmodule // SwcSim
