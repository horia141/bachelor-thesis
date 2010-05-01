`timescale 1ns/10ps

module SeqSim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;
   reg [7:0]   ireg_0;
   reg [7:0]   ireg_1;
   reg [7:0]   ireg_2;
   reg [7:0]   ireg_3;

   wire [7:0]  next;
   wire [11:0] oreg;
   wire [7:0]  oreg_wen;

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
      #8 inst = {`Seq_LDI,8'hFA};
      #0 inst_en = 1;

      #4 inst = {`Seq_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_LDR,8'bxx01xxxx};
      #0 inst_en = 1;
      #0 ireg_1 = 8'hAA;

      #4 inst = {`Seq_CMD,8'b0010x100};
      #0 inst_en = 1;

      #4 inst = {`Seq_DMP,8'bxxxxx010};
      #0 inst_en = 1;

      #4 inst = {`Seq_EQI,8'hAA};
      #0 inst_en = 1;

      #4 inst = {`Seq_LDI,8'hBB};
      #0 inst_en = 1;

      #4 inst = {`Seq_EQI,8'hAA};
      #0 inst_en = 1;

      #4 inst = {`Seq_LDI,8'hAA};
      #0 inst_en = 1;

      #4 inst = {`Seq_EQR,8'bxx10xxxx};
      #0 inst_en = 1;
      #0 ireg_2 = 8'hAA;

      #4 inst = {`Seq_LDI,8'hBB};
      #0 inst_en = 1;

      #4 inst = {`Seq_EQR,8'bxx00xxxx};
      #0 inst_en = 1;
      #0 ireg_0 = 8'hAA;

      #4 inst = {`Seq_JXI,8'h1A};
      #0 inst_en = 1;

      #4 inst = {`Seq_JXR,8'bxx10xxxx};
      #0 inst_en = 1;
      #0 ireg_2 = 8'h2A;

      #4 inst = {`Seq_LDI,8'h00};
      #0 inst_en = 1;

      #4 inst = {`Seq_JZI,8'h3A};
      #0 inst_en = 1;

      #4 inst = {`Seq_LDI,8'h01};
      #0 inst_en = 1;

      #4 inst = {`Seq_JZI,8'h4A};
      #0 inst_en = 1;

      #4 inst = {`Seq_LDI,8'h00};
      #0 inst_en = 1;

      #4 inst = {`Seq_JZR,8'bxx00xxxx};
      #0 inst_en = 1;
      #0 ireg_0 = 8'h5A;

      #4 inst = {`Seq_LDI,8'h01};
      #0 inst_en = 1;

      #4 inst = {`Seq_JZI,8'bxx01xxxx};
      #0 inst_en = 1;
      #0 ireg_1 = 8'h6A;

      #4 inst = {`Seq_LDI,8'h66};
      #0 inst_en = 1;

      // A little delay from #4 to simulate the actual delay from a real
      // wire when receiving a new command from a controller.
      #5 inst_en = 0;

      // An invalid instruction. This will send the Seq into the error state.
      #3 inst = {4'hF,8'h02};
      #0 inst_en = 1;

      // This instruction won't execute, becaue the Seq is in the error state.
      #4 inst = {`Seq_EQI,8'h10};
      #0 inst_en = 1;

      // Only a reset will return the Seq into a normal, ready, state.
      #9 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #3 inst = {`Seq_LDI,8'hFF};
      #0 inst_en = 1;

      #4 inst = {`Seq_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
   end

   Seq
   seq (.clock(clock),
	.reset(reset),

	.inst(inst),
	.inst_en(inst_en),
	.ireg_0(ireg_0),
	.ireg_1(ireg_1),
	.ireg_2(ireg_2),
	.ireg_3(ireg_3),

	.next(next),
	.oreg(oreg),
	.oreg_wen(oreg_wen));
endmodule // SeqSim
