`timescale 1ns/10ps

module SeqSim;
   reg         clock;
   reg 	       reset;

   reg [19:0]  inst;
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
      #8 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_CI,4'bx001,4'h2,8'hAA};
      #0 inst_en = 1;

      #4 inst = {`Seq_CR,4'bx010,4'h3,8'bxxxxxx01};
      #0 ireg_1 = 8'hAA;
      #0 inst_en = 1;

      #4 inst = {`Seq_JI,8'hA0,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_JR,14'bxxxxxxxxxxxxxx,2'b10};
      #0 ireg_2 = 8'hBA;
      #0 inst_en = 1;

      #4 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_JZ,8'hE0,8'bxxxxxx11};
      #0 ireg_3 = 8'h00;
      #0 inst_en = 1;

      #4 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_JZ,8'hF0,8'bxxxxxx00};
      #0 ireg_0 = 8'h10;
      #0 inst_en = 1;

      #4 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_JR,14'bxxxxxxxxxxxxxx,2'b10};
      #0 ireg_2 = 8'hBA;
      #0 inst_en = 1;

      // A little delay from #4 to simulate the actual delay from a real
      // wire when receiving a new command from a controller.
      #5 inst_en = 0;

      #7 inst = {`Seq_JZ,8'hAA,8'bxxxxxx00};
      #0 inst_en = 1;

      // Same type of delay as above.
      #5 inst_en = 0;

      // Same type of delay as above.
      #4 inst_en = 1;

      // An invalid instruction. This will send the Seq into the error state.
      #3 inst = {4'hF,16'hFFEE};
      #0 inst_en = 1;

      // This instruction won't execute, because the Seq is in the error state.
      #4 inst = {`Seq_JI,8'hBB,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Only a reset will return the Seq into a normal, ready, state.
      #9 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #4 inst = {`Seq_JI,8'hBB,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
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
