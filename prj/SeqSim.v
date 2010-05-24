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
      #0.1 inst_en = 0;

      // Test each instruction
      #8 inst = {`Seq_CI,1'bx,3'b010,4'h1,8'hAA};
      inst_en = 1;

      #4 inst = {`Seq_CR,1'bx,3'b011,4'h2,6'bxxxxxx,2'h2};
      inst_en = 1;
      ireg_2 = 8'hEE;

      #4 inst = {`Seq_JI,8'h1A,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Seq_JR,14'bxxxxxxxxxxxxxx,2'h1};
      inst_en = 1;
      ireg_1 = 8'h7B;

      #4 inst = {`Seq_JZ,8'h2A,6'bxxxxxx,2'h3};
      inst_en = 1;
      ireg_3 = 8'h00;

      #4 inst = {`Seq_JZ,8'h4A,6'bxxxxxx,2'h0};
      inst_en = 1;
      ireg_0 = 8'h11;

      #4 inst = {`Seq_JN,8'hB0,6'bxxxxxx,2'h1};
      inst_en = 1;
      ireg_1 = 8'h22;

      #4 inst = {`Seq_JN,8'h8A,6'bxxxxxx,2'h2};
      inst_en = 1;
      ireg_2 = 8'h00;

      #4 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction
      #4 inst = {`Seq_CI,1'bx,3'b011,4'h4,8'hBE};
      inst_en = 0;

      #4 inst = {`Seq_JR,14'bxxxxxxxxxxxxxx,2'h2};
      inst_en = 1;
      ireg_2 = 8'h07;

      // Test bad instruction
      #4 inst = {8'hF,16'hAEF0};
      inst_en = 1;

      #4 inst = {`Seq_JZ,8'hEF,6'bxxxxxx,2'h3};
      inst_en = 1;
      ireg_3 = 8'h00;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`Seq_JI,8'h48,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Seq_NO,16'bxxxxxxxxxxxxxxxx};
      inst_en = 1;
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
