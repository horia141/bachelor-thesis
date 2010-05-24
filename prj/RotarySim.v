`timescale 1ns/10ps

module RotarySim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;
   reg [1:0]   rotary;

   wire        rotary_left_status;
   wire        rotary_right_status;

   initial begin
      #0 $dumpfile(`VCDFILE);
      #0 $dumpvars;

      #10000 $finish;
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
      #8 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;
      rotary = 2'b00;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #5 rotary = 2'b01;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #7 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;
      rotary = 2'b00;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #5 rotary = 2'b10;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #7 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 rotary = 2'b01;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #8 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 0;

      #4 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'hAA};
      inst_en = 1;

      #4 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test multiple left turns before one read.
      #4 rotary = 2'b01;

      #30 rotary = 2'b11;
      
      #30 rotary = 2'b00;

      #8 rotary = 2'b01;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #8 rotary = 2'b01;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #8 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test multiple right turns before one read.
      #4 rotary = 2'b10;

      #30 rotary = 2'b11;
      
      #30 rotary = 2'b00;

      #8 rotary = 2'b10;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #8 rotary = 2'b10;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #8 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test multiple left turns before one read. Some of the turns are caused by bounces.
      #4 rotary = 2'b01;

      #18 rotary = 2'b00;

      #2 rotary = 2'b01;

      #18 rotary = 2'b00;

      #2 rotary = 2'b01;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #8 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDLS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test multiple right turns before one read. Some of the turns are caused by bounces.
      #4 rotary = 2'b10;

      #18 rotary = 2'b00;

      #2 rotary = 2'b10;

      #18 rotary = 2'b00;

      #2 rotary = 2'b10;

      #30 rotary = 2'b11;

      #30 rotary = 2'b00;

      #8 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_RDRS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`Rotary_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end

   Rotary
   rotary (.clock(clock),
	   .reset(reset),

	   .inst(inst),
	   .inst_en(inst_en),
	   .rotary(rotary),

	   .rotary_left_status(rotary_left_status),
	   .rotary_right_status(rotary_right_status));
endmodule // RotarySim
