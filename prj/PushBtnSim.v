`timescale 1ns/10ps

module PushBtnSim;
   reg         clock;
   reg         reset;

   reg [11:0]  inst;
   reg         inst_en;
   reg         button;

   wire        button_status;

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
      #8 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;
      button = 0;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #5 button = 1;

      #60 button = 0;

      #7 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test disabled instruction.
      #4 button = 1;

      #60 button = 0;
      
      #8 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 0;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test bad instruction.
      #4 inst = {8'hF,8'hAA};
      inst_en = 1;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 reset = 1;

      #8 reset = 0;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test multiple presses before one read.
      #4 button = 1;

      #60 button = 0;

      #8 button = 1;

      #60 button = 0;

      #8 button = 1;

      #60 button = 0;

      #8 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // Test multiple presses before one read. Some of the presses are caused by bounces.
      #4 button = 1;

      #8 button = 0;

      #4 button = 1;

      #16 button = 0;

      #4 button = 1;

      #60 button = 0;

      #8 button = 1;

      #16 button = 0;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_RDBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end      

   PushBtn #(.DebounceWait(10),
             .DebounceSize(4))
   pushbtn (.clock(clock),
            .reset(reset),

            .inst(inst),
            .inst_en(inst_en),
            .button(button),

            .button_status(button_status));
endmodule // PushBtnSim
