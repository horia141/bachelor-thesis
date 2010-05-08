`timescale 1ns/10ps

module PushBtnSim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;
   reg 	       button;

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
      // Test for all instructions after reset
      #8 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
      #0 button = 0;

      #4 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      // A little delay from #4 to simulate the actual delay from a real
      // wire when receiving a new command from a controller.
      #5 inst_en = 0;

      #7 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Same type of delay as above.
      #5 inst_en = 0;

      // Same type of delay as above.
      #4 inst_en = 1;

      // An invalid instruction. This will send the PushBtn into the error state.
      #3 inst = {4'hB,8'hAE};
      #0 inst_en = 1;

      // This instruction won't execute, because the PushBtn is in the error state.
      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Only a reset will return the PushBtn into a normal, ready, state.
      #9 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #3 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate basic behaviour of a button.
      #3 button = 1;
      
      #202 button = 0;

      #3 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #8 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate a button being pressed for a long time and reads being made simultaneously. 
      // Only one read/clear cycle should return Pressed, the others should be Released.
      #3 button = 1;

      #1 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #8 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      #8 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #220 button = 0;

      #6 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // We simulate the opposite now : lots of button presses and a very late read.
      #3 button = 1;

      #300 button = 0;

      #3 button = 1;

      #230 button = 0;

      #4 button = 1;

      #220 button = 0;

      #2 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // A test for doing a RBS exactly when a button is triggered.
      #4 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      inst_en = 1;
      button = 1;

      #200 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      inst_en = 1;

      #1 button = 0;

      #3 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // A test for debouncer behaviour.
      #4 button = 1;

      #4 button = 0;

      #10 button = 1;

      #12 button = 0;

      #4 button = 1;

      #4 button = 0;

      #4 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;

      // A second test for debouncer behaviour.
      #4 button = 1;

      #4 button = 0;

      #10 button = 1;

      #12 button = 0;

      #4 button = 1;

      #4 button = 0;

      #4 button = 1;

      #240 button = 0;

      #4 inst = {`PushBtn_RBS,8'bxxxxxxxx};
      inst_en = 1;

      #4 inst = {`PushBtn_NOP,8'bxxxxxxxx};
      inst_en = 1;
   end      

   PushBtn #(.DebounceWait(50),
	     .DebounceSize(6))
   pushbtn (.clock(clock),
	    .reset(reset),

	    .inst(inst),
	    .inst_en(inst_en),

	    .button(button),

	    .button_status(button_status));
endmodule // PushBtnSim
