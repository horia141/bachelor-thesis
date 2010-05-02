`timescale 1ns/10ps

module PushBtnBankSim;
   reg         clock;
   reg 	       reset;

   reg [11:0]  inst;
   reg 	       inst_en;
   reg 	       btn0;
   reg 	       btn1;
   reg 	       btn2;
   reg 	       btn3;

   wire [3:0]  btn_state;

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
      #8 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
      #0 btn0 = 0;
      #0 btn1 = 0;
      #0 btn2 = 0;
      #0 btn3 = 0;

      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RS0,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RS1,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RS2,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RS3,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RS2,8'bxxxxxxxx};
      #0 inst_en = 1;

      // A little delay from #4 to simulate the actual delay from a real
      // wire when receiving a new command from a controller.
      #5 inst_en = 0;

      #7 inst = {`PushBtnBank_RS1,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Same type of delay as above.
      #5 inst_en = 0;

      // Same type of delay as above.
      #4 inst_en = 1;

      // An invalid instruction. This will send the PushBtnBank into the error state.
      #3 inst = {4'hB,8'hAE};
      #0 inst_en = 1;

      // This instruction won't execute, because the PushBtnBank is in the error state.
      #4 inst = {`PushBtnBank_RS0,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Only a reset will return the PushBtnBank into a normal, ready, state.
      #9 reset = 1;
      #4 reset = 0;

      // This instruction will execute properly.
      #3 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate basic behaviour of a button.
      #3 btn1 = 0;

      #7 btn1 = 1;

      #3 btn1 = 0;

      #3 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate a button being pressed multiple times before a read is made.
      #3 btn0 = 0;

      #7 btn0 = 1;

      #3 btn0 = 0;

      #4 btn0 = 1;

      #9 btn0 = 0;

      #2 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate a button being pressed while a read is made.
      #0 btn2 = 0;
      #1 btn2 = 1;
      #3 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;
      #2 btn2 = 0;

      #2 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate all buttons being pressed. Just the first one is read.
      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
      
      #4 btn0 = 1;
      
      #1 btn1 = 1;
      
      #1 btn2 = 1;

      #1 btn3 = 1;

      #4 btn0 = 0;
      #0 btn1 = 0;
      #0 btn2 = 0;
      #0 btn3 = 0;

      #5 inst = {`PushBtnBank_RS0,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate all buttons being pressed. Just the second one is read.
      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
      
      #4 btn0 = 1;
      
      #1 btn1 = 1;
      
      #1 btn2 = 1;

      #1 btn3 = 1;

      #4 btn0 = 0;
      #0 btn1 = 0;
      #0 btn2 = 0;
      #0 btn3 = 0;

      #5 inst = {`PushBtnBank_RS1,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate all buttons being pressed. Just the third one is read.
      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
      
      #4 btn0 = 1;
      
      #1 btn1 = 1;
      
      #1 btn2 = 1;

      #1 btn3 = 1;

      #4 btn0 = 0;
      #0 btn1 = 0;
      #0 btn2 = 0;
      #0 btn3 = 0;

      #5 inst = {`PushBtnBank_RS2,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;

      // Simulate all buttons being pressed. Just the fourth one is read.
      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_RSA,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
      
      #4 btn0 = 1;
      
      #1 btn1 = 1;
      
      #1 btn2 = 1;

      #1 btn3 = 1;

      #4 btn0 = 0;
      #0 btn1 = 0;
      #0 btn2 = 0;
      #0 btn3 = 0;

      #5 inst = {`PushBtnBank_RS3,8'bxxxxxxxx};
      #0 inst_en = 1;

      #4 inst = {`PushBtnBank_NOP,8'bxxxxxxxx};
      #0 inst_en = 1;
   end

   PushBtnBank #()
   pushbtnbank (.clock(clock),
		.reset(reset),

		.inst(inst),
		.inst_en(inst_en),
		.btn0(btn0),
		.btn1(btn1),
		.btn2(btn2),
		.btn3(btn3),

		.btn_state(btn_state));
endmodule // PushBtnBankSim
