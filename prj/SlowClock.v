module SlowClock(clock,reset,outclock);
   parameter   SlowFactor = 32'h000000FF;

   input wire  clock;
   input wire  reset;
   
   output wire outclock;

   reg [31:0]  s_Counter;
   reg 	       s_OutClock;

   assign outclock = s_OutClock;

   always @ (posedge clock) begin
      if (reset) begin
	 s_Counter  <= 0;
	 s_OutClock <= 0;
      end
      else begin
	 if (s_Counter < (SlowFactor / 2 - 1)) begin
	    s_Counter  <= s_Counter + 1;
	    s_OutClock <= s_OutClock;
	 end
	 else begin
	    s_Counter  <= 0;
	    s_OutClock <= ~s_OutClock;
	 end
      end // else: !if(reset)
   end // always @ (posedge clock)
endmodule // SlowClock
