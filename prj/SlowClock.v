module SlowClock(clock,reset,outclock);
   parameter SlowFactor = 32'h000000FF;

   input wire  clock;
   input wire  reset;
   output reg  outclock;

   reg [31:0]  counter;

   always @ (posedge clock) begin
      if (reset) begin
	 counter  <= 0;
	 outclock <= 0;
      end
      else begin
	 if (counter < ((SlowFactor >> 1) - 1)) begin
	    counter  <= counter + 1;
	 end
	 else begin
	    counter <= 0;
	    outclock <= ~outclock;
	 end
      end // else: !if(reset)
   end // always @ (posedge clock)
endmodule // SlowClock
