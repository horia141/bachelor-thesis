module ClockManager(clock,reset,locked,clock0,clock90,clock180,clock270,clock2x,clock2x180);
   input wire  clock;
   input wire  reset;

   output wire locked;
   output wire clock0;
   output wire clock90;
   output wire clock180;
   output wire clock270;
   output wire clock2x;
   output wire clock2x180;

   wire        bufg_O;
   wire        dcmsp_CLK0;

   assign clock0 = dcmsp_CLK0;

   DCM_SP #(.CLKDV_DIVIDE(2.0), 
	    .CLKFX_DIVIDE(1),
	    .CLKFX_MULTIPLY(4),
	    .CLKIN_DIVIDE_BY_2("FALSE"),
	    .CLKIN_PERIOD(0.0),
	    .CLKOUT_PHASE_SHIFT("NONE"),
	    .CLK_FEEDBACK("1X"),
	    .DESKEW_ADJUST("SYSTEM_SYNCHRONOUS"),
	    .DLL_FREQUENCY_MODE("LOW"),
	    .DUTY_CYCLE_CORRECTION("TRUE"),
	    .PHASE_SHIFT(0),
	    .STARTUP_WAIT("FALSE"))
   dcmsp (.CLKIN(clock),
	  .RST(reset),
	  
	  .CLKFB(bufg_O),
	  .DSSEN(0),
	  .PSINCDEC(0),
	  .PSEN(0),
	  .PSCLK(0),

	  .LOCKED(locked),
	  .CLK0(dcmsp_CLK0),
	  .CLK90(clock90),
	  .CLK180(clock180),
	  .CLK270(clock270),
	  .CLK2X(clock2x),
	  .CLK2X180(clock2x180));

   BUFG
   bufg (.I(dcmsp_CLK0),
	 .O(bufg_O));
endmodule // ClockManager
