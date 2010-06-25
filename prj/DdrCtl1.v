`define DdrCtl1_NOP 4'h0
`define DdrCtl1_LA0 4'h1
`define DdrCtl1_LA1 4'h2
`define DdrCtl1_LA2 4'h3
`define DdrCtl1_LA3 4'h4
`define DdrCtl1_LD0 4'h5
`define DdrCtl1_LD1 4'h6
`define DdrCtl1_LD2 4'h7
`define DdrCtl1_LD3 4'h8
`define DdrCtl1_RDP 4'h9
`define DdrCtl1_WRP 4'hA
`define DdrCtl1_LCK 4'hB
`define DdrCtl1_ULK 4'hC

`define DdrCtl1_DdrCommand_PowerUp          5'b00000
`define DdrCtl1_DdrCommand_Deselect         5'b11000
`define DdrCtl1_DdrCommand_NoOperation      5'b10111
`define DdrCtl1_DdrCommand_Activate         5'b10011
`define DdrCtl1_DdrCommand_Read             5'b10101
`define DdrCtl1_DdrCommand_Write            5'b10100
`define DdrCtl1_DdrCommand_BurtTerminate    5'b10110
`define DdrCtl1_DdrCommand_PreCharge        5'b10010
`define DdrCtl1_DdrCommand_AutoRefresh      5'b10001
`define DdrCtl1_DdrCommand_SelfRefresh      5'b00001
`define DdrCtl1_DdrCommand_LoadModeRegister 5'b10000

`define DdrCtl1_DdrMode_BurstLength_2                3'b001
`define DdrCtl1_DdrMode_BurstLength_4                3'b010
`define DdrCtl1_DdrMode_BurstLength_8                3'b011
`define DdrCtl1_DdrMode_BurstType_Sequential         1'b0
`define DdrCtl1_DdrMode_BurstType_Interleaved        1'b1
`define DdrCtl1_DdrMode_CASLatency_2                 3'b010
`define DdrCtl1_DdrMode_CASLatency_3                 3'b011
`define DdrCtl1_DdrMode_CASLatency_2_5               3'b110
`define DdrCtl1_DdrMode_OperatingMode_Normal         6'b000000
`define DdrCtl1_DdrMode_OperatingMode_NormalResetDLL 6'b000010

`define DdrCtl1_DdrModeExtend_DLL_Enable             1'b0
`define DdrCtl1_DdrModeExtend_DLL_Disable            1'b1
`define DdrCtl1_DdrModeExtend_DriveStrength_Normal   1'b0
`define DdrCtl1_DdrModeExtend_DriveStrength_Reduced  1'b0
`define DdrCtl1_DdrModeExtend_OperatingMode_Reserved 11'b00000000000

`define DdrCtl1_SelectModeRegister_Normal            2'b00
`define DdrCtl1_SelectModeRegister_Extended          2'b01

`define DdrCtl1_IntfState_Reset
`define DdrCtl1_IntfState_WaitIniti
`define DdrCtl1_IntfState_Ready
`define DdrCtl1_IntfState_WaitCommand
`define DdrCtl1_IntfState_Error

`define DdrCtl1_CoreState_Reset
`define DdrCtl1_CoreState_WaitInit
`define DdrCtl1_CoreStaet_Ready
`define DdrCtl1_CoreState_Reading_Activate
`define DdrCtl1_CoreState_Reading_Wait0
`define DdrCtl1_CoreState_Reading_Read
`define DdrCtl1_CoreState_Reading_Wait1
`define DdrCtl1_CoreState_Reading_Wait2
`define DdrCtl1_CoreState_Reading_Wait3
`define DdrCtl1_CoreState_Reading_PreChargeAll
`define DdrCtl1_CoreState_Writing_Activate
`define DdrCtl1_CoreState_Writing_Wait0
`define DdrCtl1_CoreState_Writing_Write
`define DdrCtl1_CoreState_Writing_Wait1
`define DdrCtl1_CoreState_Writing_Wait2
`define DdrCtl1_CoreState_Writing_PreChargeAll
`define DdrCtl1_CoreState_Refreshing_AutoRefresh
`define DdrCtl1_CoreState_Refreshing_Wait0
`define DdrCtl1_CoreState_Refreshing_Wait1
`define DdrCtl1_CoreState_Refreshing_Wait2

`define DdrCtl1_InitState_Reset
`define DdrCtl1_InitState_PowerUp
`define DdrCtl1_InitState_Wait200us
`define DdrCtl1_InitState_BringCKEHigh
`define DdrCtl1_InitState_PreChargeAll0
`define DdrCtl1_InitState_EnableDLL
`define DdrCtl1_InitState_ProgramMRResetDLL
`define DdrCtl1_InitState_WaitMRD200
`define DdrCtl1_InitState_PreChargeAll1
`define DdrCtl1_InitState_Refresh0_AutoRefresh
`define DdrCtl1_InitState_Refresh0_Wait0
`define DdrCtl1_InitState_Refresh0_Wait1
`define DdrCtl1_InitState_Refresh0_Wait2
`define DdrCtl1_InitState_Refresh1_AutoRefresh
`define DdrCtl1_InitState_Refresh1_Wait0
`define DdrCtl1_InitState_Refresh1_Wait1
`define DdrCtl1_InitState_Refresh1_Wait2
`define DdrCtl1_InitState_ClearDLL
`define DdrCtl1_InitState_Initialized

`define DdrCtl1_State_Reset                          7'h00
`define DdrCtl1_State_Initializing_PowerUp           7'h01
`define DdrCtl1_State_Initializing_Wait200us         7'h02
`define DdrCtl1_State_Initializing_BringCKEHighDoNop 7'h03
`define DdrCtl1_State_Initializing_PreChargeAll0     7'h04
`define DdrCtl1_State_Initializing_EnableDLL         7'h05
`define DdrCtl1_State_Initializing_ProgramMRResetDLL 7'h06
`define DdrCtl1_State_Initializing_WaitMRD200DoNop   7'h07
`define DdrCtl1_State_Initializing_PreChargeAll1     7'h08
`define DdrCtl1_State_Initializing_AutoRefresh00     7'h09
`define DdrCtl1_State_Initializing_AutoRefresh01     7'h0A
`define DdrCtl1_State_Initializing_AutoRefresh02     7'h0B
`define DdrCtl1_State_Initializing_AutoRefresh03     7'h0C
`define DdrCtl1_State_Initializing_AutoRefresh10     7'h0D
`define DdrCtl1_State_Initializing_AutoRefresh11     7'h0E
`define DdrCtl1_State_Initializing_AutoRefresh12     7'h0F
`define DdrCtl1_State_Initializing_AutoRefresh13     7'h10
`define DdrCtl1_State_Initializing_ClearDLL          7'h11
`define DdrCtl1_State_Ready                          7'h12
`define DdrCtl1_State_Reading_Activate               7'h13
`define DdrCtl1_State_Reading_Wait0                  7'h14
`define DdrCtl1_State_Reading_Read                   7'h15
`define DdrCtl1_State_Reading_Wait1                  7'h16
`define DdrCtl1_State_Reading_Wait2                  7'h17
`define DdrCtl1_State_Reading_Wait3                  7'h18
`define DdrCtl1_State_Reading_Wait4                  7'h19
`define DdrCtl1_State_Writing_Activate               7'h1A
`define DdrCtl1_State_Writing_Wait0                  7'h1B
`define DdrCtl1_State_Writing_Write                  7'h1C
`define DdrCtl1_State_Writing_Wait1                  7'h1D
`define DdrCtl1_State_Writing_Wait2                  7'h1E
`define DdrCtl1_State_Writing_Wait3                  7'h1F
`define DdrCtl1_State_Refreshing_PreChargeAll        7'h20
`define DdrCtl1_State_Refreshing_AutoRefresh         7'h21
`define DdrCtl1_State_Refreshing_Wait0               7'h22
`define DdrCtl1_State_Refreshing_Wait1               7'h23
`define DdrCtl1_State_Refreshing_Wait2               7'h24
`define DdrCtl1_State_Error                          7'h25

module DdrCtl1(clock0,clock90,reset,inst,inst_en,page,ready,locked,ddr_cke,ddr_csn,ddr_rasn,ddr_casn,ddr_wen,ddr_ba,ddr_addr,ddr_dm,ddr_dq,ddr_dqs);
   input wire         clock0;
   input wire 	      clock90;
   input wire         reset;

   input wire [11:0]  inst;
   input wire 	      inst_en;
   
   output wire [31:0] page;
   output wire 	      ready;
   output wire 	      locked;

   output wire        ddr_cke;
   output wire 	      ddr_csn;
   output wire 	      ddr_rasn;
   output wire 	      ddr_casn;
   output wire 	      ddr_wen;
   output wire [1:0]  ddr_ba;
   output wire [12:0] ddr_addr;
   output wire [1:0]  ddr_dm;
   inout wire [15:0]  ddr_dq;
   inout wire [1:0]   ddr_dqs;

   reg [6:0] 	      s_State;
   reg [31:0] 	      s_Address;
   reg [31:0] 	      s_Page;
   reg 		      s_Locked;
   reg [4:0] 	      s_Command;
   reg [1:0] 	      s_Bank;
   reg [12:0] 	      s_Addr;
   reg 		      s_Count200us;
   reg 		      s_Count200;
   reg 		      s_Refreshed;
   
   wire 	      i_Ready;

   reg [15:0] 	      s_HalfPage;

   reg [13:0] 	      s_InitializeCnt200us;
   reg [7:0] 	      s_InitializeCnt200;

   reg [8:0] 	      s_RefreshCnt;
   reg 		      s_ShouldRefresh;

   reg 		      s_ShouldLock;

   wire [3:0] 	      w_InstCode;
   wire [7:0] 	      w_InstImm;

   reg [256*8-1:0]    d_Input;
   reg [256*8-1:0]    d_State;

   assign page = s_Page;
   assign ready = i_Ready;
   assign locked = s_Locked & i_Ready;

   assign ddr_cke = s_Command[4];
   assign ddr_csn = s_Command[3];
   assign ddr_rasn = s_Command[2];
   assign ddr_casn = s_Command[1];
   assign ddr_wen = s_Command[0];
   assign ddr_ba = s_Bank;
   assign ddr_addr = s_Addr;
   assign ddr_dm = 2'b00;
   assign ddr_dq = (s_State == `DdrCtl1_State_Writing_Wait2) ? (clock0 == 1 ? s_Page[31:16] : s_Page[15:0]) : 16'bzzzzzzzzzzzzzzzz;
   assign ddr_dqs = (s_State == `DdrCtl1_State_Writing_Wait2 || s_State == `DdrCtl1_State_Writing_Wait3) ? {clock90,clock90} : 2'bzz;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   assign i_Ready = s_State == `DdrCtl1_State_Ready;

   // synthesis attribute fsm_encoding of s_State is speed1;

   always @ (negedge clock0) begin
      if (s_State == `DdrCtl1_State_Reading_Wait3 && ddr_dqs[0] == 1 && ddr_dqs[1] == 1) begin
	 s_HalfPage <= ddr_dq;
      end
      else begin
	 s_HalfPage <= s_HalfPage;
      end
   end

   always @ (posedge clock0) begin
      if (reset) begin
	 s_InitializeCnt200us <= 0;
      end
      else begin
	 if (s_Count200us == 1) begin
	    s_InitializeCnt200us <= s_InitializeCnt200us + 1;
	 end
      end
   end

   always @ (posedge clock0) begin
      if (reset) begin
	 s_InitializeCnt200 <= 0;
      end
      else begin
	 if (s_Count200 == 1) begin
	    s_InitializeCnt200 <= s_InitializeCnt200 + 1;
	 end
      end
   end

   always @ (posedge clock0) begin
      if (reset) begin
	 s_RefreshCnt    <= 0;
	 s_ShouldRefresh <= 0;
      end
      else begin
	 if (s_ShouldRefresh == 1) begin
	    if (s_Refreshed == 1) begin
	       s_RefreshCnt    <= 0;
	       s_ShouldRefresh <= 0;
	    end
	    else begin
	       s_RefreshCnt    <= 0;
	       s_ShouldRefresh <= 1;
	    end
	 end
	 else begin
	    if (s_RefreshCnt == 200) begin
	       s_RefreshCnt    <= 0;
	       s_ShouldRefresh <= 1;
	    end
	    else begin
	       s_RefreshCnt    <= s_RefreshCnt + 1;
	       s_ShouldRefresh <= 0;
	    end
	 end // else: !if(s_ShouldRefresh == 1)
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ (posedge clock0) begin
      if (reset) begin
	 s_ShouldLock <= 0;
      end
      else begin
	 if (w_InstCode == `DdrCtl1_LCK) begin
	    s_ShouldLock <= 1;
	 end
	 else begin
	    if (s_Locked == 1) begin
	       s_ShouldLock <= 0;
	    end
	    else begin
	       s_ShouldLock <= s_ShouldLock;
	    end
	 end
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ (posedge clock0) begin
      if (reset) begin
	 s_State   <= `DdrCtl1_State_Reset;
	 s_Address <= 0;
	 s_Page    <= 0;
	 s_Locked  <= 0;
      end
      else begin
	 case (s_State)
	   `DdrCtl1_State_Reset: begin
	      s_State    <= `DdrCtl1_State_Initializing_PowerUp;
	      s_Address  <= 0;
	      s_Page     <= 0;
	      s_Locked   <= 0;
	   end

	   `DdrCtl1_State_Initializing_PowerUp: begin
	      s_State   <= `DdrCtl1_State_Initializing_Wait200us;
	      s_Address <= 0;
	      s_Page    <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_Wait200us: begin
	      if (s_InitializeCnt200us == 10000) begin
		 s_State   <= `DdrCtl1_State_Initializing_BringCKEHighDoNop;
		 s_Address <= 0;
		 s_Page    <= 0;
		 s_Locked  <= 0;
	      end
	      else begin
		 s_State   <= `DdrCtl1_State_Initializing_Wait200us;
		 s_Address <= 0;
		 s_Page    <= 0;
		 s_Locked  <= 0;
	      end
	   end

	   `DdrCtl1_State_Initializing_BringCKEHighDoNop: begin
	      s_State   <= `DdrCtl1_State_Initializing_PreChargeAll0;
	      s_Address <= 0;
	      s_Page    <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_PreChargeAll0: begin
	      s_State   <= `DdrCtl1_State_Initializing_EnableDLL;
	      s_Address <= 0;
	      s_Page    <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_EnableDLL: begin
	      s_State <= `DdrCtl1_State_Initializing_ProgramMRResetDLL;
	      s_Address <= 0;
	      s_Page    <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_ProgramMRResetDLL: begin
	      s_State <= `DdrCtl1_State_Initializing_WaitMRD200DoNop;
	      s_Address <= 0;
	      s_Page    <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_WaitMRD200DoNop: begin
	      if (s_InitializeCnt200 == 200) begin
		 s_State <= `DdrCtl1_State_Initializing_PreChargeAll1;
		 s_Address <= 0;
		 s_Page    <= 0;
		 s_Locked  <= 0;
	      end
	      else begin
		 s_State <= `DdrCtl1_State_Initializing_WaitMRD200DoNop;
		 s_Address <= 0;
		 s_Page    <= 0;
		 s_Locked  <= 0;
	      end
	   end

	   `DdrCtl1_State_Initializing_PreChargeAll1: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh00;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh00: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh01;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh01: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh02;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh02: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh03;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh03: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh10;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh10: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh11;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh11: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh12;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh12: begin
	      s_State <= `DdrCtl1_State_Initializing_AutoRefresh13;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh13: begin
	      s_State <= `DdrCtl1_State_Initializing_ClearDLL;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Initializing_ClearDLL: begin
	      s_State <= `DdrCtl1_State_Ready;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= 0;
	   end

	   `DdrCtl1_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `DdrCtl1_NOP: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= s_Address;
		      s_Page          <= s_Page;
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LA0: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= {s_Address[31:8],w_InstImm};
		      s_Page          <= s_Page;
		      s_Locked <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LA1: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= {s_Address[31:16],w_InstImm,s_Address[7:0]};
		      s_Page          <= s_Page;
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LA2: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= {s_Address[31:24],w_InstImm,s_Address[15:0]};
		      s_Page          <= s_Page;
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LA3: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= {w_InstImm,s_Address[23:0]};
		      s_Page          <= s_Page;
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LD0: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= s_Address;
		      s_Page          <= {s_Page[31:8],w_InstImm};
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LD1: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= s_Address;
		      s_Page          <= {s_Page[31:16],w_InstImm,s_Page[7:0]};
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LD2: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= s_Address;
		      s_Page          <= {s_Page[31:24],w_InstImm,s_Page[15:0]};
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LD3: begin
		      s_State <= `DdrCtl1_State_Ready;
		      s_Address       <= s_Address;
		      s_Page          <= {w_InstImm,s_Page[23:0]};
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_RDP: begin
		      s_State <= `DdrCtl1_State_Reading_Activate;
		      s_Address       <= s_Address;
		      s_Page          <= s_Page;
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_WRP: begin
		      s_State <= `DdrCtl1_State_Writing_Activate;
		      s_Address       <= s_Address;
		      s_Page          <= s_Page;
		      s_Locked  <= s_ShouldLock | s_Locked;
		   end

		   `DdrCtl1_LCK: begin
		      s_State   <= `DdrCtl1_State_Ready;
		      s_Address <= s_Address;
		      s_Page    <= s_Page;
		      s_Locked  <= 1;
		   end

		   `DdrCtl1_ULK: begin
		      s_State   <= `DdrCtl1_State_Ready;
		      s_Address <= s_Address;
		      s_Page    <= s_Page;
		      s_Locked  <= 0;
		   end		   

		   default: begin
		      s_State <= `DdrCtl1_State_Error;
		      s_Address       <= 0;
		      s_Page          <= 0;
		      s_Locked  <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 if (s_ShouldRefresh == 1 && s_Locked == 0) begin
		    s_State   <= `DdrCtl1_State_Refreshing_PreChargeAll;
		    s_Address <= s_Address;
		    s_Page    <= s_Page;
		    s_Locked  <= s_ShouldLock | s_Locked;
		 end
		 else begin
		    s_State   <= `DdrCtl1_State_Ready;
		    s_Address <= s_Address;
		    s_Page    <= s_Page;
		    s_Locked  <= s_ShouldLock | s_Locked;
		 end
	      end // else: !if(inst_en)
	   end // case: `DdrCtl1_State_Ready

	   `DdrCtl1_State_Reading_Activate: begin
	      s_State <= `DdrCtl1_State_Reading_Wait0;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Reading_Wait0: begin
	      s_State <= `DdrCtl1_State_Reading_Read;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Reading_Read: begin
	      s_State <= `DdrCtl1_State_Reading_Wait1;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Reading_Wait1: begin
	      s_State <= `DdrCtl1_State_Reading_Wait2;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Reading_Wait2: begin
	      s_State <= `DdrCtl1_State_Reading_Wait3;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Reading_Wait3: begin
	      s_State <= `DdrCtl1_State_Reading_Wait4;
	      s_Address       <= s_Address;
	      s_Page          <= {s_HalfPage,ddr_dq};
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Reading_Wait4: begin
	      if (s_ShouldRefresh == 1) begin
		 s_State <= `DdrCtl1_State_Refreshing_PreChargeAll;
		 s_Address       <= s_Address;
		 s_Page          <= s_Page;
		 s_Locked  <= s_ShouldLock | s_Locked;
	      end
	      else begin
		 s_State <= `DdrCtl1_State_Ready;
		 s_Address       <= s_Address;
		 s_Page          <= s_Page;
		 s_Locked  <= s_ShouldLock | s_Locked;
	      end
	   end

	   `DdrCtl1_State_Writing_Activate: begin
	      s_State <= `DdrCtl1_State_Writing_Wait0;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Writing_Wait0: begin
	      s_State <= `DdrCtl1_State_Writing_Write;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Writing_Write: begin
	      s_State <= `DdrCtl1_State_Writing_Wait1;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Writing_Wait1: begin
	      s_State <= `DdrCtl1_State_Writing_Wait2;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Writing_Wait2: begin
	      s_State <= `DdrCtl1_State_Writing_Wait3;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Writing_Wait3: begin
	      if (s_ShouldRefresh == 1) begin
		 s_State <= `DdrCtl1_State_Refreshing_PreChargeAll;
		 s_Address       <= s_Address;
		 s_Page          <= s_Page;
		 s_Locked  <= s_ShouldLock | s_Locked;
	      end
	      else begin
		 s_State <= `DdrCtl1_State_Ready;
		 s_Address       <= s_Address;
		 s_Page          <= s_Page;
		 s_Locked  <= s_ShouldLock | s_Locked;
	      end
	   end

	   `DdrCtl1_State_Refreshing_PreChargeAll: begin
	      s_State <= `DdrCtl1_State_Refreshing_AutoRefresh;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Refreshing_AutoRefresh: begin
	      s_State <= `DdrCtl1_State_Refreshing_Wait0;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Refreshing_Wait0: begin
	      s_State <= `DdrCtl1_State_Refreshing_Wait1;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Refreshing_Wait1: begin
	      s_State <= `DdrCtl1_State_Refreshing_Wait2;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Refreshing_Wait2: begin
	      s_State <= `DdrCtl1_State_Ready;
	      s_Address       <= s_Address;
	      s_Page          <= s_Page;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   `DdrCtl1_State_Error: begin
	      s_State <= `DdrCtl1_State_Error;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end

	   default: begin
	      s_State <= `DdrCtl1_State_Error;
	      s_Address       <= 0;
	      s_Page          <= 0;
	      s_Locked  <= s_ShouldLock | s_Locked;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ (posedge clock0) begin
      if (reset) begin
	 s_Command       = `DdrCtl1_DdrCommand_PowerUp;
	 s_Bank          = 0;
	 s_Addr          = 0;
	 s_Count200us    = 0;
	 s_Count200      = 0;
	 s_Refreshed     = 0;
      end
      else begin
	 case (s_State)
	   `DdrCtl1_State_Reset: begin
	      s_Command       = `DdrCtl1_DdrCommand_PowerUp;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_PowerUp: begin
	      s_Command       = `DdrCtl1_DdrCommand_PowerUp;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_Wait200us: begin
	      if (s_InitializeCnt200us == 10000) begin
		 s_Command       = `DdrCtl1_DdrCommand_PowerUp;
		 s_Bank          = 0;
		 s_Addr          = 0;
		 s_Count200us    = 0;
		 s_Count200      = 0;
		 s_Refreshed     = 0;
	      end
	      else begin
		 s_Command       = `DdrCtl1_DdrCommand_PowerUp;
		 s_Bank          = 0;
		 s_Addr          = 0;
		 s_Count200us    = 1;
		 s_Count200      = 0;
		 s_Refreshed     = 0;
	      end
	   end // case: `DdrCtl1_State_Initializing_Wait200us

	   `DdrCtl1_State_Initializing_BringCKEHighDoNop: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_PreChargeAll0: begin
	      s_Command       = `DdrCtl1_DdrCommand_PreCharge;
	      s_Bank          = 0;
	      s_Addr          = 13'b0010000000000;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_EnableDLL: begin
	      s_Command       = `DdrCtl1_DdrCommand_LoadModeRegister;
	      s_Bank          = `DdrCtl1_SelectModeRegister_Extended;
	      s_Addr          = {`DdrCtl1_DdrModeExtend_OperatingMode_Reserved,
                                  `DdrCtl1_DdrModeExtend_DriveStrength_Normal,
                                  `DdrCtl1_DdrModeExtend_DLL_Enable};
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_ProgramMRResetDLL: begin
	      s_Command       = `DdrCtl1_DdrCommand_LoadModeRegister;
	      s_Bank          = `DdrCtl1_SelectModeRegister_Normal;
	      s_Addr          = {`DdrCtl1_DdrMode_OperatingMode_NormalResetDLL,
                                  `DdrCtl1_DdrMode_CASLatency_2,
                                  `DdrCtl1_DdrMode_BurstType_Sequential,
                                  `DdrCtl1_DdrMode_BurstLength_2};
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_WaitMRD200DoNop: begin
	      if (s_InitializeCnt200 == 200) begin
		 s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          = 0;
		 s_Addr          = 0;
		 s_Count200us    = 0;
		 s_Count200      = 0;
		 s_Refreshed     = 0;
	      end
	      else begin
		 s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          = 0;
		 s_Addr          = 0;
		 s_Count200us    = 0;
		 s_Count200      = 1;
		 s_Refreshed     = 0;
	      end
	   end


	   `DdrCtl1_State_Initializing_PreChargeAll1: begin
	      s_Command       = `DdrCtl1_DdrCommand_PreCharge;
	      s_Bank          = 0;
	      s_Addr          = 13'b0010000000000;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh00: begin
	      s_Command       = `DdrCtl1_DdrCommand_AutoRefresh;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh01: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh02: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh03: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh10: begin
	      s_Command       = `DdrCtl1_DdrCommand_AutoRefresh;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh11: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh12: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh13: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Initializing_ClearDLL: begin
	      s_Command       = `DdrCtl1_DdrCommand_LoadModeRegister;
	      s_Bank          = `DdrCtl1_SelectModeRegister_Normal;
	      s_Addr          = {`DdrCtl1_DdrMode_OperatingMode_Normal,
                                  `DdrCtl1_DdrMode_CASLatency_2,
                                  `DdrCtl1_DdrMode_BurstType_Sequential,
                                  `DdrCtl1_DdrMode_BurstLength_2};
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `DdrCtl1_NOP: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LA0: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LA1: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LA2: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LA3: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LD0: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LD1: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LD2: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_LD3: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_RDP: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   `DdrCtl1_WRP: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end

		   default: begin
		      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          = 0;
		      s_Addr          = 0;
		      s_Count200us    = 0;
		      s_Count200      = 0;
		      s_Refreshed     = 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 if (s_ShouldRefresh == 1) begin
		    s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		    s_Bank          = 0;
		    s_Addr          = 0;
		    s_Count200us    = 0;
		    s_Count200      = 0;
		    s_Refreshed     = 0;
		 end
		 else begin
		    s_Command       = `DdrCtl1_DdrCommand_NoOperation;
		    s_Bank          = 0;
		    s_Addr          = 0;
		    s_Count200us    = 0;
		    s_Count200      = 0;
		    s_Refreshed     = 0;
		 end
	      end // else: !if(inst_en)
	   end // case: `DdrCtl1_State_Ready

	   `DdrCtl1_State_Reading_Activate: begin
	      s_Command       = `DdrCtl1_DdrCommand_Activate;
	      s_Bank          = s_Address[24:23];
	      s_Addr          = s_Address[22:10];
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Reading_Wait0: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Reading_Read: begin
	      s_Command       = `DdrCtl1_DdrCommand_Read;
	      s_Bank          = s_Address[24:23];
	      s_Addr          = {3'b000,s_Address[9:0]};
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Reading_Wait1: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Reading_Wait2: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Reading_Wait3: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Reading_Wait4: begin
	      if (s_ShouldRefresh == 1) begin
		 s_Command       = `DdrCtl1_DdrCommand_PreCharge;
		 s_Bank          = 0;
		 s_Addr          = 13'b0010000000000;
		 s_Count200us    = 0;
		 s_Count200      = 0;
		 s_Refreshed     = 0;
	      end
	      else begin
		 s_Command       = `DdrCtl1_DdrCommand_PreCharge;
		 s_Bank          = 0;
		 s_Addr          = 13'b0010000000000;
		 s_Count200us    = 0;
		 s_Count200      = 0;
		 s_Refreshed     = 0;
	      end
	   end

	   `DdrCtl1_State_Writing_Activate: begin
	      s_Command       = `DdrCtl1_DdrCommand_Activate;
	      s_Bank          = s_Address[24:23];
	      s_Addr          = s_Address[22:10];
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Writing_Wait0: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Writing_Write: begin
	      s_Command       = `DdrCtl1_DdrCommand_Write;
	      s_Bank          = s_Address[24:23];
	      s_Addr          = {3'b000,s_Address[9:0]};
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Writing_Wait1: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Writing_Wait2: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   `DdrCtl1_State_Writing_Wait3: begin
	      if (s_ShouldRefresh == 1) begin
		 s_Command       = `DdrCtl1_DdrCommand_PreCharge;
		 s_Bank          = 0;
		 s_Addr          = 13'b0010000000000;
		 s_Count200us    = 0;
		 s_Count200      = 0;
		 s_Refreshed     = 0;
	      end
	      else begin
		 s_Command       = `DdrCtl1_DdrCommand_PreCharge;
		 s_Bank          = 0;
		 s_Addr          = 13'b0010000000000;
		 s_Count200us    = 0;
		 s_Count200      = 0;
		 s_Refreshed     = 0;
	      end
	   end

	   `DdrCtl1_State_Refreshing_PreChargeAll: begin
	      s_Command       = `DdrCtl1_DdrCommand_PreCharge;
	      s_Bank          = 0;
	      s_Addr          = 13'b0010000000000;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 1;
	   end

	   `DdrCtl1_State_Refreshing_AutoRefresh: begin
	      s_Command       = `DdrCtl1_DdrCommand_AutoRefresh;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 1;
	   end

	   `DdrCtl1_State_Refreshing_Wait0: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 1;
	   end

	   `DdrCtl1_State_Refreshing_Wait1: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 1;
	   end

	   `DdrCtl1_State_Refreshing_Wait2: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 1;
	   end

	   `DdrCtl1_State_Error: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end

	   default: begin
	      s_Command       = `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          = 0;
	      s_Addr          = 0;
	      s_Count200us    = 0;
	      s_Count200      = 0;
	      s_Refreshed     = 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ * begin
      if (inst_en) begin
	 case (w_InstCode)
	   `DdrCtl1_NOP: begin
	      $sformat(d_Input,"EN NOP");
	   end

	   `DdrCtl1_LA0: begin
	      $sformat(d_Input,"EN (LA0 %2X)",w_InstImm);
	   end

	   `DdrCtl1_LA1: begin
	      $sformat(d_Input,"EN (LA1 %2X)",w_InstImm);
	   end

	   `DdrCtl1_LA2: begin
	      $sformat(d_Input,"EN (LA2 %2X)",w_InstImm);
	   end

	   `DdrCtl1_LA3: begin
	      $sformat(d_Input,"EN (LA3 %2X)",w_InstImm);
	   end

	   `DdrCtl1_LD0: begin
	      $sformat(d_Input,"EN (LD0 %2X)",w_InstImm);
	   end

	   `DdrCtl1_LD1: begin
	      $sformat(d_Input,"EN (LD1 %2X)",w_InstImm);
	   end

	   `DdrCtl1_LD2: begin
	      $sformat(d_Input,"EN (LD2 %2X)",w_InstImm);
	   end

	   `DdrCtl1_LD3: begin
	      $sformat(d_Input,"EN (LD3 %2X)",w_InstImm);
	   end

	   `DdrCtl1_RDP: begin
	      $sformat(d_Input,"EN RDP");
	   end

	   `DdrCtl1_WRP: begin
	      $sformat(d_Input,"EN WRP");
	   end

	   `DdrCtl1_LCK: begin
	      $sformat(d_Input,"EN LCK");
	   end

	   `DdrCtl1_ULK: begin
	      $sformat(d_Input,"EN ULK");
	   end

	   default: begin
	      $sformat(d_Input,"EN (? %2X)",w_InstImm);
	   end
	 endcase // case (w_InstCode)
      end // if (inst_en)
      else begin
	 $sformat(d_Input,"NN");
      end // else: !if(inst_en)
   end // always @ *

   always @ * begin
      case (s_State)
	`DdrCtl1_State_Reset: begin
	   $sformat(d_State,"X");
	end

	`DdrCtl1_State_Initializing_PowerUp: begin
	   $sformat(d_State,"I PowerUp (PowerUp %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_Wait200us: begin
	   $sformat(d_State,"I Wait200us (PowerUp %5B) %D",s_Command,s_InitializeCnt200us);
	end

	`DdrCtl1_State_Initializing_BringCKEHighDoNop: begin
	   $sformat(d_State,"I BringCKEHigh (NoOperation %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_PreChargeAll0: begin
	   $sformat(d_State,"I PreChargeAll0 (PreCharge %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_EnableDLL: begin
	   $sformat(d_State,"I EnableDLL (LoadModeRegister %5B %2B %13B)",s_Command,s_Bank,s_Addr);
	end

	`DdrCtl1_State_Initializing_ProgramMRResetDLL: begin
	   $sformat(d_State,"I ProgramMRResetDLL (LoadModeRegister %5B %2B %13B)",s_Command,s_Bank,s_Addr);
	end

	`DdrCtl1_State_Initializing_WaitMRD200DoNop: begin
	   $sformat(d_State,"I WaitMRD200DoNop (NoOperation %5B) %D",s_Command,s_InitializeCnt200);
	end

	`DdrCtl1_State_Initializing_PreChargeAll1: begin
	   $sformat(d_State,"I PreChargeAll1 (PreCharge %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh00: begin
	   $sformat(d_State,"I AutoRefresh00 (AutoRefresh %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh01: begin
	   $sformat(d_State,"I AutoRefresh01 (NoOperation %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh02: begin
	   $sformat(d_State,"I AutoRefresh02 (NoOperation %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh03: begin
	   $sformat(d_State,"I AutoRefresh03 (NoOperation %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh10: begin
	   $sformat(d_State,"I AutoRefresh10 (AutoRefresh %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh11: begin
	   $sformat(d_State,"I AutoRefresh11 (NoOperation %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh12: begin
	   $sformat(d_State,"I AutoRefresh12 (NoOperation %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_AutoRefresh13: begin
	   $sformat(d_State,"I AutoRefresh13 (NoOperation %5B)",s_Command);
	end

	`DdrCtl1_State_Initializing_ClearDLL: begin
	   $sformat(d_State,"I ClearAll (LoadModeRegister %5B %2B %13B)",s_Command,s_Bank,s_Addr);
	end

	`DdrCtl1_State_Ready: begin
	   $sformat(d_State,"R %8X %8X %3D %1B",s_Address,s_Page,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Reading_Activate: begin
	   $sformat(d_State,"r Activate %8X %8X (Activate %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Reading_Wait0: begin
	   $sformat(d_State,"r Wait0 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Reading_Read: begin
	   $sformat(d_State,"r Read %8X %8X (Read %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Reading_Wait1: begin
	   $sformat(d_State,"r Wait1 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Reading_Wait2: begin
	   $sformat(d_State,"r Wait2 %4X %4X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Reading_Wait3: begin
	   $sformat(d_State,"r Wait3 %4X %4X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Reading_Wait4: begin
	   $sformat(d_State,"r Wait4 %4X %4X (PreCharge %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Writing_Activate: begin
	   $sformat(d_State,"W Activate %8X %8X (Activate %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Writing_Wait0: begin
	   $sformat(d_State,"W Wait0 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Writing_Write: begin
	   $sformat(d_State,"W Write %8X %8X (Write %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Writing_Wait1: begin
	   $sformat(d_State,"W Wait1 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Writing_Wait2: begin
	   $sformat(d_State,"W Wait2 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Writing_Wait3: begin
	   $sformat(d_State,"W Wait3 %8X %8X (PreCharge %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Refreshing_PreChargeAll: begin
	   $sformat(d_State,"A PreChargeAll %8X %8X (PreCharge %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Refreshing_AutoRefresh: begin
	   $sformat(d_State,"A AutoRefresh %8X %8X (AutoRefresh %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Refreshing_Wait0: begin
	   $sformat(d_State,"A AutoRefresh1 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Refreshing_Wait1: begin
	   $sformat(d_State,"A AutoRefresh2 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Refreshing_Wait2: begin
	   $sformat(d_State,"A AutoRefresh3 %8X %8X (NoOperation %5B %2B %13B) %3D %1B",s_Address,s_Page,s_Command,s_Bank,s_Addr,s_RefreshCnt,s_ShouldRefresh);
	end

	`DdrCtl1_State_Error: begin
	   $sformat(d_State,"E");
	end

	default: begin
	   $sformat(d_State,"?");
	end
      endcase // case (s_State)
   end // always @ *
endmodule // DdrCtl1
