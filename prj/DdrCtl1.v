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

`define DdrCtl1_IntfState_Reset     3'h0
`define DdrCtl1_IntfState_WaitInit  3'h1
`define DdrCtl1_IntfState_Ready     3'h2
`define DdrCtl1_IntfState_WaitRead  3'h3
`define DdrCtl1_IntfState_WaitWrite 3'h4
`define DdrCtl1_IntfState_Error     3'h5

`define DdrCtl1_CoreState_Reset                  5'h00
`define DdrCtl1_CoreState_WaitInit               5'h01
`define DdrCtl1_CoreState_Ready                  5'h02
`define DdrCtl1_CoreState_Refreshing_AutoRefresh 5'h03
`define DdrCtl1_CoreState_Refreshing_Wait0       5'h04
`define DdrCtl1_CoreState_Refreshing_Wait1       5'h05
`define DdrCtl1_CoreState_Refreshing_Wait2       5'h06
`define DdrCtl1_CoreState_Refreshing_Wait3       5'h07
`define DdrCtl1_CoreState_Reading_Activate       5'h08
`define DdrCtl1_CoreState_Reading_Wait0          5'h09
`define DdrCtl1_CoreState_Reading_Read           5'h0A
`define DdrCtl1_CoreState_Reading_Wait1          5'h0B
`define DdrCtl1_CoreState_Reading_Wait2          5'h0C
`define DdrCtl1_CoreState_Reading_Wait3          5'h0D
`define DdrCtl1_CoreState_Reading_PreChargeAll   5'h0E
`define DdrCtl1_CoreState_Writing_Activate       5'h0F
`define DdrCtl1_CoreState_Writing_Wait0          5'h10
`define DdrCtl1_CoreState_Writing_Write          5'h11
`define DdrCtl1_CoreState_Writing_Wait1          5'h12
`define DdrCtl1_CoreState_Writing_Wait2          5'h13
`define DdrCtl1_CoreState_Writing_PreChargeAll   5'h14

`define DdrCtl1_InitState_Reset                5'h00
`define DdrCtl1_InitState_PowerUp              5'h01
`define DdrCtl1_InitState_Wait200us            5'h02
`define DdrCtl1_InitState_BringCKEHigh         5'h03
`define DdrCtl1_InitState_PreChargeAll0        5'h04
`define DdrCtl1_InitState_EnableDLL            5'h05
`define DdrCtl1_InitState_ProgramMRResetDLL    5'h06
`define DdrCtl1_InitState_WaitMRD200           5'h07
`define DdrCtl1_InitState_PreChargeAll1        5'h08
`define DdrCtl1_InitState_Refresh0_AutoRefresh 5'h09
`define DdrCtl1_InitState_Refresh0_Wait0       5'h0A
`define DdrCtl1_InitState_Refresh0_Wait1       5'h0B
`define DdrCtl1_InitState_Refresh0_Wait2       5'h0C
`define DdrCtl1_InitState_Refresh1_AutoRefresh 5'h0D
`define DdrCtl1_InitState_Refresh1_Wait0       5'h0E
`define DdrCtl1_InitState_Refresh1_Wait1       5'h0F
`define DdrCtl1_InitState_Refresh1_Wait2       5'h10
`define DdrCtl1_InitState_ClearDLL             5'h11
`define DdrCtl1_InitState_Initialized          5'h12

module DdrCtl1(clock0,clock90,clock180,reset,inst,inst_en,page,ready,ddr_cke,ddr_csn,ddr_rasn,ddr_casn,ddr_wen,ddr_ba,ddr_addr,ddr_dm,ddr_dq,ddr_dqs);
   input wire         clock0;
   input wire 	      clock90;
   input wire 	      clock180;
   input wire         reset;

   input wire [11:0]  inst;
   input wire 	      inst_en;
   
   output wire [31:0] page;
   output wire 	      ready;

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

   wire 	      i_Ready;

   reg [2:0] 	      s_IntfState;
   reg [31:0] 	      s_IntfAddress;
   reg [31:0] 	      s_IntfPage;
   reg 		      i_IntfDoRead;
   reg 		      i_IntfDoWrite;

   reg [4:0] 	      s_CoreState;
   reg [4:0] 	      i_CoreCommand;
   reg [1:0] 	      i_CoreBank;
   reg [12:0] 	      i_CoreAddr;
   reg 		      i_CoreRefreshDone;
   reg 		      i_CoreReadDone;
   reg 		      i_CoreWriteDone;

   reg [4:0] 	      s_InitState;
   reg [4:0] 	      i_InitCommand;
   reg [1:0] 	      i_InitBank;
   reg [12:0] 	      i_InitAddr;
   reg 		      i_InitDone;
   reg 		      i_InitDo200us;
   reg 		      i_InitDo200;

   reg [8:0] 	      s_AutoRefreshCounter;
   reg 		      i_AutoRefreshDoRefresh;

   reg [13:0] 	      s_InitCnt200usCounter;
   reg 		      i_InitCnt200usDone;

   reg [7:0] 	      s_InitCnt200Counter;
   reg 		      i_InitCnt200Done;

   reg [15:0] 	      s_HalfPage0;
   reg [15:0] 	      s_HalfPage1;

   wire [3:0] 	      w_InstCode;
   wire [7:0] 	      w_InstImm;

   reg [256*8-1:0]    d_Input;
   reg [256*8-1:0]    d_IntfState;
   reg [256*8-1:0]    d_CoreState;
   reg [256*8-1:0]    d_InitState;
   reg [256*8-1:0]    d_AutoRefreshCounter;
   reg [256*8-1:0]    d_InitCnt200usCounter;
   reg [256*8-1:0]    d_InitCnt200Counter;

   // synthesis attribute fsm_encoding of s_IntfState is speed1;
   // synthesis attribute fsm_encoding of s_CoreState is speed1;
   // synthesis attribute fsm_encoding of s_InitState is speed1;

   assign page = s_IntfPage;
   assign ready = i_Ready;

   assign ddr_cke = i_InitDone ? i_CoreCommand[4] : i_InitCommand[4];
   assign ddr_csn = i_InitDone ? i_CoreCommand[3] : i_InitCommand[3];
   assign ddr_rasn = i_InitDone ? i_CoreCommand[2] : i_InitCommand[2];
   assign ddr_casn = i_InitDone ? i_CoreCommand[1] : i_InitCommand[1];
   assign ddr_wen = i_InitDone ? i_CoreCommand[0] : i_InitCommand[0];
   assign ddr_ba = i_InitDone ? i_CoreBank : i_InitBank;
   assign ddr_addr = i_InitDone ? i_CoreAddr : i_InitAddr;
   assign ddr_dm = 2'b00;
   assign ddr_dq = s_CoreState == `DdrCtl1_CoreState_Writing_Wait1 || s_CoreState == `DdrCtl1_CoreState_Writing_Wait2 ? (clock90 == 1 ? s_IntfPage[31:16] : s_IntfPage[15:0]) : 16'bzzzzzzzzzzzzzzzz;
   assign ddr_dqs = s_CoreState == `DdrCtl1_CoreState_Writing_Wait1 || s_CoreState == `DdrCtl1_CoreState_Writing_Wait2 ? {clock180,clock180} : 2'bzz;

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   assign i_Ready = s_IntfState == `DdrCtl1_IntfState_Ready &&
		    s_CoreState == `DdrCtl1_CoreState_Ready &&
		    w_InstCode != `DdrCtl1_RDP &&
		    w_InstCode != `DdrCtl1_WRP;

   always @ (posedge clock0) begin
      if (reset) begin
	 s_IntfState   <= `DdrCtl1_IntfState_Reset;
	 s_IntfAddress <= 0;
	 s_IntfPage    <= 0;
      end
      else begin
	 case (s_IntfState)
	   `DdrCtl1_IntfState_Reset: begin
	      s_IntfState   <= `DdrCtl1_IntfState_WaitInit;
	      s_IntfAddress <= 0;
	      s_IntfPage    <= 0;
	   end

	   `DdrCtl1_IntfState_WaitInit: begin
	      if (i_InitDone) begin
		 s_IntfState   <= `DdrCtl1_IntfState_Ready;
		 s_IntfAddress <= 0;
		 s_IntfPage    <= 0;
	      end
	      else begin
		 s_IntfState   <= `DdrCtl1_IntfState_WaitInit;
		 s_IntfAddress <= 0;
		 s_IntfPage    <= 0;
	      end
	   end // case: `DdrCtl1_IntfState_WaitInit

	   `DdrCtl1_IntfState_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `DdrCtl1_NOP: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= s_IntfAddress;
		      s_IntfPage    <= s_IntfPage;
		   end

		   `DdrCtl1_LA0: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= {s_IntfAddress[31:8],w_InstImm};
		      s_IntfPage    <= s_IntfPage;
		   end

		   `DdrCtl1_LA1: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= {s_IntfAddress[31:16],w_InstImm,s_IntfAddress[7:0]};
		      s_IntfPage    <= s_IntfPage;
		   end

		   `DdrCtl1_LA2: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= {s_IntfAddress[31:24],w_InstImm,s_IntfAddress[15:0]};
		      s_IntfPage    <= s_IntfPage;
		   end

		   `DdrCtl1_LA3: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= {w_InstImm,s_IntfAddress[23:0]};
		      s_IntfPage    <= s_IntfPage;
		   end

		   `DdrCtl1_LD0: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= s_IntfAddress;
		      s_IntfPage    <= {s_IntfPage[31:8],w_InstImm};
		   end

		   `DdrCtl1_LD1: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= s_IntfAddress;
		      s_IntfPage    <= {s_IntfPage[31:16],w_InstImm,s_IntfPage[7:0]};
		   end

		   `DdrCtl1_LD2: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= s_IntfAddress;
		      s_IntfPage    <= {s_IntfPage[31:24],w_InstImm,s_IntfPage[15:0]};
		   end

		   `DdrCtl1_LD3: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Ready;
		      s_IntfAddress <= s_IntfAddress;
		      s_IntfPage    <= {w_InstImm,s_IntfPage[23:0]};
		   end

		   `DdrCtl1_RDP: begin
		      s_IntfState   <= `DdrCtl1_IntfState_WaitRead;
		      s_IntfAddress <= s_IntfAddress;
		      s_IntfPage    <= s_IntfPage;
		   end

		   `DdrCtl1_WRP: begin
		      s_IntfState   <= `DdrCtl1_IntfState_WaitWrite;
		      s_IntfAddress <= s_IntfAddress;
		      s_IntfPage    <= s_IntfPage;
		   end

		   default: begin
		      s_IntfState   <= `DdrCtl1_IntfState_Error;
		      s_IntfAddress <= 0;
		      s_IntfPage    <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 s_IntfState   <= `DdrCtl1_IntfState_Ready;
		 s_IntfAddress <= s_IntfAddress;
		 s_IntfPage    <= s_IntfPage;
	      end // else: !if(inst_en)
	   end // case: `DdrCtl1_IntfState_Ready

	   `DdrCtl1_IntfState_WaitRead: begin
	      if (i_CoreReadDone) begin
		 s_IntfState   <= `DdrCtl1_IntfState_Ready;
		 s_IntfAddress <= s_IntfAddress;
		 s_IntfPage    <= {s_HalfPage1,s_HalfPage0};
	      end
	      else begin
		 s_IntfState   <= `DdrCtl1_IntfState_WaitRead;
		 s_IntfAddress <= s_IntfAddress;
		 s_IntfPage    <= s_IntfPage;
	      end
	   end // case: `DdrCtl1_IntfState_WaitRead

	   `DdrCtl1_IntfState_WaitWrite: begin
	      if (i_CoreWriteDone) begin
		 s_IntfState   <= `DdrCtl1_IntfState_Ready;
		 s_IntfAddress <= s_IntfAddress;
		 s_IntfPage    <= s_IntfPage;
	      end
	      else begin
		 s_IntfState   <= `DdrCtl1_IntfState_WaitWrite;
		 s_IntfAddress <= s_IntfAddress;
		 s_IntfPage    <= s_IntfPage;
	      end
	   end // case: `DdrCtl1_IntfState_WaitRead
		 
	   `DdrCtl1_IntfState_Error: begin
	      s_IntfState   <= `DdrCtl1_IntfState_Error;
	      s_IntfAddress <= 0;
	      s_IntfPage    <= 0;
	   end

	   default: begin
	      s_IntfState   <= `DdrCtl1_IntfState_Error;
	      s_IntfAddress <= 0;
	      s_IntfPage    <= 0;
	   end
	 endcase // case (s_IntfState)
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ * begin
      if (reset) begin
	 i_IntfDoRead  = 0;
	 i_IntfDoWrite = 0;
      end
      else begin
	 case (s_IntfState)
	   `DdrCtl1_IntfState_Reset: begin
	      i_IntfDoRead  = 0;
	      i_IntfDoWrite = 0;
	   end

	   `DdrCtl1_IntfState_WaitInit: begin
	      i_IntfDoRead  = 0;
	      i_IntfDoWrite = 0;
	   end

	   `DdrCtl1_IntfState_Ready: begin
	      i_IntfDoRead  = 0;
	      i_IntfDoWrite = 0;
	   end

	   `DdrCtl1_IntfState_WaitRead: begin
	      i_IntfDoRead  = 1;
	      i_IntfDoWrite = 0;
	   end

	   `DdrCtl1_IntfState_WaitWrite: begin
	      i_IntfDoRead  = 0;
	      i_IntfDoWrite = 1;
	   end

	   `DdrCtl1_IntfState_Error: begin
	      i_IntfDoRead  = 0;
	      i_IntfDoWrite = 0;
	   end

	   default: begin
	      i_IntfDoRead  = 0;
	      i_IntfDoWrite = 0;
	   end
	 endcase // case (s_IntfState)
      end // else: !if(reset)
   end // always @ *

   always @ (posedge clock0) begin
      if (reset) begin
	 s_CoreState <= `DdrCtl1_CoreState_Reset;
      end
      else begin
	 case (s_CoreState)
	   `DdrCtl1_CoreState_Reset: begin
	      s_CoreState <= `DdrCtl1_CoreState_WaitInit;
	   end

	   `DdrCtl1_CoreState_WaitInit: begin
	      if (i_InitDone) begin
		 s_CoreState <= `DdrCtl1_CoreState_Ready;
	      end
	      else begin
		 s_CoreState <= `DdrCtl1_CoreState_WaitInit;
	      end
	   end

	   `DdrCtl1_CoreState_Ready: begin
	      case ({i_AutoRefreshDoRefresh,i_IntfDoRead,i_IntfDoWrite})
		3'b000: begin
		   s_CoreState <= `DdrCtl1_CoreState_Ready;
		end

		3'b001: begin
		   s_CoreState <= `DdrCtl1_CoreState_Writing_Activate;
		end

		3'b010: begin
		   s_CoreState <= `DdrCtl1_CoreState_Reading_Activate;
		end

		3'b011: begin
		   s_CoreState <= `DdrCtl1_CoreState_Reading_Activate;
		end

		3'b100: begin
		   s_CoreState <= `DdrCtl1_CoreState_Refreshing_AutoRefresh;
		end

		3'b101: begin
		   s_CoreState <= `DdrCtl1_CoreState_Refreshing_AutoRefresh;
		end

		3'b110: begin
		   s_CoreState <= `DdrCtl1_CoreState_Refreshing_AutoRefresh;
		end

		3'b111: begin
		   s_CoreState <= `DdrCtl1_CoreState_Refreshing_AutoRefresh;
		end
	      endcase // case ({i_RefreshDoAutoRefresh,i_IntfDoRead,i_IntfDoWrite})
	   end // case: `DdrCtl1_CoreState_Ready

	   `DdrCtl1_CoreState_Refreshing_AutoRefresh: begin
	      s_CoreState <= `DdrCtl1_CoreState_Refreshing_Wait0;
	   end

	   `DdrCtl1_CoreState_Refreshing_Wait0: begin
	      s_CoreState <= `DdrCtl1_CoreState_Refreshing_Wait1;
	   end
	   	
	   `DdrCtl1_CoreState_Refreshing_Wait1: begin
	      s_CoreState <= `DdrCtl1_CoreState_Refreshing_Wait2;
	   end

	   `DdrCtl1_CoreState_Refreshing_Wait2: begin
	      s_CoreState <= `DdrCtl1_CoreState_Refreshing_Wait3;
	   end

	   `DdrCtl1_CoreState_Refreshing_Wait3: begin
	      s_CoreState <= `DdrCtl1_CoreState_Ready;
	   end

	   `DdrCtl1_CoreState_Reading_Activate: begin
	      s_CoreState <= `DdrCtl1_CoreState_Reading_Wait0;
	   end

	   `DdrCtl1_CoreState_Reading_Wait0: begin
	      s_CoreState <= `DdrCtl1_CoreState_Reading_Read;
	   end

	   `DdrCtl1_CoreState_Reading_Read: begin
	      s_CoreState <= `DdrCtl1_CoreState_Reading_Wait1;
	   end

	   `DdrCtl1_CoreState_Reading_Wait1: begin
	      s_CoreState <= `DdrCtl1_CoreState_Reading_Wait2;
	   end

	   `DdrCtl1_CoreState_Reading_Wait2: begin
	      s_CoreState <= `DdrCtl1_CoreState_Reading_Wait3;
	   end

	   `DdrCtl1_CoreState_Reading_Wait3: begin
	      s_CoreState <= `DdrCtl1_CoreState_Reading_PreChargeAll;
	   end

	   `DdrCtl1_CoreState_Reading_PreChargeAll: begin
	      s_CoreState <= `DdrCtl1_CoreState_Ready;
	   end

	   `DdrCtl1_CoreState_Writing_Activate: begin
	      s_CoreState <= `DdrCtl1_CoreState_Writing_Wait0;
	   end

	   `DdrCtl1_CoreState_Writing_Wait0: begin
	      s_CoreState <= `DdrCtl1_CoreState_Writing_Write;
	   end

	   `DdrCtl1_CoreState_Writing_Write: begin
	      s_CoreState <= `DdrCtl1_CoreState_Writing_Wait1;
	   end

	   `DdrCtl1_CoreState_Writing_Wait1: begin
	      s_CoreState <= `DdrCtl1_CoreState_Writing_Wait2;
	   end

	   `DdrCtl1_CoreState_Writing_Wait2: begin
	      s_CoreState <= `DdrCtl1_CoreState_Writing_PreChargeAll;
	   end

	   `DdrCtl1_CoreState_Writing_PreChargeAll: begin
	      s_CoreState <= `DdrCtl1_CoreState_Ready;
	   end

	   default: begin
	      s_CoreState <= `DdrCtl1_CoreState_Reset;
	   end
	 endcase // case (s_CoreState)
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ * begin
      if (reset) begin
	 i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	 i_CoreBank        = 0;
	 i_CoreAddr        = 0;
	 i_CoreRefreshDone = 0;
	 i_CoreReadDone    = 0;
	 i_CoreWriteDone   = 0;
      end
      else begin
	 case (s_CoreState)
	   `DdrCtl1_CoreState_Reset: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_WaitInit: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Ready: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Refreshing_AutoRefresh: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_AutoRefresh;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Refreshing_Wait0: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Refreshing_Wait1: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Refreshing_Wait2: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Refreshing_Wait3: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 1;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Reading_Activate: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_Activate;
	      i_CoreBank        = s_IntfAddress[24:23];
	      i_CoreAddr        = s_IntfAddress[22:10];
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Reading_Wait0: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Reading_Read: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_Read;
	      i_CoreBank        = s_IntfAddress[24:23];
	      i_CoreAddr        = {3'b000,s_IntfAddress[9:0]};
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Reading_Wait1: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Reading_Wait2: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Reading_Wait3: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Reading_PreChargeAll: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_PreCharge;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 13'b0010000000000;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 1;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Writing_Activate: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_Activate;
	      i_CoreBank        = s_IntfAddress[24:23];
	      i_CoreAddr        = s_IntfAddress[22:10];
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Writing_Wait0: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Writing_Write: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_Write;
	      i_CoreBank        = s_IntfAddress[24:23];
	      i_CoreAddr        = {3'b000,s_IntfAddress[9:0]};
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Writing_Wait1: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Writing_Wait2: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end

	   `DdrCtl1_CoreState_Writing_PreChargeAll: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_PreCharge;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 13'b0010000000000;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 1;
	   end

	   default: begin
	      i_CoreCommand     = `DdrCtl1_DdrCommand_NoOperation;
	      i_CoreBank        = 0;
	      i_CoreAddr        = 0;
	      i_CoreRefreshDone = 0;
	      i_CoreReadDone    = 0;
	      i_CoreWriteDone   = 0;
	   end
	 endcase // case (s_CoreState)
      end // else: !if(reset)
   end // always @ *

   always @ (posedge clock0) begin
      if (reset) begin
	 s_InitState <= `DdrCtl1_InitState_Reset;
      end
      else begin
	 case (s_InitState)
	   `DdrCtl1_InitState_Reset: begin
	      s_InitState <= `DdrCtl1_InitState_PowerUp;
	   end

	   `DdrCtl1_InitState_PowerUp: begin
	      s_InitState <= `DdrCtl1_InitState_Wait200us;
	   end

	   `DdrCtl1_InitState_Wait200us: begin
	      if (i_InitCnt200usDone) begin
		 s_InitState <= `DdrCtl1_InitState_BringCKEHigh;
	      end
	      else begin
		 s_InitState <= `DdrCtl1_InitState_Wait200us;
	      end
	   end

	   `DdrCtl1_InitState_BringCKEHigh: begin
	      s_InitState <= `DdrCtl1_InitState_PreChargeAll0;
	   end

	   `DdrCtl1_InitState_PreChargeAll0: begin
	      s_InitState <= `DdrCtl1_InitState_EnableDLL;
	   end

	   `DdrCtl1_InitState_EnableDLL: begin
	      s_InitState <= `DdrCtl1_InitState_ProgramMRResetDLL;
	   end

	   `DdrCtl1_InitState_ProgramMRResetDLL: begin
	      s_InitState <= `DdrCtl1_InitState_WaitMRD200;
	   end

	   `DdrCtl1_InitState_WaitMRD200: begin
	      if (i_InitCnt200Done) begin
		 s_InitState <= `DdrCtl1_InitState_PreChargeAll1;
	      end
	      else begin
		 s_InitState <= `DdrCtl1_InitState_WaitMRD200;
	      end
	   end

	   `DdrCtl1_InitState_PreChargeAll1: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh0_AutoRefresh;
	   end

	   `DdrCtl1_InitState_Refresh0_AutoRefresh: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh0_Wait0;
	   end

	   `DdrCtl1_InitState_Refresh0_Wait0: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh0_Wait1;
	   end

	   `DdrCtl1_InitState_Refresh0_Wait1: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh0_Wait2;
	   end

	   `DdrCtl1_InitState_Refresh0_Wait2: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh1_AutoRefresh;
	   end

	   `DdrCtl1_InitState_Refresh1_AutoRefresh: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh1_Wait0;
	   end

	   `DdrCtl1_InitState_Refresh1_Wait0: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh1_Wait1;
	   end

	   `DdrCtl1_InitState_Refresh1_Wait1: begin
	      s_InitState <= `DdrCtl1_InitState_Refresh1_Wait2;
	   end

	   `DdrCtl1_InitState_Refresh1_Wait2: begin
	      s_InitState <= `DdrCtl1_InitState_ClearDLL;
	   end

	   `DdrCtl1_InitState_ClearDLL: begin
	      s_InitState <= `DdrCtl1_InitState_Initialized;
	   end

	   `DdrCtl1_InitState_Initialized: begin
	      s_InitState <= `DdrCtl1_InitState_Initialized;
	   end

	   default: begin
	      s_InitState <= `DdrCtl1_InitState_Reset;
	   end
	 endcase // case (s_InitState)
      end // else: !if(reset)
   end // always @ (posedge init0)

   always @ * begin
      if (reset) begin
	 i_InitCommand = `DdrCtl1_DdrCommand_PowerUp;
	 i_InitBank    = 0;
	 i_InitAddr    = 0;
	 i_InitDone    = 0;
	 i_InitDo200us = 0;
	 i_InitDo200   = 0;
      end
      else begin
	 case (s_InitState)
	   `DdrCtl1_InitState_Reset: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_PowerUp;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_PowerUp: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_PowerUp;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Wait200us: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_PowerUp;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 1;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_BringCKEHigh: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_PreChargeAll0: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_PreCharge;
	      i_InitBank    = 0;
	      i_InitAddr    = 13'b0010000000000;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_EnableDLL: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_LoadModeRegister;
	      i_InitBank    = `DdrCtl1_SelectModeRegister_Extended;
	      i_InitAddr    = {`DdrCtl1_DdrModeExtend_OperatingMode_Reserved,
                               `DdrCtl1_DdrModeExtend_DriveStrength_Normal,
                               `DdrCtl1_DdrModeExtend_DLL_Enable};
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_ProgramMRResetDLL: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_LoadModeRegister;
	      i_InitBank    = `DdrCtl1_SelectModeRegister_Normal;
	      i_InitAddr    = {`DdrCtl1_DdrMode_OperatingMode_NormalResetDLL,
                               `DdrCtl1_DdrMode_CASLatency_2,
                               `DdrCtl1_DdrMode_BurstType_Sequential,
                               `DdrCtl1_DdrMode_BurstLength_2};
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_WaitMRD200: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 1;
	   end

	   `DdrCtl1_InitState_PreChargeAll1: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_PreCharge;
	      i_InitBank    = 0;
	      i_InitAddr    = 13'b0010000000000;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh0_AutoRefresh: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_AutoRefresh;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh0_Wait0: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh0_Wait1: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh0_Wait2: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh1_AutoRefresh: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_AutoRefresh;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh1_Wait0: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh1_Wait1: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Refresh1_Wait2: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_ClearDLL: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_LoadModeRegister;
	      i_InitBank    = `DdrCtl1_SelectModeRegister_Normal;
	      i_InitAddr    = {`DdrCtl1_DdrMode_OperatingMode_Normal,
                               `DdrCtl1_DdrMode_CASLatency_2,
                               `DdrCtl1_DdrMode_BurstType_Sequential,
                               `DdrCtl1_DdrMode_BurstLength_2};
	      i_InitDone    = 0;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   `DdrCtl1_InitState_Initialized: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 1;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end

	   default: begin
	      i_InitCommand = `DdrCtl1_DdrCommand_NoOperation;
	      i_InitBank    = 0;
	      i_InitAddr    = 0;
	      i_InitDone    = 1;
	      i_InitDo200us = 0;
	      i_InitDo200   = 0;
	   end
	 endcase // case (s_InitState)
      end // else: !if(reset)
   end // always @ *

   always @ (posedge clock0) begin
      if (reset) begin
	 s_AutoRefreshCounter <= 0;
      end
      else begin
	 if (i_InitDone) begin
	    if (s_AutoRefreshCounter == 395) begin
	       if (i_CoreRefreshDone) begin
		  s_AutoRefreshCounter <= 0;
	       end
	       else begin
		  s_AutoRefreshCounter <= s_AutoRefreshCounter;
	       end
	    end
	    else begin
	       s_AutoRefreshCounter <= s_AutoRefreshCounter + 1;
	    end // else: !if(i_InitDone)
	 end // if (i_InitDone)
	 else begin
	    s_AutoRefreshCounter <= 0;
	 end // else: !if(i_InitDone)
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ * begin
      if (reset) begin
	 i_AutoRefreshDoRefresh = 0;
      end
      else begin
	 if (s_AutoRefreshCounter == 395) begin
	    i_AutoRefreshDoRefresh = 1;
	 end
	 else begin
	    i_AutoRefreshDoRefresh = 0;
	 end
      end // else: !if(reset)
   end // always @ *

   always @ (posedge clock0) begin
      if (reset) begin
	 s_InitCnt200usCounter <= 0;
      end
      else begin
	 if (i_InitDo200us) begin
	    s_InitCnt200usCounter <= s_InitCnt200usCounter + 1;
	 end
	 else begin
	    s_InitCnt200usCounter <= 0;
	 end
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ * begin
      if (reset) begin
	 i_InitCnt200usDone = 0;
      end
      else begin
	 if (s_InitCnt200usCounter == 10000) begin
	    i_InitCnt200usDone = 1;
	 end
	 else begin
	    i_InitCnt200usDone = 0;
	 end
      end // else: !if(reset)
   end // always @ *

   always @ (posedge clock0) begin
      if (reset) begin
	 s_InitCnt200Counter <= 0;
      end
      else begin
	 if (i_InitDo200) begin
	    s_InitCnt200Counter <= s_InitCnt200Counter + 1;
	 end
	 else begin
	    s_InitCnt200Counter <= 0;
	 end
      end // else: !if(reset)
   end // always @ (posedge clock0)

   always @ * begin
      if (reset) begin
	 i_InitCnt200Done = 0;
      end
      else begin
	 if (s_InitCnt200Counter == 200) begin
	    i_InitCnt200Done = 1;
	 end
	 else begin
	    i_InitCnt200Done = 0;
	 end
      end // else: !if(reset)
   end // always @ *

   always @ (posedge clock90) begin
      if (s_CoreState == `DdrCtl1_CoreState_Reading_Wait3 && ddr_dqs[0] == 0 && ddr_dqs[1] == 0) begin
	 s_HalfPage0 <= ddr_dq;
      end
      else begin
	 s_HalfPage0 <= s_HalfPage0;
      end
   end

   always @ (negedge clock90) begin
      if (s_CoreState == `DdrCtl1_CoreState_Reading_Wait2 && ddr_dqs[0] == 1 && ddr_dqs[1] == 1) begin
	 s_HalfPage1 <= ddr_dq;
      end
      else begin
	 s_HalfPage1 <= s_HalfPage1;
      end
   end

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
      case (s_IntfState)
	`DdrCtl1_IntfState_Reset: begin
	   $sformat(d_IntfState,"X");
	end

	`DdrCtl1_IntfState_WaitInit: begin
	   $sformat(d_IntfState,"I %S",
		    i_InitDone ? "InitDone" : "InitNotDone");
	end

	`DdrCtl1_IntfState_Ready: begin
	   $sformat(d_IntfState,"R %4X %4X (%S %S) (%S %S)",
		    s_IntfAddress,
		    s_IntfPage,
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone");
	end

	`DdrCtl1_IntfState_WaitRead: begin
	   $sformat(d_IntfState,"r %4X %4X (%S %S) (%S %S)",
		    s_IntfAddress,
		    s_IntfPage,
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone");
	end

	`DdrCtl1_IntfState_WaitWrite: begin
	   $sformat(d_IntfState,"w %4X %4X (%S %S) (%S %S)",
		    s_IntfAddress,
		    s_IntfPage,
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone");
	end

	`DdrCtl1_IntfState_Error: begin
	   $sformat(d_IntfState,"E");
	end

	default: begin
	   $sformat(d_IntfState,"?");
	end
      endcase // case (s_IntfState)
   end // always @ *

   always @ * begin
      case (s_CoreState)
	`DdrCtl1_CoreState_Reset: begin
	   $sformat(d_CoreState,"X");
	end

	`DdrCtl1_CoreState_WaitInit: begin
	   $sformat(d_CoreState,"I %S",
		    i_InitDone ? "InitDone" : "InitNotDone");
	end

	`DdrCtl1_CoreState_Ready: begin
	   $sformat(d_CoreState,"R %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Refreshing_AutoRefresh: begin
	   $sformat(d_CoreState,"A AutoRefresh %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Refreshing_Wait0: begin
	   $sformat(d_CoreState,"A Wait0 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Refreshing_Wait1: begin
	   $sformat(d_CoreState,"A Wait1 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Refreshing_Wait2: begin
	   $sformat(d_CoreState,"A Wait2 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end // case: `DdrCtl1_CoreState_Refreshing_Wait2

	`DdrCtl1_CoreState_Refreshing_Wait3: begin
	   $sformat(d_CoreState,"A Wait3 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end // case: `DdrCtl1_CoreState_Refreshing_Wait3
	
	`DdrCtl1_CoreState_Reading_Activate: begin
	   $sformat(d_CoreState,"r Activate %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end // case: `DdrCtl1_CoreState_Reading_Activate

	`DdrCtl1_CoreState_Reading_Wait0: begin
	   $sformat(d_CoreState,"r Wait0 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end // case: `DdrCtl1_CoreState_Reading_Wait0

	`DdrCtl1_CoreState_Reading_Read: begin
	   $sformat(d_CoreState,"r Read %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Reading_Wait1: begin
	   $sformat(d_CoreState,"r Wait1 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Reading_Wait2: begin
	   $sformat(d_CoreState,"r Wait2 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Reading_Wait3: begin
	   $sformat(d_CoreState,"r Wait3 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Reading_PreChargeAll: begin
	   $sformat(d_CoreState,"r PreChargeAll %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Writing_Activate: begin
	   $sformat(d_CoreState,"w Activate %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Writing_Wait0: begin
	   $sformat(d_CoreState,"w Wait0 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Writing_Write: begin
	   $sformat(d_CoreState,"w Write %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Writing_Wait1: begin
	   $sformat(d_CoreState,"w Wait1 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Writing_Wait2: begin
	   $sformat(d_CoreState,"w Wait2 %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	`DdrCtl1_CoreState_Writing_PreChargeAll: begin
	   $sformat(d_CoreState,"w PreChargeAll %5B %2B %4X (%S %S %S) (%S %S %S)",
		    i_CoreCommand,
		    i_CoreBank,
		    i_CoreAddr,
		    i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone",
		    i_CoreReadDone ? "ReadDone" : "ReadNotDone",
		    i_CoreWriteDone ? "WriteDone" : "WriteNotDone",
		    i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
		    i_IntfDoRead ? "DoRead" : "NoRead",
		    i_IntfDoWrite ? "DoWrite" : "NoWrite");
	end

	default: begin
	   $sformat(d_CoreState,"?");
	end
      endcase // case (s_CoreState)
   end // always @ *

   always @ * begin
      case (s_InitState)
	`DdrCtl1_InitState_Reset: begin
	   $sformat(d_InitState,"R");
	end

	`DdrCtl1_InitState_PowerUp: begin
	   $sformat(d_InitState,"I PowerUp %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Wait200us: begin
	   $sformat(d_InitState,"I Wait200us %5B %2B %4X %S %S",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr,
		    i_InitDo200us ? "Do200us" : "No200us",
		    i_InitCnt200usDone ? "200usDone" : "200usNotDone");
	end

	`DdrCtl1_InitState_BringCKEHigh: begin
	   $sformat(d_InitState,"I BringCKEHigh %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_PreChargeAll0: begin
	   $sformat(d_InitState,"I PreChargeAll0 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_EnableDLL: begin
	   $sformat(d_InitState,"I EnableDLL %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_ProgramMRResetDLL: begin
	   $sformat(d_InitState,"I ProgramMRResetDLL %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_WaitMRD200: begin
	   $sformat(d_InitState,"I Wait200 %5B %2B %4X %S %S",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr,
		    i_InitDo200 ? "Do200" : "No200",
		    i_InitCnt200Done ? "200Done" : "200NotDone");	   
	end

	`DdrCtl1_InitState_PreChargeAll1: begin
	   $sformat(d_InitState,"I PreChargeAll1 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh0_AutoRefresh: begin
	   $sformat(d_InitState,"I Refresh0 AutoRefresh %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh0_Wait0: begin
	   $sformat(d_InitState,"I Refresh0 Wait0 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh0_Wait1: begin
	   $sformat(d_InitState,"I Refresh0 Wait1 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh0_Wait2: begin
	   $sformat(d_InitState,"I Refresh0 Wait2 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh1_AutoRefresh: begin
	   $sformat(d_InitState,"I Refresh1 AutoRefresh %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh1_Wait0: begin
	   $sformat(d_InitState,"I Refresh1 Wait0 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh1_Wait1: begin
	   $sformat(d_InitState,"I Refresh1 Wait1 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Refresh1_Wait2: begin
	   $sformat(d_InitState,"I Refresh1 Wait2 %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_ClearDLL: begin
	   $sformat(d_InitState,"I ClearDLL %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	`DdrCtl1_InitState_Initialized: begin
	   $sformat(d_InitState,"I Initialized %5B %2B %4X",
		    i_InitCommand,
		    i_InitBank,
		    i_InitAddr);
	end

	default: begin
	   $sformat(d_InitState,"?");
	end
      endcase // case (s_InitState)
   end // always @ *

   always @ * begin
      $sformat(d_AutoRefreshCounter,"%3D %S %S",
	       s_AutoRefreshCounter,
	       i_AutoRefreshDoRefresh ? "DoRefresh" : "NoRefresh",
	       i_CoreRefreshDone ? "RefreshDone" : "RefreshNotDone");
   end

   always @ * begin
      $sformat(d_InitCnt200usCounter,"%5D %S %S",
	       s_InitCnt200usCounter,
	       i_InitCnt200usDone ? "200usDone" : "200usNotDone",
	       i_InitDo200us ? "Do200us" : "No200us");
   end

   always @ * begin
      $sformat(d_InitCnt200Counter,"%3D %S %S",
	       s_InitCnt200Counter,
	       i_InitCnt200Done ? "200Done" : "200NotDone",
	       i_InitDo200 ? "Do200" : "No200");
   end
endmodule // DdrCtl1
