`define DdrCtl1_NOP 4'h0
`define DdrCtl1_SA0 4'h1
`define DdrCtl1_SA1 4'h2
`define DdrCtl1_SA2 4'h3
`define DdrCtl1_SA3 4'h4
`define DdrCtl1_RDD 4'h5
`define DdrCtl1_LDD 4'h6

`define DdrCtl1_DdrCommand_PowerUp0         5'b00000
`define DdrCtl1_DdrCommand_PowerUp1         5'b10000
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

`define DdrCtl1_State_Reset                          6'h00
`define DdrCtl1_State_Initializing_PowerUp           6'h01
`define DdrCtl1_State_Initializing_Wait200us         6'h02
`define DdrCtl1_State_Initializing_BringCKEHigh      6'h03
`define DdrCtl1_State_Initializing_DoNop             6'h04
`define DdrCtl1_State_Initializing_PreChargeAll0     6'h05
`define DdrCtl1_State_Initializing_EnableDLL         6'h08
`define DdrCtl1_State_Initializing_ProgramMRResetDLL 6'h0A
`define DdrCtl1_State_Initializing_WaitMRD200DoNop   6'h0B
`define DdrCtl1_State_Initializing_PreChargeAll1     6'h0C
`define DdrCtl1_State_Initializing_AutoRefresh00     6'h0E
`define DdrCtl1_State_Initializing_AutoRefresh01     6'h0E
`define DdrCtl1_State_Initializing_AutoRefresh02     6'h0E
`define DdrCtl1_State_Initializing_AutoRefresh03     6'h0E
`define DdrCtl1_State_Initializing_AutoRefresh10     6'h0E
`define DdrCtl1_State_Initializing_AutoRefresh11     6'h0E
`define DdrCtl1_State_Initializing_AutoRefresh12     6'h0E
`define DdrCtl1_State_Initializing_AutoRefresh13     6'h0E
`define DdrCtl1_State_Initializing_ClearDLL          6'h10
`define DdrCtl1_State_Ready                          6'h12
`define DdrCtl1_State_Reading_Activate               6'h13
`define DdrCtl1_State_Reading_Wait                   6'h14
`define DdrCtl1_State_Reading_Read                   6'h15
`define DdrCtl1_State_Reading_BurstTerminate         6'h16
`define DdrCtl1_State_Writing_Activate               6'h18
`define DdrCtl1_State_Writing_Wait                   6'h19
`define DdrCtl1_State_Writing_Write                  6'h1A
`define DdrCtl1_State_Writing_BurstTerminate         6'h1B
`define DdrCtl1_State_AutoRefreshing_AutoRefresh0    6'h1D
`define DdrCtl1_State_AutoRefreshing_AutoRefresh1    6'h1D
`define DdrCtl1_State_AutoRefreshing_AutoRefresh2    6'h1D
`define DdrCtl1_State_AutoRefreshing_AutoRefresh3    6'h1D
`define DdrCtl1_State_Error                          6'h1F

module DdrCtl1(clock,reset,inst,inst_en,data,ready,ddr_cke,ddr_csn,ddr_rasn,ddr_casn,ddr_wen,ddr_ba,ddr_addr,ddr_dm,ddr_dq,ddr_dqs);
   input wire         clock;
   input wire         reset;

   input wire [11:0]  inst;
   input wire 	      inst_en;
   
   output wire [8:0]  data;
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

   reg [5:0] 	      s_State;
   reg [31:0] 	      s_Address;
   reg [4:0] 	      s_Command;
   reg [1:0] 	      s_Bank;
   reg [12:0] 	      s_Addr;
   reg [15:0] 	      s_Data;
   reg [13:0] 	      s_InitializeCnt;
   reg 		      s_AutoRefreshed;
   wire 	      i_Ready;

   reg [8:0] 	      s_AutoRefreshCnt;
   reg 		      s_ShouldAutoRefresh;

   wire [3:0] 	      w_InstCode;
   wire [7:0] 	      w_InstImm;

   reg [256*8-1:0]    d_Input;
   reg [256*8-1:0]    d_State;

   assign data = s_ReadCapture0;
   assign ready = i_Ready;

   assign ddr_cke = s_Command[4];
   assign ddr_csn = s_Command[3];
   assign ddr_rasn = s_Command[2];
   assign ddr_casn = s_Command[1];
   assign ddr_wen = s_Command[0];
   assign ddr_ba = s_Bank;
   assign ddr_addr = s_Addr;
   assign ddr_dm = 1;
   assign ddr_dq = s_State == DdrCtl1_State_Writing_Wait ? s_Data : 16'bzzzzzzzzzzzzzzzz;
   assign ddr_dqs = s_State == DdrCtl1_State_Writing_Wait ? 2'b11 : 2'bzz; // Trebuie diagrama de timp si sa vezi cum vine strobe-ul fata de ceasul principal.

   assign w_InstCode = inst[11:8];
   assign w_InstImm = inst[7:0];

   assign i_Ready = (s_State == DdrCtl1_State_Ready) ||
		    (s_State == DdrCtl1_State_Writing_BurstTerminate && !s_ShouldAutoRefresh) ||
		    (s_State == DdrCtl1_State_Reading_BurstTerminate && !s_ShouldAutoRefresh);

   always @ (negedge clock) begin
      if (ddr_dqs[0] == 1 && ddr_dqs[1] == 1) begin
	 s_ReadCapture0 <= ddr_dq;
      end
      else begin
	 s_ReadCapture0 <= s_ReadCapture0;
      end
   end

   always @ (posedge clock) begin
      if (ddr_dqs[0] == 1 && ddr_dqs[1] == 1) begin
	 s_ReadCapture1 <= ddr_dq;
      end
      else begin
	 s_ReadCapture1 <= s_ReadCapture1;
      end
   end

   always @ (posedge clock) begin
      if (reset) begin
	 s_State         <= `DdrCtl1_State_Reset;
	 s_Address       <= 0;
	 s_Command       <= `DdrCtl1_DdrCommand_Deselect;
	 s_Bank          <= 0;
	 s_Addr          <= 0;
	 s_Data          <= 0;
	 s_InitializeCnt <= 0;
	 s_AutoRefreshed <= 0;
      end
      else begin
	 case (s_State)
	   `DdrCtl1_State_Reset: begin
	      s_State         <= `DdrCtl1_State_Initializing_PowerUp;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_PowerUp: begin
	      s_State         <= `DdrCtl1_State_Initializing_Wait200us;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_PowerUp0;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_Wait200us: begin
	      if (s_InitializeCnt == 10000) begin
		 s_State         <= `DdrCtl1_State_Initializing_BringCKEHigh;
		 s_Address       <= 0;
		 s_Command       <= `DdrCtl1_DdrCommand_PowerUp0;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= 0;
		 s_AutoRefreshed <= 0;
	      end
	      else begin
		 s_State         <= `DdrCtl1_State_Initializing_Wait200us;
		 s_Address       <= 0;
		 s_Command       <= `DdrCtl1_DdrCommand_PowerUp0;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= s_InitializeCnt + 1;
		 s_AutoRefreshed <= 0;
	      end // else: !if(s_InitializeCnt == 10000)
	   end // case: `DdrCtl1_State_Initializing_Wait200us

	   `DdrCtl1_State_Initializing_BringCKEHigh: begin
	      s_State         <= `DdrCtl1_State_Initializing_DoNop;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_PowerUp1;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_DoNop: begin
	      s_State         <= `DdrCtl1_State_Initializing_PreChargeAll0;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_PreChargeAll0: begin
	      s_State         <= `DdrCtl1_State_Initializing_EnableDLL;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_PreCharge;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_EnableDLL: begin
	      s_State         <= `DdrCtl1_State_Initializing_ResetDLL;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_LoadModeRegister;
	      s_Bank          <= `DdrCtl1_SelectModeRegister_Extended;
	      s_Addr          <= {`DdrCtl1_DdrModeExtend_OperatingMode_Reserved,
                                  `DdrCtl1_DdrModeExtend_DriveStrength_Normal,
                                  `DdrCtl1_DdrModeExtend_DLL_Enable};
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end // case: `DdrCtl1_State_Initializing_EnableDLL

	   `DdrCtl1_State_Initializing_ProgramMRResetDLL: begin
	      s_State         <= `DdrCtl1_State_Initializing_WaitMRD200DoNop;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_LoadModeRegister;
	      s_Bank          <= `DdrCtl1_SelectModeRegister_Normal;
	      s_Addr          <= {`DdrCtl1_DdrMode_OperatingMode_NormalResetDLL<
                                  `DdrCtl1_DdrMode_CASLatency_2,
                                  `DdrCtl1_DdrMode_BurstType_Sequential,
                                  `DdrCtl1_DdrMode_BurstLength_2};
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end // case: `DdrCtl1_State_Initializing_ProgramMRResetDLL

	   `DdrCtl1_State_Initializing_WaitMRD200DoNop: begin
	      if (s_InitializeCnt == 200) begin
		 s_State         <= `DdrCtl1_State_Initializing_PreChargeAll1;
		 s_Address       <= 0;
		 s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= 0;
		 s_AutoRefreshed <= 0;
	      end
	      else begin
		 s_State         <= `DdrCtl1_State_Initializing_WaitMRD200DoNop;
		 s_Address       <= 0;
		 s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= s_InitializeCnt + 1;
		 s_AutoRefreshed <= 0;
	      end // else: !if(s_InitializeCnt == 200)
	   end // case: `DdrCtl1_State_Initializing_WaitMRD200DoNop


	   `DdrCtl1_State_Initializing_PreChargeAll1: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh00;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_PreCharge;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh00: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh01;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_AutoRefresh;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh01: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh02;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh02: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh03;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh03: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh10;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh10: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh11;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_AutoRefresh;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh11: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh12;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh12: begin
	      s_State         <= `DdrCtl1_State_Initializing_AutoRefresh13;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_AutoRefresh13: begin
	      s_State         <= `DdrCtl1_State_Initializing_ClearDLL;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Initializing_ClearDLL: begin
	      s_State         <= `DdrCtl1_State_Ready;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_LoadModeRegister;
	      s_Bank          <= `DdrCtl1_SelectModeRegister_Normal;
	      s_Addr          <= {`DdrCtl1_DdrMode_OperatingMode_Normal,
                                  `DdrCtl1_DdrMode_CASLatency_2,
                                  `DdrCtl1_DdrMode_BurstType_Sequential,
                                  `DdrCtl1_DdrMode_BurstLength_2};
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end // case: `DdrCtl1_State_Initializing_ClearDLL

	   `DdrCtl1_State_Ready: begin
	      if (inst_en) begin
		 case (w_InstCode)
		   `DdrCtl1_NOP: begin
		      s_State         <= `DdrCtl1_State_Ready;
		      s_Address       <= s_Address;
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= 0;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end

		   `DdrCtl1_SA0: begin
		      s_State         <= `DdrCtl1_State_Ready;
		      s_Address       <= {s_Address[31:8],w_InstImm};
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= 0;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end

		   `DdrCtl1_SA1: begin
		      s_State         <= `DdrCtl1_State_Ready;
		      s_Address       <= {s_Address[31:16],w_InstImm,s_Address[7:0]};
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= 0;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end

		   `DdrCtl1_SA2: begin
		      s_State         <= `DdrCtl1_State_Ready;
		      s_Address       <= {s_Address[31:24],w_InstImm,s_Address[15:0]};
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= 0;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end

		   `DdrCtl1_SA2: begin
		      s_State         <= `DdrCtl1_State_Ready;
		      s_Address       <= {w_InstImm,s_Address[23:0]};
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= 0;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end

		   `DdrCtl1_RDD: begin
		      s_State         <= `DdrCtl1_State_Reading_Activate;
		      s_Address       <= s_Address;
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= 0;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end

		   `DdrCtl1_LDD: begin
		      s_State         <= `DdrCtl1_State_Writing_Activate;
		      s_Address       <= s_Address;
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= w_InstImm;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end

		   default: begin
		      s_State         <= `DdrCtl1_State_Error;
		      s_Address       <= 0;
		      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		      s_Bank          <= 0;
		      s_Addr          <= 0;
		      s_Data          <= 0;
		      s_InitializeCnt <= 0;
		      s_AutoRefreshed <= 0;
		   end
		 endcase // case (w_InstCode)
	      end // if (inst_en)
	      else begin
		 s_State         <= `DdrCtl1_State_Ready;
		 s_Address       <= s_Address;
		 s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= 0;
		 s_AutoRefreshed <= 0;
	      end // else: !if(inst_en)
	   end // case: `DdrCtl1_State_Ready

	   `DdrCtl1_State_Reading_Activate: begin
	      s_State         <= `DdrCtl1_State_Reading_Wait;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_Activate;
	      s_Bank          <= s_Address[24:23];
	      s_Addr          <= s_Address[22:10];
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Reading_Wait: begin
	      s_State         <= `DdrCtl1_State_Reading_BurstTerminate;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bamk          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Reading_Read: begin
	      s_State         <= `DdrCtl1_State_Reading_BurstTerminate;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_Read;
	      s_Bamk          <= s_Address[24:23];
	      s_Addr          <= {3'b000,s_Address[9:0]};
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Reading_BurstTerminate: begin
	      if (s_ShouldAutoRefresh) begin
		 s_State         <= `DdrCtl1_State_AutoRefreshing_AutoRefresh0;
		 s_Address       <= s_Address;
		 s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= 0;
		 s_AutoRefreshed <= 0;
	      end
	      else begin
		 s_State         <= `DdrCtl1_State_Ready;
		 s_Address       <= s_Address;
		 s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= 0;
		 s_AutoRefreshed <= 0;
	      end
	   end // case: `DdrCtl1_State_Reading_BurstTerminate

	   `DdrCtl1_State_Writing_Activate: begin
	      s_State         <= `DdrCtl1_State_Writing_Wait;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_Activate;
	      s_Bank          <= s_Address[24:23];
	      s_Addr          <= s_Address[22:10];
	      s_Data          <= s_Data;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Writing_Wait: begin
	      s_State         <= `DdrCtl1_State_Writing_Write;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= s_Data;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Writing_Write: begin
	      s_State         <= `DdrCtl1_State_Writing_BurstTerminate;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_Write;
	      s_Bank          <= s_Address[24:23];
	      s_Addr          <= {3'b000,s_Address[9:0]};
	      s_Data          <= s_Data;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_Writing_BurstTerminate: begin
	      if (s_ShouldAutoRefresh) begin
		 s_State         <= `DdrCtl1_State_AutoRefreshing_AutoRefresh0;
		 s_Address       <= s_Address;
		 s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= 0;
		 s_AutoRefreshed <= 0;
	      end
	      else begin
		 s_State         <= `DdrCtl1_State_Ready;
		 s_Address       <= s_Address;
		 s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
		 s_Bank          <= 0;
		 s_Addr          <= 0;
		 s_Data          <= 0;
		 s_InitializeCnt <= 0;
		 s_AutoRefreshed <= 0;
	      end
	   end // case: `DdrCtl1_State_Writing_BurstTerminate

	   `DdrCtl1_State_AutoRefreshing_AutoRefresh0: begin
	      s_State         <= `DdrCtl1_State_AutoRefreshing_AutoRefresh1;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_AutoRefresh;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_AutoRefreshing_AutoRefresh1: begin
	      s_State         <= `DdrCtl1_State_AutoRefreshing_AutoRefresh2;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_AutoRefreshing_AutoRefresh2: begin
	      s_State         <= `DdrCtl1_State_AutoRefreshing_AutoRefresh3;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   `DdrCtl1_State_AutoRefreshing_AutoRefresh3: begin
	      s_State         <= `DdrCtl1_State_AutoRefreshing_Ready;
	      s_Address       <= s_Address;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 1;
	   end

	   `DdrCtl1_State_Error: begin
	      s_State         <= `DdrCtl1_State_Error;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end

	   default: begin
	      s_State         <= `DdrCtl1_State_Error;
	      s_Address       <= 0;
	      s_Command       <= `DdrCtl1_DdrCommand_NoOperation;
	      s_Bank          <= 0;
	      s_Addr          <= 0;
	      s_Data          <= 0;
	      s_InitializeCnt <= 0;
	      s_AutoRefreshed <= 0;
	   end
	 endcase // case (s_State)
      end // else: !if(reset)
   end // always @ (posedge clock)

   always @ (posedge clock) begin
      if (reset) begin
	 s_AutoRefreshCnt    <= 0;
	 s_ShouldAutoRefresh <= 0;
      end
      else begin
	 if (s_ShouldAutoRefresh == 1) begin
	    if (s_AutoRefreshed == 1) begin
	       s_AutoRefreshCnt    <= 0;
	       s_ShouldAutoRefresh <= 0;
	    end
	    else begin
	       s_AutoRefreshCnt    <= 0;
	       s_ShouldAutoRefresh <= 1;
	    end
	 end
	 else begin
	    if (s_AutoRefreshCnt == 400) begin
	       s_AutoRefreshCnt <= 0;
	       s_ShouldAutoRefresh <= 1;
	    end
	    else begin
	       s_AutoRefreshCnt    <= s_AutoRefreshCnt + 1;
	       s_ShouldAutoRefresh <= 0;
	    end
	 end // else: !if(s_ShouldAutoRefresh == 1)
      end // else: !if(reset)
   end // always @ (posedge clock)
endmodule // DdrCtl1
