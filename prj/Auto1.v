`define Send_ALU 4'bx000
`define Send_SWC 4'bx001

`define Recv_ALU 8'bxx00xxxx
`define Recv_SWC 8'bxx01xxxx

module Auto1(clock,reset,counter);
   input wire         clock;
   input wire 	      reset;

   output wire [23:0] counter;
   
   
   wire [7:0] 	      seq_next;
   wire [11:0] 	      seq_oreg;
   wire [7:0] 	      seq_oreg_wen;

   wire [11:0] 	      rom_data_o;

   wire [7:0] 	      alu_result;

   wire [23:0] 	      swc_counter;
   wire 	      swc_ready;

   assign counter = swc_counter;

   Seq
   seq (.clock(clock),
	.reset(reset),

	.inst(rom_data_o),
	.inst_en(1),
	.ireg_0(alu_result),
	.ireg_1({7'h0,swc_ready}),
	.ireg_2(8'h00),
	.ireg_3(8'h00),

	.next(seq_next),
	.oreg(seq_oreg),
	.oreg_wen(seq_oreg_wen));

   Auto1Rom
   rom (.addr(seq_next[3:0]),
	.data_o(rom_data_o));

   Alu
   alu (.clock(clock),
	.reset(reset),

	.inst(seq_oreg),
	.inst_en(seq_oreg_wen[0]),

	.result(alu_result));

   Swc
   swc (.clock(clock),
	.reset(reset),

	.inst(seq_oreg),
	.inst_en(seq_oreg_wen[1]),

	.counter(swc_counter),
	.ready(swc_ready));
endmodule // Auto1
