all: alu swc seq

alu: Alu.v AluTest.v
	iverilog -o AluTest.vvp Alu.v AluTest.v
	vvp AluTest.vvp

swc: Swc.v
	iverilog -o Swc Swc.v

seq: Seq.v
	iverilog -o Seq Seq.v


