all: alu swc seq

out:
	mkdir -p out

alu: Alu.v AluTest.v out
	iverilog -o out/AluTest.vvp Alu.v AluTest.v
	vvp out/AluTest.vvp

swc: Swc.v SwcTest.v out
	iverilog -o out/SwcTest.vvp Swc.v SwcTest.v
	vvp out/SwcTest.vvp

seq: Seq.v SeqTest.v out
	iverilog -o out/SeqTest.vvp Seq.v SeqTest.v
	vvp out/SeqTest.vvp

clean:
	rm -rf out
