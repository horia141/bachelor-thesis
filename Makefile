all: alu swc seq auto1

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

auto1: Alu.v Seq.v Swc.v Rom.v Auto1.v Auto1Test.v out
	iverilog -o out/Auto1Test.vvp Alu.v Seq.v Swc.v Rom.v Auto1.v Auto1Test.v
	vvp out/Auto1Test.vvp

clean:
	rm -rf out
