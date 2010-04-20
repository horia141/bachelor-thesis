# Configuration Variables

Projects = AluTest SwcTest SeqTest Auto1Test

AluTest_Src = Alu.v AluTest.v
AluTest_Mem = 
AluTest_Out = AluTest.vvp

SwcTest_Src = Swc.v SwcTest.v
SwcTest_Mem =
SwcTest_Out = SwcTest.vvp

SeqTest_Src = Seq.v SeqTest.v
SeqTest_Mem = 
SeqTest_Out = SeqTest.vvp

Auto1Test_Src = Alu.v Swc.v Seq.v Auto1.v Auto1Test.v
Auto1Test_Mem = Auto1.mem
Auto1Test_Out = Auto1Test.vvp

Tools = MemGen

MemGen_Src = Main.hs
MemGen_Out = memgen

# Configuration Dependent Rules

SrcPath = src
DevPath = dev
OutPath = out
TmpPath = out/tmp

all: $(addsuffix -build,$(Projects)) $(addsuffix -build,$(Tools))

clean:
	rm -rf $(OutPath)

define projectBuild
$(1)_SrcFull = $(addprefix $(SrcPath)/,$($(1)_Src))
$(1)_MemFull = $(addprefix $(SrcPath)/,$($(1)_Mem))
$(1)_OutFull = $(addprefix $(OutPath)/,$($(1)_Out))

$(1)-build: $$($(1)_SrcFull) _out $(addsuffix -build,$(Tools))
	$$(info === [Building Project $(1)] ===)
	$(OutPath)/memgen -o $(TmpPath)/$(1).Gen.v $$($(1)_MemFull)
	iverilog -o $$($(1)_OutFull) $(TmpPath)/$(1).Gen.v $$($(1)_SrcFull)
	vvp $$($(1)_OutFull)
endef

define toolBuild
$(1)_SrcFull = $(addprefix $(DevPath)/$(1)/,$($(1)_Src))
$(1)_OutFull = $(addprefix $(OutPath)/,$($(1)_Out))
$(1)_TmpPath = $(TmpPath)/$(1)

$(1)-build: $$($(1)_SrcFull) _out _tmp
	$$(info === [Building Tool $(1)] ===)
	mkdir -p $$($(1)_TmpPath)
	ghc --make -o $$($(1)_OutFull) -odir $$($(1)_TmpPath) -hidir $$($(1)_TmpPath) $$($(1)_SrcFull)
endef

$(foreach project,$(Projects),$(eval $(call projectBuild,$(project))))
$(foreach tool,$(Tools),$(eval $(call toolBuild,$(tool))))

_out:
	mkdir -p $(OutPath)

_tmp: _out
	mkdir -p $(TmpPath)
