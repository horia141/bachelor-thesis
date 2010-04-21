# Configuration Variables

Prj.Auto1.All.Src = Auto1.v Auto1.mem
Prj.Auto1.All.Ref = Alu.All Seq.All Swc.All

Prj.Auto1.Sim.Src = Auto1Sim.v Auto1Sim.sav
Prj.Auto1.Sim.Ref = Auto1.All
Prj.Auto1.Sim.Out = Auto1Sim

Prj.Auto1.FPGA.Src = Auto1FPGA.v Auto1FPGA.ucf
Prj.Auto1.FPGA.Ref = Auto1.All
Prj.Auto1.FPGA.Out = Auto1FPGA

Prj.Alu.All.Src = Alu.v
Prj.Alu.All.Ref =

Prj.Alu.Sim.Src = AluSim.v AluSim.sav
Prj.Alu.Sim.Ref = Alu.All
Prj.Alu.Sim.Out = AluSim

Prj.Seq.All.Src = Seq.v
Prj.Seq.All.Ref =

Prj.Seq.Sim.Src = SeqSim.v AluSim.sav
Prj.Seq.Sim.Ref = Seq.All
Prj.Seq.Sim.Out = SeqSim

Prj.Swc.All.Src = Swc.v
Prj.Swc.All.Ref =

Prj.Swc.Sim.Src = SwcSim.v SwcSim.sav
Prj.Swc.Sim.Ref = Swc.All
Prj.Swc.Sim.Out = SwcSim

Dev.MemGen.Src = MemGen/Main.hs
Dev.MemGen.Ref =
Dev.MemGen.Out = memgen

Dev.LineBR.Src = LineBR/Main.hs
Dev.LineBR.Ref =
Dev.LineBR.Out = linebr

Cfg.Prj.SrcPath = prj
Cfg.Prj.OutPath = out/prj
Cfg.Dev.SrcPath = dev
Cfg.Dev.OutPath = out/dev

# Configuration Dependent Rules

GetAllSrc = $(strip $(foreach ref,$($(1).$(2).Ref),$(call GetAllSrc,$(1),$(ref))) $($(1).$(2).Src))

PrjGetAllSrc = $(addprefix $(Cfg.Prj.SrcPath)/,$(call GetAllSrc,Prj,$(1)))
PrjSimGetProjects = $(foreach srcString,$(filter Prj.%.Sim.Src,$(.VARIABLES)),$(word 2,$(subst ., ,$(srcString))).Sim)
PrjSimProjectToRule = Rule.$(1).Build
PrjSimProjectsToRules = $(foreach project,$(PrjSimGetProjects),$(call PrjSimProjectToRule,$(project)))

define PrjSimBuildE
Prj.$(1).Gen.RuleName       = $(call PrjSimProjectToRule,$(1))
Prj.$(1).Gen.AllSrc         = $(call PrjGetAllSrc,$(1))
Prj.$(1).Gen.VerilogSrc     = $$(filter %.v,$$(Prj.$(1).Gen.AllSrc))
Prj.$(1).Gen.MemGenSrc      = $$(filter %.mem,$$(Prj.$(1).Gen.AllSrc))
Prj.$(1).Gen.SAVSrc         = $$(filter %.sav,$$(Prj.$(1).Gen.AllSrc))
Prj.$(1).Gen.OutPath        = $(Cfg.Prj.OutPath)/$(1)
Prj.$(1).Gen.VVPOutFile     = $$(Prj.$(1).Gen.OutPath)/$(Prj.$(1).Out).vvp
Prj.$(1).Gen.VCDOutFile     = $$(Prj.$(1).Gen.OutPath)/$(Prj.$(1).Out).vcd
Prj.$(1).Gen.TmpPath        = $(Cfg.Prj.OutPath)/$(1)/tmp
Prj.$(1).Gen.MemGenOutFile  = $$(Prj.$(1).Gen.TmpPath)/MemGen.v

$$(Prj.$(1).Gen.RuleName): $$(Prj.$(1).Gen.AllSrc) $$(DevProjectsToRules)
	$$(info === [Building Prj $(1)] ===)
	mkdir -p $$(Prj.$(1).Gen.OutPath)
	mkdir -p $$(Prj.$(1).Gen.TmpPath)
	$$(Dev.MemGen.Gen.OutFile) -o $$(Prj.$(1).Gen.MemGenOutFile) $$(Prj.$(1).Gen.MemGenSrc)
	iverilog -o $$(Prj.$(1).Gen.VVPOutFile) -Wall -Wno-timescale -DVCDFILE=\"$$(Prj.$(1).Gen.VCDOutFile)\" $$(Prj.$(1).Gen.MemGenOutFile) $$(Prj.$(1).Gen.VerilogSrc)
	vvp $$(Prj.$(1).Gen.VVPOutFile)
	cp $$(Prj.$(1).Gen.SAVSrc) $$(Prj.$(1).Gen.OutPath)
endef

define PrjBuildFPGA
endef

DevGetAllSrc = $(addprefix $(Cfg.Dev.SrcPath)/,$(call GetAllSrc,Dev,$(1)))
DevGetProjects = $(foreach srcString,$(filter Dev.%.Src,$(.VARIABLES)),$(word 2,$(subst ., ,$(srcString))))
DevProjectToRule = Rule.$(1).Build
DevProjectsToRules = $(foreach project,$(DevGetProjects),$(call DevProjectToRule,$(project)))

define DevBuildE
Dev.$(1).Gen.RuleName = $(call DevProjectToRule,$(1))
Dev.$(1).Gen.AllSrc   = $(call DevGetAllSrc,$(1))
Dev.$(1).Gen.OutPath  = $(Cfg.Dev.OutPath)/$(1)
Dev.$(1).Gen.OutFile  = $$(Dev.$(1).Gen.OutPath)/$(Dev.$(1).Out)
Dev.$(1).Gen.TmpPath  = $(Cfg.Dev.OutPath)/$(1)/tmp

$$(Dev.$(1).Gen.RuleName): $$(Dev.$(1).Gen.AllSrc)
	$$(info === [Building Dev $(1)] ===)
	mkdir -p $$(Dev.$(1).Gen.OutPath)
	mkdir -p $$(Dev.$(1).Gen.TmpPath)
	ghc --make -o $$(Dev.$(1).Gen.OutFile) -odir $$(Dev.$(1).Gen.TmpPath) -hidir $$(Dev.$(1).Gen.TmpPath) $$(Dev.$(1).Gen.AllSrc)
endef

$(foreach project,$(PrjSimGetProjects),$(eval $(call PrjSimBuildE,$(project))))
$(foreach project,$(DevGetProjects),$(eval $(call DevBuildE,$(project))))

all: $(DevProjectsToRules) $(PrjSimProjectsToRules)
	$(info $^)

clean:
	rm -rf $(Cfg.Prj.OutPath)
	rm -rf $(Cfg.Dev.OutPath)

.PHONY = $(DevProjectsToRules) $(PrjSimProjectsToRules)
.DEFAULT_GOAL = all
