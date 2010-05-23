# Configuration Variables

Prj.Auto2.All.Src = Auto2.v Auto2.mem
Prj.Auto2.All.Ref = Alu.All Seq.All Swc.All LedBank.All VGA.All

Prj.Auto2.Sim.Src = Auto2Sim.v Auto2Sim.sav
Prj.Auto2.Sim.Ref = Auto2.All
Prj.Auto2.Sim.Out = Auto2Sim

Prj.Auto2.FPGA.Src = Auto2FPGA.v Auto2FPGA.ucf
Prj.Auto2.FPGA.Ref = Auto2.All ClockManager.All
Prj.Auto2.FPGA.Out = Auto2FPGA
Prj.Auto2.FPGA.Top = Auto2FPGA

Prj.BlockFall.All.Src = BlockFall.v BlockFall.mem
Prj.BlockFall.All.Ref = Alu.All Seq.All Swc.All VGA.All

Prj.BlockFall.FPGA.Src = BlockFallFPGA.v BlockFallFPGA.ucf
Prj.BlockFall.FPGA.Ref = BlockFall.All ClockManager.All
Prj.BlockFall.FPGA.Out = BlockFallFPGA
Prj.BlockFall.FPGA.Top = BlockFallFPGA

Prj.PressCount.All.Src = PressCount.v PressCount.mem
Prj.PressCount.All.Ref = Seq.All Alu.All PushBtn.All LedBank.All VGA.All

Prj.PressCount.Sim.Src = PressCountSim.v PressCountSim.sav
Prj.PressCount.Sim.Ref = PressCount.All
Prj.PressCount.Sim.Out = PressCountSim

Prj.PressCount.FPGA.Src = PressCountFPGA.v PressCountFPGA.ucf
Prj.PressCount.FPGA.Ref = PressCount.All ClockManager.All
Prj.PressCount.FPGA.Out = PressCountFPGA
Prj.PressCount.FPGA.Top = PressCountFPGA

Prj.RotaryLed.All.Src = RotaryLed.v RotaryLed.mem
Prj.RotaryLed.All.Ref = Seq.All Alu.All Rotary.All LedBank.All

Prj.RotaryLed.Sim.Src = RotaryLedSim.v RotaryLedSim.sav
Prj.RotaryLed.Sim.Ref = RotaryLed.All
Prj.RotaryLed.Sim.Out = RotaryLedSim

Prj.RotaryLed.FPGA.Src = RotaryLedFPGA.v RotaryLedFPGA.ucf
Prj.RotaryLed.FPGA.Ref = RotaryLed.All
Prj.RotaryLed.FPGA.Out = RotaryLedFPGA
Prj.RotaryLed.FPGA.Top = RotaryLedFPGA

Prj.LedBank.All.Src = LedBank.v
Prj.LedBank.All.Ref =

Prj.LedBank.Sim.Src = LedBankSim.v LedBankSim.sav
Prj.LedBank.Sim.Ref = LedBank.All
Prj.LedBank.Sim.Out = LedBankSim

Prj.VGA.All.Src = VGA.v 
Prj.VGA.All.Ref = VGAInterface.All

Prj.PushBtn.All.Src = PushBtn.v
Prj.PushBtn.All.Ref = PushBtnInterface.All

Prj.PushBtn.Sim.Src = PushBtnSim.v PushBtnSim.sav
Prj.PushBtn.Sim.Ref = PushBtn.All
Prj.PushBtn.Sim.Out = PushBtnSim

Prj.Rotary.All.Src = Rotary.v
Prj.Rotary.All.Ref = RotaryInterface.All

Prj.Rotary.Sim.Src = RotarySim.v RotarySim.sav
Prj.Rotary.Sim.Ref = Rotary.All
Prj.Rotary.Sim.Out = RotarySim

Prj.Alu.All.Src = Alu.v
Prj.Alu.All.Ref =

Prj.Alu.Sim.Src = AluSim.v AluSim.sav
Prj.Alu.Sim.Ref = Alu.All
Prj.Alu.Sim.Out = AluSim

Prj.Seq.All.Src = Seq.v
Prj.Seq.All.Ref =

Prj.Seq.Sim.Src = SeqSim.v SeqSim.sav
Prj.Seq.Sim.Ref = Seq.All
Prj.Seq.Sim.Out = SeqSim

Prj.Swc.All.Src = Swc.v
Prj.Swc.All.Ref =

Prj.Swc.Sim.Src = SwcSim.v SwcSim.sav
Prj.Swc.Sim.Ref = Swc.All
Prj.Swc.Sim.Out = SwcSim

Prj.SlowClock.All.Src = SlowClock.v
Prj.SlowClock.All.Ref =

Prj.SlowClock.Sim.Src = SlowClockSim.v SlowClockSim.sav
Prj.SlowClock.Sim.Ref = SlowClock.All
Prj.SlowClock.Sim.Out = SlowClockSim

Prj.SlowClock.FPGA.Src = SlowClockFPGA.v SlowClockFPGA.ucf
Prj.SlowClock.FPGA.Ref = SlowClock.All
Prj.SlowClock.FPGA.Out = SlowClockFPGA
Prj.SlowClock.FPGA.Top = SlowClockFPGA

Prj.ClockManager.All.Src = ClockManager.v
Prj.ClockManager.All.Ref =

Prj.ClockManager.FPGA.Src = ClockManagerFPGA.v ClockManagerFPGA.ucf
Prj.ClockManager.FPGA.Ref = ClockManager.All SlowClock.All
Prj.ClockManager.FPGA.Out = ClockManagerFPGA
Prj.ClockManager.FPGA.Top = ClockManagerFPGA

Prj.VGAInterface.All.Src = VGAInterfaceBad.v
Prj.VGAInterface.All.Ref =

Prj.PushBtnInterface.All.Src = PushBtnInterface.v
Prj.PushBtnInterface.All.Ref =

Prj.PushBtnInterface.Sim.Src = PushBtnInterfaceSim.v PushBtnInterfaceSim.sav
Prj.PushBtnInterface.Sim.Ref = PushBtnInterface.All
Prj.PushBtnInterface.Sim.Out = PushBtnInterfaceSim

Prj.RotaryInterface.All.Src = RotaryInterface.v
Prj.RotaryInterface.All.Ref =

Prj.RotaryInterface.Sim.Src = RotaryInterfaceSim.v RotaryInterfaceSim.sav
Prj.RotaryInterface.Sim.Ref = RotaryInterface.All
Prj.RotaryInterface.Sim.Out = RotaryInterfaceSim

Dev.MemGen.Src = MemGen/Main.hs
Dev.MemGen.Ref =
Dev.MemGen.Out = memgen

Dev.LineBR.Src = LineBR/Main.hs
Dev.LineBR.Ref =
Dev.LineBR.Out = linebr

Cfg.OutPath                                = out
Cfg.RulePath                               = $(Cfg.OutPath)/rule
Cfg.Prj.SrcPath                            = prj
Cfg.Prj.OutPath                            = $(Cfg.OutPath)/prj
Cfg.Prj.Targets.Sim.Tools.MemGen.Invoke    = $(Dev.MemGen.Gen.OutFile)
Cfg.Prj.Targets.Sim.Tools.IVerilog.Invoke  = iverilog
Cfg.Prj.Targets.Sim.Tools.Vvp.Invoke       = vvp
Cfg.Prj.Targets.Sim.Tools.Sav.Invoke       = cp
Cfg.Prj.Targets.FPGA.Part                  = xc3s500e-5fg320
Cfg.Prj.Targets.FPGA.Tools.MemGen.Invoke   = $(Dev.MemGen.Gen.OutFile)
Cfg.Prj.Targets.FPGA.Tools.Xst.Invoke      = xst
Cfg.Prj.Targets.FPGA.Tools.Xst.OptMode     = SPEED
Cfg.Prj.Targets.FPGA.Tools.Xst.OptLevel    = 1
Cfg.Prj.Targets.FPGA.Tools.NgdBuild.Invoke = ngdbuild
Cfg.Prj.Targets.FPGA.Tools.Map.Invoke      = map
Cfg.Prj.Targets.FPGA.Tools.Par.Invoke      = par
Cfg.Prj.Targets.FPGA.Tools.BitGen.Invoke   = bitgen
Cfg.Dev.SrcPath                            = dev
Cfg.Dev.OutPath                            = $(Cfg.OutPath)/dev
Cfg.Dev.Tools.Ghc.Invoke                   = ghc

# Configuration Dependent Rules

LineH1 = $(if $(wildcard $(Dev.LineBR.Gen.OutFile)),$(Dev.LineBR.Gen.OutFile) $(1))
LineH2 = $(if $(wildcard $(Dev.LineBR.Gen.OutFile)),$(Dev.LineBR.Gen.OutFile) -p 3 -c - $(1))
GetAllSrc = $(strip $(foreach ref,$($(1).$(2).Ref),$(call GetAllSrc,$(1),$(ref))) $($(1).$(2).Src))

PrjGetAllSrc = $(addprefix $(Cfg.Prj.SrcPath)/,$(call GetAllSrc,Prj,$(1)))

PrjSimGetProjects = $(foreach srcString,$(filter Prj.%.Sim.Src,$(.VARIABLES)),$(word 2,$(subst ., ,$(srcString))).Sim)
PrjSimProjectToRule = $(Cfg.RulePath)/$(1).Build
PrjSimProjectsToRules = $(foreach project,$(PrjSimGetProjects),$(call PrjSimProjectToRule,$(project)))

define PrjSimBuildE
Prj.$(1).Gen.RuleFile                  = $(call PrjSimProjectToRule,$(1))
Prj.$(1).Gen.Src                       = $(call PrjGetAllSrc,$(1))
Prj.$(1).Gen.BasePath                  = $(Cfg.Prj.OutPath)/$(1)
Prj.$(1).Gen.OutFile                   = $$(Prj.$(1).Gen.BasePath)/$(Prj.$(1).Out).vcd
Prj.$(1).Gen.Tools.MemGen.MemSrc       = $$(filter %.mem,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.MemGen.OutPath      = $$(Prj.$(1).Gen.BasePath)/MemGen
Prj.$(1).Gen.Tools.MemGen.OutFile      = $$(Prj.$(1).Gen.Tools.MemGen.OutPath)/MemGen.v
Prj.$(1).Gen.Tools.IVerilog.VerilogSrc = $$(filter %.v,$$(Prj.$(1).Gen.Src)) $$(Prj.$(1).Gen.Tools.MemGen.OutFile)
Prj.$(1).Gen.Tools.IVerilog.OutPath    = $$(Prj.$(1).Gen.BasePath)/IVerilog
Prj.$(1).Gen.Tools.IVerilog.OutFile    = $$(Prj.$(1).Gen.Tools.IVerilog.OutPath)/$(1).vvp
Prj.$(1).Gen.Tools.Vvp.VvpSrc          = $$(Prj.$(1).Gen.Tools.IVerilog.OutFile)
Prj.$(1).Gen.Tools.Vvp.OutPath         = $$(Prj.$(1).Gen.BasePath)/Vvp
Prj.$(1).Gen.Tools.Sav.SavSrc          = $$(filter %.sav,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.Sav.OutPath         = $$(Prj.$(1).Gen.BasePath)

$$(Prj.$(1).Gen.RuleFile): $$(Prj.$(1).Gen.AllSrc) $$(DevProjectsToRules) _out
	@$$(call LineH1,Building Prj $(1))
	mkdir -p $$(Prj.$(1).Gen.BasePath)
	@$$(call LineH2,Building Prj $(1) : MemGen)
	mkdir -p $$(Prj.$(1).Gen.Tools.MemGen.OutPath)
	$$(Cfg.Prj.Targets.Sim.Tools.MemGen.Invoke) -o $$(Prj.$(1).Gen.Tools.MemGen.OutFile) $$(Prj.$(1).Gen.Tools.MemGen.MemSrc)
	@$$(call LineH2,Building Prj $(1) : IVerilog)
	mkdir -p $$(Prj.$(1).Gen.Tools.IVerilog.OutPath)
	$$(Cfg.Prj.Targets.Sim.Tools.IVerilog.Invoke) -o $$(Prj.$(1).Gen.Tools.IVerilog.OutFile) -Wall -Wno-timescale -DVCDFILE=\"$$(Prj.$(1).Gen.OutFile)\" $$(Prj.$(1).Gen.Tools.IVerilog.VerilogSrc)
	@$$(call LineH2,Building Prj $(2) : Vvp)
	mkdir -p $$(Prj.$(1).Gen.Tools.Vvp.OutPath)
	$$(Cfg.Prj.Targets.Sim.Tools.Vvp.Invoke) $$(Prj.$(1).Gen.Tools.Vvp.VvpSrc)
	@$$(call LineH2,Building Prj $(2) : Sav)
	mkdir -p $$(Prj.$(1).Gen.Tools.Sav.OutPath)
	$$(Cfg.Prj.Targets.Sim.Tools.Sav.Invoke) $$(Prj.$(1).Gen.Tools.Sav.SavSrc) $$(Prj.$(1).Gen.Tools.Sav.OutPath)
	@$$(call LineH2,Building Prj $(1) : Done)
	echo Done > $$(Prj.$(1).Gen.RuleFile)
endef

PrjFPGAGetLibraryName = $(subst .,_,$(1))
PrjFPGAGetProjects = $(foreach srcString,$(filter Prj.%.FPGA.Src,$(.VARIABLES)),$(word 2,$(subst ., ,$(srcString))).FPGA)
PrjFPGAProjectToRule = $(Cfg.RulePath)/$(1).Build
PrjFPGAProjectsToRules = $(foreach project, $(PrjFPGAGetProjects),$(call PrjFPGAProjectToRule,$(project)))

define PrjFPGABuildE
Prj.$(1).Gen.RuleFile                     = $(call PrjFPGAProjectToRule,$(1))
Prj.$(1).Gen.Src                          = $(call PrjGetAllSrc,$(1))
Prj.$(1).Gen.BasePath                     = $$(Cfg.Prj.OutPath)/$(1)
Prj.$(1).Gen.OutFile                      = $$(Prj.$(1).Gen.BasePath)/$$(Prj.$(1).Out)
Prj.$(1).Gen.Tools.MemGen.MemSrc          = $$(filter %.mem,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.MemGen.OutPath         = $$(Prj.$(1).Gen.BasePath)/MemGen
Prj.$(1).Gen.Tools.MemGen.OutFile         = $$(Prj.$(1).Gen.Tools.MemGen.OutPath)/MemGen.v
Prj.$(1).Gen.Tools.Xst.VerilogSrc         = $$(filter %.v,$$(Prj.$(1).Gen.Src)) $$(Prj.$(1).Gen.Tools.MemGen.OutFile)
Prj.$(1).Gen.Tools.Xst.OutPath            = $$(Prj.$(1).Gen.BasePath)/Xst
Prj.$(1).Gen.Tools.Xst.TmpPath            = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/tmp
Prj.$(1).Gen.Tools.Xst.ProjectFile        = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/XstProject.prj
Prj.$(1).Gen.Tools.Xst.LSearchFile        = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/XstLSearch.lso
Prj.$(1).Gen.Tools.Xst.CommandsFile       = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/XstCommands.cmd
Prj.$(1).Gen.Tools.Xst.OutFile            = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/$(1).ngc
Prj.$(1).Gen.Tools.Xst.LogFile            = $$(Prj.$(1).Gen.Tools.Xst.TmpPath)/$(1).log
Prj.$(1).Gen.Tools.Xst.XrptFileMV         = $$(Prj.$(1).Gen.Tools.Xst.OutFile)_xst.xrpt
Prj.$(1).Gen.Tools.NgdBuild.NgcSrc        = $$(Prj.$(1).Gen.Tools.Xst.OutFile)
Prj.$(1).Gen.Tools.NgdBuild.UcfSrc        = $$(filter %.ucf,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.NgdBuild.OutPath       = $$(Prj.$(1).Gen.BasePath)/NgdBuild
Prj.$(1).Gen.Tools.NgdBuild.TmpPath       = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/tmp
Prj.$(1).Gen.Tools.NgdBuild.CommandsFile  = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/NgdBuildCommands.cmd
Prj.$(1).Gen.Tools.NgdBuild.OutFile       = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/$(1).ngd
Prj.$(1).Gen.Tools.NgdBuild.BldFileMV     = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/$(1).bld
Prj.$(1).Gen.Tools.NgdBuild.XrptFileMV    = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/$(1)_ngdbuild.xrpt
Prj.$(1).Gen.Tools.NgdBuild.XbcdFileMV    = xlnx_auto_0_xdb/cst.xbcd
Prj.$(1).Gen.Tools.NgdBuild.XdbPathRM     = xlnx_auto_0_xdb
Prj.$(1).Gen.Tools.Map.NgdSrc             = $$(Prj.$(1).Gen.Tools.NgdBuild.OutFile)
Prj.$(1).Gen.Tools.Map.OutPath            = $$(Prj.$(1).Gen.BasePath)/Map
Prj.$(1).Gen.Tools.Map.TmpPath            = $$(Prj.$(1).Gen.Tools.Map.OutPath)/tmp
Prj.$(1).Gen.Tools.Map.CommandsFile       = $$(Prj.$(1).Gen.Tools.Map.OutPath)/MapCommands.cmd
Prj.$(1).Gen.Tools.Map.NcdOutFile         = $$(Prj.$(1).Gen.Tools.Map.OutPath)/$(1).ncd
Prj.$(1).Gen.Tools.Map.PcfOutFile         = $$(Prj.$(1).Gen.Tools.Map.OutPath)/$(1).pcf
Prj.$(1).Gen.Tools.Map.XrptFileMV         = $$(Prj.$(1).Top)_map.xrpt
Prj.$(1).Gen.Tools.Map.DeviceDetailsMV    = xilinx_device_details.xml
Prj.$(1).Gen.Tools.Map.MapFileMV          = $$(Prj.$(1).Gen.Tools.Map.OutPath)/$(1).map
Prj.$(1).Gen.Tools.Map.MrpFileMV          = $$(Prj.$(1).Gen.Tools.Map.OutPath)/$(1).mrp
Prj.$(1).Gen.Tools.Map.NgmFileMV          = $$(Prj.$(1).Gen.Tools.Map.OutPath)/$(1).ngm
Prj.$(1).Gen.Tools.Par.NcdSrc             = $$(Prj.$(1).Gen.Tools.Map.NcdOutFile)
Prj.$(1).Gen.Tools.Par.PcfSrc             = $$(Prj.$(1).Gen.Tools.Map.PcfOutFile)
Prj.$(1).Gen.Tools.Par.OutPath            = $$(Prj.$(1).Gen.BasePath)/Par
Prj.$(1).Gen.Tools.Par.TmpPath            = $$(Prj.$(1).Gen.Tools.Par.OutPath)/tmp
Prj.$(1).Gen.Tools.Par.CommandsFile       = $$(Prj.$(1).Gen.Tools.Par.OutPath)/ParCommands.cmd
Prj.$(1).Gen.Tools.Par.NcdOutFile         = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1).ncd
Prj.$(1).Gen.Tools.Par.XrptFileMV         = $$(Prj.$(1).Top)_par.xrpt
Prj.$(1).Gen.Tools.Par.TwrFileMV          = smartpreview.twr
Prj.$(1).Gen.Tools.Par.PadFileMV          = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1).pad
Prj.$(1).Gen.Tools.Par.PadCsvFileMV       = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1)_pad.csv
Prj.$(1).Gen.Tools.Par.PadTxtFileMV       = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1)_pad.txt
Prj.$(1).Gen.Tools.Par.ParFileMV          = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1).par
Prj.$(1).Gen.Tools.Par.PtwxFileMV         = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1).ptwx
Prj.$(1).Gen.Tools.Par.UnroutesFileMV     = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1).unroutes
Prj.$(1).Gen.Tools.Par.XpiFileMV          = $$(Prj.$(1).Gen.Tools.Par.OutPath)/$(1).xpi
Prj.$(1).Gen.Tools.BitGen.NcdSrc          = $$(Prj.$(1).Gen.Tools.Par.NcdOutFile)
Prj.$(1).Gen.Tools.BitGen.PcfSrc          = $$(Prj.$(1).Gen.Tools.Map.PcfOutFile)
Prj.$(1).Gen.Tools.BitGen.OutPath         = $$(Prj.$(1).Gen.BasePath)/BitGen
Prj.$(1).Gen.Tools.BitGen.TmpPath         = $$(Prj.$(1).Gen.Tools.BitGen.OutPath)/tmp
Prj.$(1).Gen.Tools.BitGen.CommandsFile    = $$(Prj.$(1).Gen.Tools.BitGen.OutPath)/BitGenCommands.cmd
Prj.$(1).Gen.Tools.BitGen.BitOutFile      = $$(Prj.$(1).Gen.OutFile)
Prj.$(1).Gen.Tools.BitGen.DeviceDetailsMV = xilinx_device_details.xml
Prj.$(1).Gen.Tools.BitGen.BgnFileMV       = $$(Prj.$(1).Gen.OutFile).bgn
Prj.$(1).Gen.Tools.BitGen.DrcFileMV       = $$(Prj.$(1).Gen.OutFile).drc

$$(Prj.$(1).Gen.RuleFile): $$(Prj.$(1).Gen.Src) $$(DevProjectsToRules) _out
	@$$(call LineH1,Building Prj $(1))
	mkdir -p $$(Prj.$(1).Gen.BasePath)
	@$$(call LineH2,Building Prj $(1) : MemGen)
	mkdir -p $$(Prj.$(1).Gen.Tools.MemGen.OutPath)
	$$(Cfg.Prj.Targets.FPGA.Tools.MemGen.Invoke) -o $$(Prj.$(1).Gen.Tools.MemGen.OutFile) $$(Prj.$(1).Gen.Tools.MemGen.MemSrc)
	@$$(call LineH2,Building Prj $(1) : Xst)
	mkdir -p $$(Prj.$(1).Gen.Tools.Xst.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.Xst.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.Xst.ProjectFile)
	rm -rf $$(Prj.$(1).Gen.Tools.Xst.LSearchFile)
	rm -rf $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	$$(foreach fileName,$$(Prj.$(1).Gen.Tools.Xst.VerilogSrc),\
		echo verilog $$(call PrjFPGAGetLibraryName,$(1)) $$(fileName) >> $$(Prj.$(1).Gen.Tools.Xst.ProjectFile);)
	echo $$(call PrjFPGAGetLibraryName,$(1)) >> $$(Prj.$(1).Gen.Tools.Xst.LSearchFile)
	echo set >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -tmpdir $$(Prj.$(1).Gen.Tools.Xst.TmpPath) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -xsthdpdir $$(Prj.$(1).Gen.Tools.Xst.TmpPath) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo run >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -top $(Prj.$(1).Top) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -ifn $$(Prj.$(1).Gen.Tools.Xst.ProjectFile) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -ifmt Mixed >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -ofn $$(Prj.$(1).Gen.Tools.Xst.OutFile) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -ofmt NGC >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -lso $$(Prj.$(1).Gen.Tools.Xst.LSearchFile) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -opt_mode $$(Cfg.Prj.Targets.FPGA.Tools.Xst.OptMode) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -opt_level $$(Cfg.Prj.Targets.FPGA.Tools.Xst.OptLevel) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	echo -p $$(Cfg.Prj.Targets.FPGA.Part) >> $$(Prj.$(1).Gen.Tools.Xst.CommandsFile)
	$$(Cfg.Prj.Targets.FPGA.Tools.Xst.Invoke) -ifn $$(Prj.$(1).Gen.Tools.Xst.CommandsFile) -ofn $$(Prj.$(1).Gen.Tools.Xst.LogFile)
	mv -f $$(Prj.$(1).Gen.Tools.Xst.XrptFileMV) $$(Prj.$(1).Gen.Tools.Xst.TmpPath)
	@$$(call LineH2,Building Prj $(1) : NgdBuild)
	mkdir -p $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)
	echo -p $$(Cfg.Prj.Targets.FPGA.Part) >> $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)
	echo -uc $$(Prj.$(1).Gen.Tools.NgdBuild.UcfSrc) >> $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)
	echo -dd $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath) >> $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)
	$$(Cfg.Prj.Targets.FPGA.Tools.NgdBuild.Invoke) -f $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile) $$(Prj.$(1).Gen.Tools.NgdBuild.NgcSrc) $$(Prj.$(1).Gen.Tools.NgdBuild.OutFile)
	mv -f $$(Prj.$(1).Gen.Tools.NgdBuild.BldFileMV) $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.NgdBuild.XrptFileMV) $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.NgdBuild.XbcdFileMV) $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.NgdBuild.XdbPathRM)
	@$$(call LineH2,Building Prj $(1) : Map)
	mkdir -p $$(Prj.$(1).Gen.Tools.Map.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.Map.CommandsFile)
	echo -detail >> $$(Prj.$(1).Gen.Tools.Map.CommandsFile)
	echo -pr b >> $$(Prj.$(1).Gen.Tools.Map.CommandsFile)
	echo -o $$(Prj.$(1).Gen.Tools.Map.NcdOutFile) >> $$(Prj.$(1).Gen.Tools.Map.CommandsFile)
	$$(Cfg.Prj.Targets.FPGA.Tools.Map.Invoke) -f $$(Prj.$(1).Gen.Tools.Map.CommandsFile) $$(Prj.$(1).Gen.Tools.Map.NgdSrc) $$(Prj.$(1).Gen.Tools.Map.PcfOutFile)
	mv -f $$(Prj.$(1).Gen.Tools.Map.XrptFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.DeviceDetailsMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.MapFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.MrpFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.NgmFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	@$$(call LineH2,Building Prj $(1) : Par)
	mkdir -p $$(Prj.$(1).Gen.Tools.Par.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.Par.CommandsFile)
	echo -w >> $$(Prj.$(1).Gen.Tools.Par.CommandsFile)
	$$(Cfg.Prj.Targets.FPGA.Tools.Par.Invoke) -f $$(Prj.$(1).Gen.Tools.Par.CommandsFile) $$(Prj.$(1).Gen.Tools.Par.NcdSrc) $$(Prj.$(1).Gen.Tools.Par.NcdOutFile) $$(Prj.$(1).Gen.Tools.Par.PcfSrc)
	mv -f $$(Prj.$(1).Gen.Tools.Par.XrptFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.TwrFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PadFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PadCsvFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PadTxtFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.ParFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PtwxFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.UnroutesFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.XpiFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	@$$(call LineH2,Building Prj $(1) : BitGen)
	mkdir -p $$(Prj.$(1).Gen.Tools.BitGen.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)
	echo -w >> $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)
	echo -g StartUpClk:CClk >> $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)
	echo -g CRC:Enable >> $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)
	$$(Cfg.Prj.Targets.FPGA.Tools.BitGen.Invoke) -f $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile) $$(Prj.$(1).Gen.Tools.BitGen.NcdSrc) $$(Prj.$(1).Gen.Tools.BitGen.BitOutFile) $$(Prj.$(1).Gen.Tools.BitGen.PcfSrc)
	mv -f $$(Prj.$(1).Gen.Tools.BitGen.DeviceDetailsMV) $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.BitGen.BgnFileMV) $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.BitGen.DrcFileMV) $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)
	@$$(call LineH2,Building Prj $(1) : Done)
	echo Done > $$(Prj.$(1).Gen.RuleFile)
endef

DevGetAllSrc = $(addprefix $(Cfg.Dev.SrcPath)/,$(call GetAllSrc,Dev,$(1)))
DevGetProjects = $(foreach srcString,$(filter Dev.%.Src,$(.VARIABLES)),$(word 2,$(subst ., ,$(srcString))))
DevProjectToRule = $(Cfg.RulePath)/$(1).Build
DevProjectsToRules = $(foreach project,$(DevGetProjects),$(call DevProjectToRule,$(project)))

define DevBuildE
Dev.$(1).Gen.RuleFile             = $(call DevProjectToRule,$(1))
Dev.$(1).Gen.Src                  = $(call DevGetAllSrc,$(1))
Dev.$(1).Gen.BasePath             = $$(Cfg.Dev.OutPath)/$(1)
Dev.$(1).Gen.OutFile              = $$(Dev.$(1).Gen.BasePath)/$$(Dev.$(1).Out)
Dev.$(1).Gen.Tools.Ghc.HaskellSrc = $$(Dev.$(1).Gen.Src)
Dev.$(1).Gen.Tools.Ghc.OutPath    = $$(Dev.$(1).Gen.BasePath)/Ghc
Dev.$(1).Gen.Tools.Ghc.TmpPath    = $$(Dev.$(1).Gen.Tools.Ghc.OutPath)/tmp
Dev.$(1).Gen.Tools.Ghc.OutFile    = $$(Dev.$(1).Gen.OutFile)

$$(Dev.$(1).Gen.RuleFile): $$(Dev.$(1).Gen.Src) _out
	@$$(call LineH1,Building Dev $(1))
	mkdir -p $$(Dev.$(1).Gen.BasePath)
	@$$(call LineH2,Building Dev $(1) : Ghc)
	mkdir -p $$(Dev.$(1).Gen.Tools.Ghc.OutPath)
	mkdir -p $$(Dev.$(1).Gen.Tools.Ghc.TmpPath)
	$(Cfg.Dev.Tools.Ghc.Invoke) --make -o $$(Dev.$(1).Gen.Tools.Ghc.OutFile) -odir $$(Dev.$(1).Gen.Tools.Ghc.TmpPath) -hidir $$(Dev.$(1).Gen.Tools.Ghc.TmpPath) $$(Dev.$(1).Gen.Tools.Ghc.HaskellSrc)
	@$$(call LineH2,Building Dev $(1) : Done)
	echo Done > $$(Dev.$(1).Gen.RuleFile)
endef

$(foreach project,$(PrjSimGetProjects),$(eval $(call PrjSimBuildE,$(project))))
$(foreach project,$(PrjFPGAGetProjects),$(eval $(call PrjFPGABuildE,$(project))))
$(foreach project,$(DevGetProjects),$(eval $(call DevBuildE,$(project))))

all: sim fpga dev

sim: $(PrjSimProjectsToRules)

fpga: $(PrjFPGAProjectsToRules)

dev: $(DevProjectsToRules)

clean:
	rm -rf $(Cfg.Prj.OutPath)
	rm -rf $(Cfg.Dev.OutPath)
	rm -rf $(Cfg.RulePath)
	rm -rf $(Cfg.OutPath)

_out:
	mkdir -p $(Cfg.OutPath)
	mkdir -p $(Cfg.RulePath)

.PHONY = all
.DEFAULT_GOAL = all
