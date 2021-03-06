# Configuration Variables

# Projects configuration zone.

# Projects in prj

# Projects in prj/applications

# Auto2

Prj.Auto2.All.Base = applications/Auto2
Prj.Auto2.All.Src  = Auto2.v Auto2.seq Auto2.dev
Prj.Auto2.All.Ref  = Alu.All Seq.All Swc.All LedBank.All VGA1.All

Prj.Auto2.Sim.Base = $(Prj.Auto2.All.Base)
Prj.Auto2.Sim.Src  = Auto2Sim.v Auto2Sim.sav
Prj.Auto2.Sim.Ref  = Auto2.All
Prj.Auto2.Sim.Out  = Auto2Sim
Prj.Auto2.Sim.Top  = Auto2Sim

Prj.Auto2.FPGA.Base = $(Prj.Auto2.All.Base)
Prj.Auto2.FPGA.Src  = Auto2FPGA.v Auto2FPGA.ucf
Prj.Auto2.FPGA.Ref  = Auto2.All ClockManager.All
Prj.Auto2.FPGA.Out  = Auto2FPGA
Prj.Auto2.FPGA.Top  = Auto2FPGA

# BigSDRAM

Prj.BigSDRAM.All.Base = applications/BigSDRAM
Prj.BigSDRAM.All.Src  = BigSDRAM.v BigSDRAM.seq BigSDRAM.dev
Prj.BigSDRAM.All.Ref  = Seq.All Swc.All LedBank.All DdrCtl1.All

Prj.BigSDRAM.Sim.Base = $(Prj.BigSDRAM.All.Base)
Prj.BigSDRAM.Sim.Src  = BigSDRAMSim.v BigSDRAMSim.sav
Prj.BigSDRAM.Sim.Ref  = BigSDRAM.All MicronDDRSim.All
Prj.BigSDRAM.Sim.Out  = BigSDRAMSim
Prj.BigSDRAM.Sim.Top  = BigSDRAMSim

Prj.BigSDRAM.FPGA.Base = $(Prj.BigSDRAM.All.Base)
Prj.BigSDRAM.FPGA.Src  = BigSDRAMFPGA.v BigSDRAMFPGA.ucf
Prj.BigSDRAM.FPGA.Ref  = BigSDRAM.All ClockManager.All
Prj.BigSDRAM.FPGA.Out  = BigSDRAMFPGA
Prj.BigSDRAM.FPGA.Top  = BigSDRAMFPGA

# BlockFall

Prj.BlockFall.All.Base = applications/BlockFall
Prj.BlockFall.All.Src  = BlockFall.v BlockFall.seq BlockFall.dev
Prj.BlockFall.All.Ref  = Alu.All Seq.All Swc.All VGA1.All

Prj.BlockFall.FPGA.Base = $(Prj.BlockFall.All.Base)
Prj.BlockFall.FPGA.Src  = BlockFallFPGA.v BlockFallFPGA.ucf
Prj.BlockFall.FPGA.Ref  = BlockFall.All ClockManager.All
Prj.BlockFall.FPGA.Out  = BlockFallFPGA
Prj.BlockFall.FPGA.Top  = BlockFallFPGA

# PressCount

Prj.PressCount.All.Base = applications/PressCount
Prj.PressCount.All.Src  = PressCount.v PressCount.seq PressCount.dev
Prj.PressCount.All.Ref  = Seq.All Alu.All PushBtn.All LedBank.All VGA1.All

Prj.PressCount.Sim.Base = $(Prj.PressCount.All.Base)
Prj.PressCount.Sim.Src  = PressCountSim.v PressCountSim.sav
Prj.PressCount.Sim.Ref  = PressCount.All
Prj.PressCount.Sim.Out  = PressCountSim
Prj.PressCount.Sim.Top  = PressCountSim

Prj.PressCount.FPGA.Base = $(Prj.PressCount.All.Base)
Prj.PressCount.FPGA.Src  = PressCountFPGA.v PressCountFPGA.ucf
Prj.PressCount.FPGA.Ref  = PressCount.All ClockManager.All
Prj.PressCount.FPGA.Out  = PressCountFPGA
Prj.PressCount.FPGA.Top  = PressCountFPGA

# RedRectangle

Prj.RedRectangle.All.Base = applications/RedRectangle
Prj.RedRectangle.All.Src  = RedRectangle.v RedRectangle.seq RedRectangle.dev
Prj.RedRectangle.All.Ref  = Seq.All Alu.All RegBankP4.All VGA2.All

Prj.RedRectangle.Sim.Base = $(Prj.RedRectangle.All.Base)
Prj.RedRectangle.Sim.Src  = RedRectangleSim.v RedRectangleSim.sav
Prj.RedRectangle.Sim.Ref  = RedRectangle.All
Prj.RedRectangle.Sim.Out  = RedRectangleSim
Prj.RedRectangle.Sim.Top  = RedRectangleSim

Prj.RedRectangle.FPGA.Base = $(Prj.RedRectangle.All.Base)
Prj.RedRectangle.FPGA.Src  = RedRectangleFPGA.v RedRectangleFPGA.ucf
Prj.RedRectangle.FPGA.Ref  = RedRectangle.All ClockManager.All
Prj.RedRectangle.FPGA.Out  = RedRectangleFPGA
Prj.RedRectangle.FPGA.Top  = RedRectangleFPGA

# RotaryLed

Prj.RotaryLed.All.Base = applications/RotaryLed
Prj.RotaryLed.All.Src  = RotaryLed.v RotaryLed.seq RotaryLed.dev
Prj.RotaryLed.All.Ref  = Seq.All Alu.All Rotary.All LedBank.All VGA1.All

Prj.RotaryLed.Sim.Base = $(Prj.RotaryLed.All.Base)
Prj.RotaryLed.Sim.Src  = RotaryLedSim.v RotaryLedSim.sav
Prj.RotaryLed.Sim.Ref  = RotaryLed.All
Prj.RotaryLed.Sim.Out  = RotaryLedSim
Prj.RotaryLed.Sim.Top  = RotaryLedSim

Prj.RotaryLed.FPGA.Base = $(Prj.RotaryLed.All.Base)
Prj.RotaryLed.FPGA.Src  = RotaryLedFPGA.v RotaryLedFPGA.ucf
Prj.RotaryLed.FPGA.Ref  = RotaryLed.All ClockManager.All
Prj.RotaryLed.FPGA.Out  = RotaryLedFPGA
Prj.RotaryLed.FPGA.Top  = RotaryLedFPGA

# ViewImage

Prj.ViewImage.All.Base = applications/ViewImage
Prj.ViewImage.All.Src  = ViewImage.v ViewImage.seq ViewImage.dev ViewImageImgROM.mem
Prj.ViewImage.All.Ref  = Seq.All Alu.All RegBankS8.All ROMMatrix.All VGA2.All

Prj.ViewImage.Sim.Base = $(Prj.ViewImage.All.Base)
Prj.ViewImage.Sim.Src  = ViewImageSim.v ViewImageSim.sav
Prj.ViewImage.Sim.Ref  = ViewImage.All
Prj.ViewImage.Sim.Out  = ViewImageSim
Prj.ViewImage.Sim.Top  = ViewImageSim

Prj.ViewImage.FPGA.Base = $(Prj.ViewImage.All.Base)
Prj.ViewImage.FPGA.Src  = ViewImageFPGA.v ViewImageFPGA.ucf
Prj.ViewImage.FPGA.Ref  = ViewImage.All ClockManager.All
Prj.ViewImage.FPGA.Out  = ViewImageFPGA
Prj.ViewImage.FPGA.Top  = ViewImageFPGA

# Projects in prj/components

# Alu

Prj.Alu.All.Base = components/Alu
Prj.Alu.All.Src  = Alu.v
Prj.Alu.All.Ref  =

Prj.Alu.Sim.Base = $(Prj.Alu.All.Base)
Prj.Alu.Sim.Src  = AluSim.v AluSim.sav
Prj.Alu.Sim.Ref  = Alu.All
Prj.Alu.Sim.Out  = AluSim
Prj.Alu.Sim.Top  = AluSim

# DdrCtl1

Prj.DdrCtl1.All.Base = components/DdrCtl1
Prj.DdrCtl1.All.Src  = DdrCtl1.v
Prj.DdrCtl1.All.Ref  = 

Prj.DdrCtl1.Sim.Base = $(Prj.DdrCtl1.All.Base)
Prj.DdrCtl1.Sim.Src  = DdrCtl1Sim.v DdrCtl1Sim.sav
Prj.DdrCtl1.Sim.Ref  = DdrCtl1.All MicronDDRSim.All
Prj.DdrCtl1.Sim.Out  = DdrCtl1Sim
Prj.DdrCtl1.Sim.Top  = DdrCtl1Sim

# LedBank

Prj.LedBank.All.Base = components/LedBank
Prj.LedBank.All.Src  = LedBank.v
Prj.LedBank.All.Ref  =

Prj.LedBank.Sim.Base = $(Prj.LedBank.All.Base)
Prj.LedBank.Sim.Src  = LedBankSim.v LedBankSim.sav
Prj.LedBank.Sim.Ref  = LedBank.All
Prj.LedBank.Sim.Out  = LedBankSim
Prj.LedBank.Sim.Top  = LedBankSim

# PushBtn

Prj.PushBtn.All.Base = components/PushBtn
Prj.PushBtn.All.Src  = PushBtn.v
Prj.PushBtn.All.Ref  = PushBtnInterface.All

Prj.PushBtn.Sim.Base = $(Prj.PushBtn.All.Base)
Prj.PushBtn.Sim.Src  = PushBtnSim.v PushBtnSim.sav
Prj.PushBtn.Sim.Ref  = PushBtn.All
Prj.PushBtn.Sim.Out  = PushBtnSim
Prj.PushBtn.Sim.Top  = PushBtnSim

Prj.PushBtnInterface.All.Base = $(Prj.PushBtn.All.Base)
Prj.PushBtnInterface.All.Src  = PushBtnInterface.v
Prj.PushBtnInterface.All.Ref  =

Prj.PushBtnInterface.Sim.Base = $(Prj.PushBtn.All.Base)
Prj.PushBtnInterface.Sim.Src  = PushBtnInterfaceSim.v PushBtnInterfaceSim.sav
Prj.PushBtnInterface.Sim.Ref  = PushBtnInterface.All
Prj.PushBtnInterface.Sim.Out  = PushBtnInterfaceSim
Prj.PushBtnInterface.Sim.Top  = PushBtnInterfaceSim

# RegBank

Prj.RegBankP2.All.Base = components/RegBank
Prj.RegBankP2.All.Src  = RegBankP2.v
Prj.RegBankP2.All.Ref  =

Prj.RegBankP2.Sim.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankP2.Sim.Src  = RegBankP2Sim.v RegBankP2Sim.sav
Prj.RegBankP2.Sim.Ref  = RegBankP2.All
Prj.RegBankP2.Sim.Out  = RegBankP2Sim
Prj.RegBankP2.Sim.Top  = RegBankP2Sim

Prj.RegBankP4.All.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankP4.All.Src  = RegBankP4.v
Prj.RegBankP4.All.Ref  =

Prj.RegBankP4.Sim.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankP4.Sim.Src  = RegBankP4Sim.v RegBankP4Sim.sav
Prj.RegBankP4.Sim.Ref  = RegBankP4.All
Prj.RegBankP4.Sim.Out  = RegBankP4Sim
Prj.RegBankP4.Sim.Top  = RegBankP4Sim

Prj.RegBankP8.All.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankP8.All.Src  = RegBankP8.v
Prj.RegBankP8.All.Ref  =

Prj.RegBankP8.Sim.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankP8.Sim.Src  = RegBankP8Sim.v RegBankP8Sim.sav
Prj.RegBankP8.Sim.Ref  = RegBankP8.All
Prj.RegBankP8.Sim.Out  = RegBankP8Sim
Prj.RegBankP8.Sim.Top  = RegBankP8Sim

Prj.RegBankS2.All.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankS2.All.Src  = RegBankS2.v
Prj.RegBankS2.All.Ref  =

Prj.RegBankS2.Sim.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankS2.Sim.Src  = RegBankS2Sim.v RegBankS2Sim.sav
Prj.RegBankS2.Sim.Ref  = RegBankS2.All
Prj.RegBankS2.Sim.Out  = RegBankS2Sim
Prj.RegBankS2.Sim.Top  = RegBankS2Sim

Prj.RegBankS4.All.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankS4.All.Src  = RegBankS4.v
Prj.RegBankS4.All.Ref  =

Prj.RegBankS4.Sim.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankS4.Sim.Src  = RegBankS4Sim.v RegBankS4Sim.sav
Prj.RegBankS4.Sim.Ref  = RegBankS4.All
Prj.RegBankS4.Sim.Out  = RegBankS4Sim
Prj.RegBankS4.Sim.Top  = RegBankS4Sim

Prj.RegBankS8.All.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankS8.All.Src  = RegBankS8.v
Prj.RegBankS8.All.Ref  =

Prj.RegBankS8.Sim.Base = $(Prj.RegBankP2.All.Base)
Prj.RegBankS8.Sim.Src  = RegBankS8Sim.v RegBankS8Sim.sav
Prj.RegBankS8.Sim.Ref  = RegBankS8.All
Prj.RegBankS8.Sim.Out  = RegBankS8Sim
Prj.RegBankS8.Sim.Top  = RegBankS8Sim

# ROMMatrix

Prj.ROMMatrix.All.Base = components/ROMMatrix
Prj.ROMMatrix.All.Src  = ROMMatrix.v
Prj.ROMMatrix.All.Ref  =

Prj.ROMMatrix.Sim.Base = $(Prj.ROMMatrix.All.Base)
Prj.ROMMatrix.Sim.Src  = ROMMatrixSim.v ROMMatrixSim.sav ROMMatrixSim.mem
Prj.ROMMatrix.Sim.Ref  = ROMMatrix.All
Prj.ROMMatrix.Sim.Out  = ROMMatrixSim
Prj.ROMMatrix.Sim.Top  = ROMMatrixSim

# Rotary

Prj.Rotary.All.Base = components/Rotary
Prj.Rotary.All.Src  = Rotary.v
Prj.Rotary.All.Ref  = RotaryInterface.All

Prj.Rotary.Sim.Base = $(Prj.Rotary.All.Base)
Prj.Rotary.Sim.Src  = RotarySim.v RotarySim.sav
Prj.Rotary.Sim.Ref  = Rotary.All
Prj.Rotary.Sim.Out  = RotarySim
Prj.Rotary.Sim.Top  = RotarySim

Prj.RotaryInterface.All.Base = $(Prj.Rotary.All.Base)
Prj.RotaryInterface.All.Src  = RotaryInterface.v
Prj.RotaryInterface.All.Ref  =

Prj.RotaryInterface.Sim.Base = $(Prj.Rotary.All.Base)
Prj.RotaryInterface.Sim.Src  = RotaryInterfaceSim.v RotaryInterfaceSim.sav
Prj.RotaryInterface.Sim.Ref  = RotaryInterface.All
Prj.RotaryInterface.Sim.Out  = RotaryInterfaceSim
Prj.RotaryInterface.Sim.Top  = RotaryInterfaceSim

# Swc

Prj.Swc.All.Base = components/Swc
Prj.Swc.All.Src  = Swc.v
Prj.Swc.All.Ref  =

Prj.Swc.Sim.Base = $(Prj.Swc.All.Base)
Prj.Swc.Sim.Src  = SwcSim.v SwcSim.sav
Prj.Swc.Sim.Ref  = Swc.All
Prj.Swc.Sim.Out  = SwcSim
Prj.Swc.Sim.Top  = SwcSim

# VGA1

Prj.VGA1.All.Base = components/VGA1
Prj.VGA1.All.Src  = VGA1.v 
Prj.VGA1.All.Ref  = VGA1Interface.All

Prj.VGA1Interface.All.Base = $(Prj.VGA1.All.Base)
Prj.VGA1Interface.All.Src  = VGA1Interface.v
Prj.VGA1Interface.All.Ref  =

# VGA2

Prj.VGA2.All.Base = components/VGA2
Prj.VGA2.All.Src  = VGA2.v
Prj.VGA2.All.Ref  = VGA2Interface.All RAMDP.All

Prj.VGA2.Sim.Base = $(Prj.VGA2.All.Base)
Prj.VGA2.Sim.Src  = VGA2Sim.v VGA2Sim.sav
Prj.VGA2.Sim.Ref  = VGA2.All
Prj.VGA2.Sim.Out  = VGA2Sim
Prj.VGA2.Sim.Top  = VGA2Sim
Prj.VGA2.Sim.Def.SIM_FullState =

Prj.VGA2Interface.All.Base = $(Prj.VGA2.All.Base)
Prj.VGA2Interface.All.Src  = VGA2Interface.v
Prj.VGA2Interface.All.Ref  =

Prj.VGA2Interface.Sim.Base = $(Prj.VGA2.All.Base)
Prj.VGA2Interface.Sim.Src  = VGA2InterfaceSim.v VGA2InterfaceSim.sav
Prj.VGA2Interface.Sim.Ref  = VGA2Interface.All
Prj.VGA2Interface.Sim.Out  = VGA2InterfaceSim
Prj.VGA2Interface.Sim.Top  = VGA2InterfaceSim

Prj.VGA2Interface.FPGA.Base = $(Prj.VGA2.All.Base)
Prj.VGA2Interface.FPGA.Src  = VGA2InterfaceFPGA.v VGA2InterfaceFPGA.ucf
Prj.VGA2Interface.FPGA.Ref  = VGA2Interface.All
Prj.VGA2Interface.FPGA.Out  = VGA2InterfaceFPGA
Prj.VGA2Interface.FPGA.Top  = VGA2InterfaceFPGA

# Projects in prj/sequencers

Prj.Seq.All.Base = sequencers/Seq
Prj.Seq.All.Src  = Seq.v
Prj.Seq.All.Ref  =

Prj.Seq.Sim.Base = $(Prj.Seq.All.Base)
Prj.Seq.Sim.Src  = SeqSim.v SeqSim.sav
Prj.Seq.Sim.Ref  = Seq.All
Prj.Seq.Sim.Out  = SeqSim
Prj.Seq.Sim.Top  = SeqSim

# Projects in prj/xtra

# Projects in prj/xtra/common

# ClockManager

Prj.ClockManager.All.Base = xtra/common/ClockManager
Prj.ClockManager.All.Src  = ClockManager.v
Prj.ClockManager.All.Ref  =

Prj.ClockManager.FPGA.Base = $(Prj.ClockManager.All.Base)
Prj.ClockManager.FPGA.Src  = ClockManagerFPGA.v ClockManagerFPGA.ucf
Prj.ClockManager.FPGA.Ref  = ClockManager.All SlowClock.All
Prj.ClockManager.FPGA.Out  = ClockManagerFPGA
Prj.ClockManager.FPGA.Top  = ClockManagerFPGA

# RAMDP

Prj.RAMDP.All.Base = xtra/common/RAMDP
Prj.RAMDP.All.Src  = RAMDP.v
Prj.RAMDP.All.Ref  =

Prj.RAMDP.Sim.Base = $(Prj.RAMDP.All.Base)
Prj.RAMDP.Sim.Src  = RAMDPSim.v RAMDPSim.sav
Prj.RAMDP.Sim.Ref  = RAMDP.All
Prj.RAMDP.Sim.Out  = RAMDPSim
Prj.RAMDP.Sim.Top  = RAMDPSim

# RAMSP

Prj.RAMSP.All.Base = xtra/common/RAMSP
Prj.RAMSP.All.Src  = RAMSP.v
Prj.RAMSP.All.Ref  =

Prj.RAMSP.Sim.Base = $(Prj.RAMSP.All.Base)
Prj.RAMSP.Sim.Src  = RAMSPSim.v RAMSPSim.sav
Prj.RAMSP.Sim.Ref  = RAMSP.All
Prj.RAMSP.Sim.Out  = RAMSPSim
Prj.RAMSP.Sim.Top  = RAMSPSim

# SlowClock

Prj.SlowClock.All.Base = xtra/common/SlowClock
Prj.SlowClock.All.Src  = SlowClock.v
Prj.SlowClock.All.Ref  =

Prj.SlowClock.Sim.Base = $(Prj.SlowClock.All.Base)
Prj.SlowClock.Sim.Src  = SlowClockSim.v SlowClockSim.sav
Prj.SlowClock.Sim.Ref  = SlowClock.All
Prj.SlowClock.Sim.Out  = SlowClockSim
Prj.SlowClock.Sim.Top  = SlowClockSim

Prj.SlowClock.FPGA.Base = $(Prj.SlowClock.All.Base)
Prj.SlowClock.FPGA.Src  = SlowClockFPGA.v SlowClockFPGA.ucf
Prj.SlowClock.FPGA.Ref  = SlowClock.All
Prj.SlowClock.FPGA.Out  = SlowClockFPGA
Prj.SlowClock.FPGA.Top  = SlowClockFPGA

# Projects in prj/xtra/extern

# MicronDDRSim

Prj.MicronDDRSim.All.Base = xtra/extern/MicronDDRSim
Prj.MicronDDRSim.All.Src  = Ddr.v
Prj.MicronDDRSim.All.Ref  =

# Projects in dev

# LineBR

Dev.LineBR.Base = LineBR
Dev.LineBR.Src  = Main.hs
Dev.LineBR.Ref  =
Dev.LineBR.Out  = linebr

# MemGen

Dev.MemGen.Base = MemGen
Dev.MemGen.Src  = Main.hs
Dev.MemGen.Ref  =
Dev.MemGen.Out  = memgen

# SeqAsm

Dev.SeqAsm.Base = SeqAsm
Dev.SeqAsm.Src  = Main.hs Core.hs Utils.hs Configs.hs Compiler.hs
Dev.SeqAsm.Ref  =
Dev.SeqAsm.Out  = seqasm

# End of projects configuration zone.

Cfg.OutPath                                      = out
Cfg.RulePath                                     = $(Cfg.OutPath)/rule
Cfg.Prj.SrcPath                                  = prj
Cfg.Prj.OutPath                                  = $(Cfg.OutPath)/prj
Cfg.Prj.Targets.Sim.Tools.SeqAsm.Invoke          = $(Dev.SeqAsm.Gen.OutFile)
Cfg.Prj.Targets.Sim.Tools.SeqAsm.SequencersFile  = $(Cfg.Prj.SrcPath)/sequencers/Sequencers.cfg
Cfg.Prj.Targets.Sim.Tools.SeqAsm.ComponentsFile  = $(Cfg.Prj.SrcPath)/components/Components.cfg
Cfg.Prj.Targets.Sim.Tools.MemGen.Invoke          = $(Dev.MemGen.Gen.OutFile)
Cfg.Prj.Targets.Sim.Tools.IVerilog.Invoke        = iverilog
Cfg.Prj.Targets.Sim.Tools.Vvp.Invoke             = vvp
Cfg.Prj.Targets.Sim.Tools.Sav.Invoke             = cp
Cfg.Prj.Targets.FPGA.Part                        = xc3s500e-5fg320
Cfg.Prj.Targets.FPGA.Tools.SeqAsm.Invoke         = $(Dev.SeqAsm.Gen.OutFile)
Cfg.Prj.Targets.FPGA.Tools.SeqAsm.SequencersFile = $(Cfg.Prj.SrcPath)/sequencers/Sequencers.cfg
Cfg.Prj.Targets.FPGA.Tools.SeqAsm.ComponentsFile = $(Cfg.Prj.SrcPath)/components/Components.cfg
Cfg.Prj.Targets.FPGA.Tools.MemGen.Invoke         = $(Dev.MemGen.Gen.OutFile)
Cfg.Prj.Targets.FPGA.Tools.IVerilog.Invoke       = iverilog
Cfg.Prj.Targets.FPGA.Tools.Xst.Invoke            = xst
Cfg.Prj.Targets.FPGA.Tools.Xst.OptMode           = SPEED
Cfg.Prj.Targets.FPGA.Tools.Xst.OptLevel          = 1
Cfg.Prj.Targets.FPGA.Tools.NgdBuild.Invoke       = ngdbuild
Cfg.Prj.Targets.FPGA.Tools.Map.Invoke            = map
Cfg.Prj.Targets.FPGA.Tools.Par.Invoke            = par
Cfg.Prj.Targets.FPGA.Tools.BitGen.Invoke         = bitgen
Cfg.Dev.SrcPath                                  = dev
Cfg.Dev.OutPath                                  = $(Cfg.OutPath)/dev
Cfg.Dev.Tools.Ghc.Invoke                         = ghc

# Configuration Dependent Rules

LineH1 = $(if $(wildcard $(Dev.LineBR.Gen.OutFile)),$(Dev.LineBR.Gen.OutFile) $(1))
LineH2 = $(if $(wildcard $(Dev.LineBR.Gen.OutFile)),$(Dev.LineBR.Gen.OutFile) -p 3 -c - $(1))
GetAllSrc = $(strip $(foreach ref,$($(1).$(2).Ref),$(call GetAllSrc,$(1),$(ref))) $(addprefix $($(1).$(2).Base)/,$($(1).$(2).Src)))

PrjGetAllSrc = $(addprefix $(Cfg.Prj.SrcPath)/,$(call GetAllSrc,Prj,$(1)))

PrjSimGetProjects = $(foreach srcString,$(filter Prj.%.Sim.Src,$(.VARIABLES)),$(word 2,$(subst ., ,$(srcString))).Sim)
PrjSimProjectToRule = $(Cfg.RulePath)/$(1).Build
PrjSimProjectsToRules = $(foreach project,$(PrjSimGetProjects),$(call PrjSimProjectToRule,$(project)))

define PrjSimBuildE
Prj.$(1).Gen.RuleFile                  = $(call PrjSimProjectToRule,$(1))
Prj.$(1).Gen.Src                       = $(call PrjGetAllSrc,$(1))
Prj.$(1).Gen.BasePath                  = $(Cfg.Prj.OutPath)/$(1)
Prj.$(1).Gen.VcdOutFile                = $$(Prj.$(1).Gen.BasePath)/$(Prj.$(1).Out).vcd
Prj.$(1).Gen.SavOutFile                = $$(Prj.$(1).Gen.BasePath)/$(Prj.$(1).Out).sav
Prj.$(1).Gen.Tools.SeqAsm.SeqSrc       = $$(filter %.seq,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.SeqAsm.DevSrc       = $$(filter %.dev,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.SeqAsm.OutPath      = $$(Prj.$(1).Gen.BasePath)/SeqAsm
Prj.$(1).Gen.Tools.SeqAsm.OutFile      = $$(Prj.$(1).Gen.Tools.SeqAsm.OutPath)/SeqAsm.mem
Prj.$(1).Gen.Tools.SeqAsm.OutTextFile  = $$(Prj.$(1).Gen.Tools.SeqAsm.OutPath)/SeqAsm.text.mem
Prj.$(1).Gen.Tools.MemGen.MemSrc       = $$(filter %.mem,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.MemGen.OutPath      = $$(Prj.$(1).Gen.BasePath)/MemGen
Prj.$(1).Gen.Tools.MemGen.OutFile      = $$(Prj.$(1).Gen.Tools.MemGen.OutPath)/MemGen.v
Prj.$(1).Gen.Tools.IVerilog.VerilogSrc = $$(filter %.v,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.IVerilog.OutPath    = $$(Prj.$(1).Gen.BasePath)/IVerilog
Prj.$(1).Gen.Tools.IVerilog.OutFile    = $$(Prj.$(1).Gen.Tools.IVerilog.OutPath)/IVerilog.vvp
Prj.$(1).Gen.Tools.Vvp.VvpSrc          = $$(Prj.$(1).Gen.Tools.IVerilog.OutFile)
Prj.$(1).Gen.Tools.Vvp.OutPath         = $$(Prj.$(1).Gen.BasePath)/Vvp
Prj.$(1).Gen.Tools.Vvp.OutFile         = $$(Prj.$(1).Gen.Tools.Vvp.OutPath)/Vvp.vcd
Prj.$(1).Gen.Tools.Sav.SavSrc          = $$(filter %.sav,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.Sav.OutPath         = $$(Prj.$(1).Gen.BasePath)/Sav
Prj.$(1).Gen.Tools.Sav.OutFile         = $$(Prj.$(1).Gen.Tools.Sav.OutPath)/Sav.sav

$$(Prj.$(1).Gen.RuleFile): $$(Prj.$(1).Gen.AllSrc) $$(DevProjectsToRules) _out
#	Start building the project.
	@$$(call LineH1,Building Prj $(1))
	mkdir -p $$(Prj.$(1).Gen.BasePath)

#	Compile any .seq files, and produce .mem files for MemGen.
	@$$(call LineH2,Building Prj $(1) : SeqAsm)
	mkdir -p $$(Prj.$(1).Gen.Tools.SeqAsm.OutPath)
	$$(if $$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc),\
		$$(Cfg.Prj.Targets.Sim.Tools.SeqAsm.Invoke) \
			-o $$(Prj.$(1).Gen.Tools.SeqAsm.OutFile) \
			-t $$(Prj.$(1).Gen.Tools.SeqAsm.OutTextFile) \
			-s $$(Cfg.Prj.Targets.Sim.Tools.SeqAsm.SequencersFile) \
			-c $$(Cfg.Prj.Targets.Sim.Tools.SeqAsm.ComponentsFile) \
			-d $$(Prj.$(1).Gen.Tools.SeqAsm.DevSrc) \
			$$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc))

#	Compile any .mem files, and produce a MemGen.v file for IVerilog.
	@$$(call LineH2,Building Prj $(1) : MemGen)
	mkdir -p $$(Prj.$(1).Gen.Tools.MemGen.OutPath)
	$$(if $$(and $$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc),\
		     $$(Prj.$(1).Gen.Tools.MemGen.MemSrc)),\
		$$(Cfg.Prj.Targets.Sim.Tools.MemGen.Invoke) \
			-o $$(Prj.$(1).Gen.Tools.MemGen.OutFile) \
			$$(Prj.$(1).Gen.Tools.MemGen.MemSrc) \
			$$(Prj.$(1).Gen.Tools.SeqAsm.OutFile) \
			$$(Prj.$(1).Gen.Tools.SeqAsm.OutTextFile),\
		$$(if $$(Prj.$(1).Gen.Tools.MemGen.MemSrc),\
			$$(Cfg.Prj.Targets.Sim.Tools.MemGen.Invoke) \
				-o $$(Prj.$(1).Gen.Tools.MemGen.OutFile) \
				$$(Prj.$(1).Gen.Tools.MemGen.MemSrc)))

#	Compile all .v files, producing a .vvp file for Vvp.
	@$$(call LineH2,Building Prj $(1) : IVerilog)
	mkdir -p $$(Prj.$(1).Gen.Tools.IVerilog.OutPath)
	$$(Cfg.Prj.Targets.Sim.Tools.IVerilog.Invoke) \
		-o $$(Prj.$(1).Gen.Tools.IVerilog.OutFile) \
		-s $(Prj.$(1).Top) \
		-Wall -Wno-timescale \
		-DVCDFILE=\"$$(Prj.$(1).Gen.Tools.Vvp.OutFile)\" \
		-DSIM \
		$$(foreach simDef,$$(filter Prj.$(1).Def.%,$$(.VARIABLES)),\
			$$(if $$(value $$(simDef)), \
				-D$$(word 5,$$(subst ., ,$$(simDef)))=$$(value $$(simDef)), \
				-D$$(word 5,$$(subst ., ,$$(simDef))))) \
		$$(Prj.$(1).Gen.Tools.IVerilog.VerilogSrc) \
		$$(if $$(or $$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc),\
			    $$(Prj.$(1).Gen.Tools.MemGen.MemSrc)),\
			$$(Prj.$(1).Gen.Tools.MemGen.OutFile))

#	Execute vvp and produce simulation outputs.
	@$$(call LineH2,Building Prj $(2) : Vvp)
	mkdir -p $$(Prj.$(1).Gen.Tools.Vvp.OutPath)
	$$(Cfg.Prj.Targets.Sim.Tools.Vvp.Invoke) $$(Prj.$(1).Gen.Tools.Vvp.VvpSrc)

#	Copy the .sav files that describe the gtkwave view to output directory.
	@$$(call LineH2,Building Prj $(2) : Sav)
	mkdir -p $$(Prj.$(1).Gen.Tools.Sav.OutPath)
	$$(Cfg.Prj.Targets.Sim.Tools.Sav.Invoke) $$(Prj.$(1).Gen.Tools.Sav.SavSrc) $$(Prj.$(1).Gen.Tools.Sav.OutFile)

#	It's all done. Copy .vcd and .sav files to the root of the project output directory.
	@$$(call LineH2,Building Prj $(1) : Done)
	echo Done > $$(Prj.$(1).Gen.RuleFile)
	cp $$(Prj.$(1).Gen.Tools.Vvp.OutFile) $$(Prj.$(1).Gen.VcdOutFile)
	cp $$(Prj.$(1).Gen.Tools.Sav.OutFile) $$(Prj.$(1).Gen.SavOutFile)
endef

PrjFPGAGetLibraryName = $(subst .,_,$(1))
PrjFPGAGetProjects = $(foreach srcString,$(filter Prj.%.FPGA.Src,$(.VARIABLES)),$(word 2,$(subst ., ,$(srcString))).FPGA)
PrjFPGAProjectToRule = $(Cfg.RulePath)/$(1).Build
PrjFPGAProjectsToRules = $(foreach project, $(PrjFPGAGetProjects),$(call PrjFPGAProjectToRule,$(project)))

define PrjFPGABuildE
Prj.$(1).Gen.RuleFile                     = $(call PrjFPGAProjectToRule,$(1))
Prj.$(1).Gen.Src                          = $(call PrjGetAllSrc,$(1))
Prj.$(1).Gen.BasePath                     = $$(Cfg.Prj.OutPath)/$(1)
Prj.$(1).Gen.BitOutFile                   = $$(Prj.$(1).Gen.BasePath)/$$(Prj.$(1).Out).bit
Prj.$(1).Gen.Tools.SeqAsm.SeqSrc          = $$(filter %.seq,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.SeqAsm.DevSrc          = $$(filter %.dev,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.SeqAsm.OutPath         = $$(Prj.$(1).Gen.BasePath)/SeqAsm
Prj.$(1).Gen.Tools.SeqAsm.OutFile         = $$(Prj.$(1).Gen.Tools.SeqAsm.OutPath)/SeqAsm.mem
Prj.$(1).Gen.Tools.SeqAsm.OutTextFile     = $$(Prj.$(1).Gen.Tools.SeqAsm.OutPath)/SeqAsm.text.mem
Prj.$(1).Gen.Tools.MemGen.MemSrc          = $$(filter %.mem,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.MemGen.OutPath         = $$(Prj.$(1).Gen.BasePath)/MemGen
Prj.$(1).Gen.Tools.MemGen.OutFile         = $$(Prj.$(1).Gen.Tools.MemGen.OutPath)/MemGen.v
Prj.$(1).Gen.Tools.IVerilog.VerilogSrc    = $$(filter %.v,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.IVerilog.OutPath       = $$(Prj.$(1).Gen.BasePath)/IVerilog
Prj.$(1).Gen.Tools.IVerilog.OutFile       = $$(Prj.$(1).Gen.Tools.IVerilog.OutPath)/IVerilog.v
Prj.$(1).Gen.Tools.Xst.VerilogSrc         = $$(Prj.$(1).Gen.Tools.IVerilog.OutFile)
Prj.$(1).Gen.Tools.Xst.OutPath            = $$(Prj.$(1).Gen.BasePath)/Xst
Prj.$(1).Gen.Tools.Xst.TmpPath            = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/tmp
Prj.$(1).Gen.Tools.Xst.ProjectFile        = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/XstProject.prj
Prj.$(1).Gen.Tools.Xst.LSearchFile        = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/XstLSearch.lso
Prj.$(1).Gen.Tools.Xst.CommandsFile       = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/XstCommands.cmd
Prj.$(1).Gen.Tools.Xst.OutFile            = $$(Prj.$(1).Gen.Tools.Xst.OutPath)/Xst.ngc
Prj.$(1).Gen.Tools.Xst.LogFile            = $$(Prj.$(1).Gen.Tools.Xst.TmpPath)/Xst.log
Prj.$(1).Gen.Tools.Xst.XrptFileMV         = $$(Prj.$(1).Gen.Tools.Xst.OutFile)_xst.xrpt
Prj.$(1).Gen.Tools.NgdBuild.NgcSrc        = $$(Prj.$(1).Gen.Tools.Xst.OutFile)
Prj.$(1).Gen.Tools.NgdBuild.UcfSrc        = $$(filter %.ucf,$$(Prj.$(1).Gen.Src))
Prj.$(1).Gen.Tools.NgdBuild.OutPath       = $$(Prj.$(1).Gen.BasePath)/NgdBuild
Prj.$(1).Gen.Tools.NgdBuild.TmpPath       = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/tmp
Prj.$(1).Gen.Tools.NgdBuild.CommandsFile  = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/NgdBuildCommands.cmd
Prj.$(1).Gen.Tools.NgdBuild.OutFile       = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/NgdBuild.ngd
Prj.$(1).Gen.Tools.NgdBuild.BldFileMV     = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/NgdBuild.bld
Prj.$(1).Gen.Tools.NgdBuild.XrptFileMV    = $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)/NgdBuild_ngdbuild.xrpt
Prj.$(1).Gen.Tools.NgdBuild.XbcdFileMV    = xlnx_auto_0_xdb/cst.xbcd
Prj.$(1).Gen.Tools.NgdBuild.XdbPathRM     = xlnx_auto_0_xdb
Prj.$(1).Gen.Tools.Map.NgdSrc             = $$(Prj.$(1).Gen.Tools.NgdBuild.OutFile)
Prj.$(1).Gen.Tools.Map.OutPath            = $$(Prj.$(1).Gen.BasePath)/Map
Prj.$(1).Gen.Tools.Map.TmpPath            = $$(Prj.$(1).Gen.Tools.Map.OutPath)/tmp
Prj.$(1).Gen.Tools.Map.CommandsFile       = $$(Prj.$(1).Gen.Tools.Map.OutPath)/MapCommands.cmd
Prj.$(1).Gen.Tools.Map.NcdOutFile         = $$(Prj.$(1).Gen.Tools.Map.OutPath)/Map.ncd
Prj.$(1).Gen.Tools.Map.PcfOutFile         = $$(Prj.$(1).Gen.Tools.Map.OutPath)/Map.pcf
Prj.$(1).Gen.Tools.Map.XrptFileMV         = $$(Prj.$(1).Top)_map.xrpt
Prj.$(1).Gen.Tools.Map.DeviceDetailsMV    = xilinx_device_details.xml
Prj.$(1).Gen.Tools.Map.MapFileMV          = $$(Prj.$(1).Gen.Tools.Map.OutPath)/Map.map
Prj.$(1).Gen.Tools.Map.MrpFileMV          = $$(Prj.$(1).Gen.Tools.Map.OutPath)/Map.mrp
Prj.$(1).Gen.Tools.Map.NgmFileMV          = $$(Prj.$(1).Gen.Tools.Map.OutPath)/Map.ngm
Prj.$(1).Gen.Tools.Par.NcdSrc             = $$(Prj.$(1).Gen.Tools.Map.NcdOutFile)
Prj.$(1).Gen.Tools.Par.PcfSrc             = $$(Prj.$(1).Gen.Tools.Map.PcfOutFile)
Prj.$(1).Gen.Tools.Par.OutPath            = $$(Prj.$(1).Gen.BasePath)/Par
Prj.$(1).Gen.Tools.Par.TmpPath            = $$(Prj.$(1).Gen.Tools.Par.OutPath)/tmp
Prj.$(1).Gen.Tools.Par.CommandsFile       = $$(Prj.$(1).Gen.Tools.Par.OutPath)/ParCommands.cmd
Prj.$(1).Gen.Tools.Par.NcdOutFile         = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par.ncd
Prj.$(1).Gen.Tools.Par.XrptFileMV         = $$(Prj.$(1).Top)_par.xrpt
Prj.$(1).Gen.Tools.Par.TwrFileMV          = smartpreview.twr
Prj.$(1).Gen.Tools.Par.PadFileMV          = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par.pad
Prj.$(1).Gen.Tools.Par.PadCsvFileMV       = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par_pad.csv
Prj.$(1).Gen.Tools.Par.PadTxtFileMV       = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par_pad.txt
Prj.$(1).Gen.Tools.Par.ParFileMV          = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par.par
Prj.$(1).Gen.Tools.Par.PtwxFileMV         = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par.ptwx
Prj.$(1).Gen.Tools.Par.UnroutesFileMV     = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par.unroutes
Prj.$(1).Gen.Tools.Par.XpiFileMV          = $$(Prj.$(1).Gen.Tools.Par.OutPath)/Par.xpi
Prj.$(1).Gen.Tools.BitGen.NcdSrc          = $$(Prj.$(1).Gen.Tools.Par.NcdOutFile)
Prj.$(1).Gen.Tools.BitGen.PcfSrc          = $$(Prj.$(1).Gen.Tools.Map.PcfOutFile)
Prj.$(1).Gen.Tools.BitGen.OutPath         = $$(Prj.$(1).Gen.BasePath)/BitGen
Prj.$(1).Gen.Tools.BitGen.TmpPath         = $$(Prj.$(1).Gen.Tools.BitGen.OutPath)/tmp
Prj.$(1).Gen.Tools.BitGen.CommandsFile    = $$(Prj.$(1).Gen.Tools.BitGen.OutPath)/BitGenCommands.cmd
Prj.$(1).Gen.Tools.BitGen.BitOutFile      = $$(Prj.$(1).Gen.Tools.BitGen.OutPath)/BitGen.bit
Prj.$(1).Gen.Tools.BitGen.DeviceDetailsMV = xilinx_device_details.xml
Prj.$(1).Gen.Tools.BitGen.BgnFileMV       = $$(Prj.$(1).Gen.Tools.BitGen.OutPath)/BitGen.bgn
Prj.$(1).Gen.Tools.BitGen.DrcFileMV       = $$(Prj.$(1).Gen.Tools.BitGen.OutPath)/BitGen.drc

$$(Prj.$(1).Gen.RuleFile): $$(Prj.$(1).Gen.Src) $$(DevProjectsToRules) _out
# 	Start building the project.
	@$$(call LineH1,Building Prj $(1))
	mkdir -p $$(Prj.$(1).Gen.BasePath)

#	Compile any .seq files, and produce .mem files for MemGen.
	@$$(call LineH2,Building Prj $(1) : SeqAsm)
	mkdir -p $$(Prj.$(1).Gen.Tools.SeqAsm.OutPath)
	$$(if $$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc),\
		$$(Cfg.Prj.Targets.Sim.Tools.SeqAsm.Invoke) \
			-o $$(Prj.$(1).Gen.Tools.SeqAsm.OutFile) \
			-t $$(Prj.$(1).Gen.Tools.SeqAsm.OutTextFile) \
			-s $$(Cfg.Prj.Targets.Sim.Tools.SeqAsm.SequencersFile) \
			-c $$(Cfg.Prj.Targets.Sim.Tools.SeqAsm.ComponentsFile) \
			-d $$(Prj.$(1).Gen.Tools.SeqAsm.DevSrc) \
			$$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc))

#	Compile any .mem files, and produce a MemGen.v file for IVerilog.
	@$$(call LineH2,Building Prj $(1) : MemGen)
	mkdir -p $$(Prj.$(1).Gen.Tools.MemGen.OutPath)
	$$(if $$(and $$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc),\
		     $$(Prj.$(1).Gen.Tools.MemGen.MemSrc)),\
		$$(Cfg.Prj.Targets.Sim.Tools.MemGen.Invoke) \
			-o $$(Prj.$(1).Gen.Tools.MemGen.OutFile) \
			$$(Prj.$(1).Gen.Tools.MemGen.MemSrc) \
			$$(Prj.$(1).Gen.Tools.SeqAsm.OutFile) \
			$$(Prj.$(1).Gen.Tools.SeqAsm.OutTextFile),\
		$$(if $$(Prj.$(1).Gen.Tools.MemGen.MemSrc),\
			$$(Cfg.Prj.Targets.Sim.Tools.MemGen.Invoke) \
				-o $$(Prj.$(1).Gen.Tools.MemGen.OutFile) \
				$$(Prj.$(1).Gen.Tools.MemGen.MemSrc)))

#       Preprocess all .v files with IVerilog.
	@$$(call LineH2,Building Prj $(1) : IVerilog)
	mkdir -p $$(Prj.$(1).Gen.Tools.IVerilog.OutPath)
	$$(Cfg.Prj.Targets.FPGA.Tools.IVerilog.Invoke) \
		-o $$(Prj.$(1).Gen.Tools.IVerilog.OutFile) \
		-Wall -Wno-timescale \
		-E \
		-DFPGA \
		$$(foreach fpgaDef,$$(filter Prj.$(1).Def.%,$$(.VARIABLES)),\
			$$(if $$(value $$(fpgaDef)), \
				-D$$(word 5,$$(subst ., ,$$(fpgaDef)))=$$(value $$(fpgaDef)), \
				-D$$(word 5,$$(subst ., ,$$(fpgaDef))))) \
		$$(Prj.$(1).Gen.Tools.IVerilog.VerilogSrc) \
		$$(if $$(or $$(Prj.$(1).Gen.Tools.SeqAsm.SeqSrc),\
			    $$(Prj.$(1).Gen.Tools.MemGen.MemSrc)),\
			$$(Prj.$(1).Gen.Tools.MemGen.OutFile))

#	Compile all .v files with Xst.
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

	$$(Cfg.Prj.Targets.FPGA.Tools.Xst.Invoke) \
		-ifn $$(Prj.$(1).Gen.Tools.Xst.CommandsFile) \
		-ofn $$(Prj.$(1).Gen.Tools.Xst.LogFile)

	mv -f $$(Prj.$(1).Gen.Tools.Xst.XrptFileMV) $$(Prj.$(1).Gen.Tools.Xst.TmpPath)

#       Call the ngdbuild tool.
	@$$(call LineH2,Building Prj $(1) : NgdBuild)
	mkdir -p $$(Prj.$(1).Gen.Tools.NgdBuild.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)

	echo -p $$(Cfg.Prj.Targets.FPGA.Part) >> $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)
	echo -uc $$(Prj.$(1).Gen.Tools.NgdBuild.UcfSrc) >> $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)
	echo -dd $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath) >> $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile)

	$$(Cfg.Prj.Targets.FPGA.Tools.NgdBuild.Invoke) \
		-f $$(Prj.$(1).Gen.Tools.NgdBuild.CommandsFile) \
		$$(Prj.$(1).Gen.Tools.NgdBuild.NgcSrc) \
		$$(Prj.$(1).Gen.Tools.NgdBuild.OutFile)

	mv -f $$(Prj.$(1).Gen.Tools.NgdBuild.BldFileMV) $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.NgdBuild.XrptFileMV) $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.NgdBuild.XbcdFileMV) $$(Prj.$(1).Gen.Tools.NgdBuild.TmpPath)

	rm -rf $$(Prj.$(1).Gen.Tools.NgdBuild.XdbPathRM)

#       Call the map tool.
	@$$(call LineH2,Building Prj $(1) : Map)
	mkdir -p $$(Prj.$(1).Gen.Tools.Map.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.Map.CommandsFile)

	echo -detail >> $$(Prj.$(1).Gen.Tools.Map.CommandsFile)
	echo -pr b >> $$(Prj.$(1).Gen.Tools.Map.CommandsFile)
	echo -o $$(Prj.$(1).Gen.Tools.Map.NcdOutFile) >> $$(Prj.$(1).Gen.Tools.Map.CommandsFile)

	$$(Cfg.Prj.Targets.FPGA.Tools.Map.Invoke) \
		-f $$(Prj.$(1).Gen.Tools.Map.CommandsFile) \
		$$(Prj.$(1).Gen.Tools.Map.NgdSrc) \
		$$(Prj.$(1).Gen.Tools.Map.PcfOutFile)

	mv -f $$(Prj.$(1).Gen.Tools.Map.XrptFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.DeviceDetailsMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.MapFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.MrpFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Map.NgmFileMV) $$(Prj.$(1).Gen.Tools.Map.TmpPath)

#       Call the par tool.
	@$$(call LineH2,Building Prj $(1) : Par)
	mkdir -p $$(Prj.$(1).Gen.Tools.Par.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.Par.CommandsFile)

	echo -w >> $$(Prj.$(1).Gen.Tools.Par.CommandsFile)

	$$(Cfg.Prj.Targets.FPGA.Tools.Par.Invoke) \
		-f $$(Prj.$(1).Gen.Tools.Par.CommandsFile) \
		$$(Prj.$(1).Gen.Tools.Par.NcdSrc) \
		$$(Prj.$(1).Gen.Tools.Par.NcdOutFile) \
		$$(Prj.$(1).Gen.Tools.Par.PcfSrc)

	mv -f $$(Prj.$(1).Gen.Tools.Par.XrptFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.TwrFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PadFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PadCsvFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PadTxtFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.ParFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.PtwxFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.UnroutesFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.Par.XpiFileMV) $$(Prj.$(1).Gen.Tools.Par.TmpPath)

#	Call the bitgen tool.
	@$$(call LineH2,Building Prj $(1) : BitGen)
	mkdir -p $$(Prj.$(1).Gen.Tools.BitGen.OutPath)
	mkdir -p $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)
	rm -rf $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)

	echo -w >> $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)
	echo -g StartUpClk:CClk >> $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)
	echo -g CRC:Enable >> $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile)

	$$(Cfg.Prj.Targets.FPGA.Tools.BitGen.Invoke) \
		-f $$(Prj.$(1).Gen.Tools.BitGen.CommandsFile) \
		$$(Prj.$(1).Gen.Tools.BitGen.NcdSrc) \
		$$(Prj.$(1).Gen.Tools.BitGen.BitOutFile) \
		$$(Prj.$(1).Gen.Tools.BitGen.PcfSrc)

	mv -f $$(Prj.$(1).Gen.Tools.BitGen.DeviceDetailsMV) $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.BitGen.BgnFileMV) $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)
	mv -f $$(Prj.$(1).Gen.Tools.BitGen.DrcFileMV) $$(Prj.$(1).Gen.Tools.BitGen.TmpPath)

#	It's all done.
	@$$(call LineH2,Building Prj $(1) : Done)
	echo Done > $$(Prj.$(1).Gen.RuleFile)
	cp $$(Prj.$(1).Gen.Tools.BitGen.BitOutFile) $$(Prj.$(1).Gen.BitOutFile)
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
Dev.$(1).Gen.Tools.Ghc.OutFile    = $$(Dev.$(1).Gen.Tools.Ghc.OutPath)/Ghc.bin

$$(Dev.$(1).Gen.RuleFile): $$(Dev.$(1).Gen.Src) _out
	@$$(call LineH1,Building Dev $(1))
	mkdir -p $$(Dev.$(1).Gen.BasePath)

	@$$(call LineH2,Building Dev $(1) : Ghc)
	mkdir -p $$(Dev.$(1).Gen.Tools.Ghc.OutPath)
	mkdir -p $$(Dev.$(1).Gen.Tools.Ghc.TmpPath)
	$(Cfg.Dev.Tools.Ghc.Invoke) --make -o $$(Dev.$(1).Gen.Tools.Ghc.OutFile) -odir $$(Dev.$(1).Gen.Tools.Ghc.TmpPath) -hidir $$(Dev.$(1).Gen.Tools.Ghc.TmpPath) $$(Dev.$(1).Gen.Tools.Ghc.HaskellSrc)

	@$$(call LineH2,Building Dev $(1) : Done)
	echo Done > $$(Dev.$(1).Gen.RuleFile)
	cp $$(Dev.$(1).Gen.Tools.Ghc.OutFile) $$(Dev.$(1).Gen.OutFile)
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
