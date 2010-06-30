module Core
    (DevicesCfg(..),
     Sequencer(..),
     Component(..),
     Inst(..),
     ArgType(..),
     FormatAtom(..),
     Device(..)) where

data DevicesCfg
    = DevicesCfg {
        devicesCfgSequencers :: [(String,Sequencer)],
        devicesCfgComponents :: [(String,Component)]}
    deriving (Show)

data Device
    = Device {
        deviceRomName :: String,
        deviceSequencer :: Sequencer,
        deviceComponents :: [(String,Component)],
        deviceSeqOutputs :: [String],
        deviceSeqInputs :: [(String,String)]}
    deriving (Show)

data Sequencer
    = Sequencer {
        sequencerWordSize :: Int,
        sequencerAddressSize :: Int,
        sequencerInputs :: Int,
        sequencerOutputs :: Int,
        sequencerInstructionSize :: Int,
        sequencerCommandSize :: Int,
        sequencerDeviceCommandSize :: Int,
        sequencerInstructions :: [(String,Inst)]}
    deriving (Show)

data Component
    = Component {
        componentCommandSize :: Int,
        componentArgumentSize :: Int,
        componentOutputs :: [String],
        componentInstructions :: [(String,Inst)]}
    deriving (Show)

data Inst
    = Inst {
        instArguments :: [(String,ArgType)],
        instOpCode :: Int,
        instFormat :: [FormatAtom]}
    deriving (Show)

data ArgType
    = Immediate {
        immediateSize :: Int}
    | Address
    | DeviceCommand
    | DeviceInput
    deriving (Show)

data FormatAtom
    = Literal {
        literalValue :: String}
    | Reference {
        referenceValue :: String}
    deriving (Show)
