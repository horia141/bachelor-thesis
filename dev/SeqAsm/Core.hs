module Core
    (CDevice(..),
     CSequencer(..),
     CComponent(..),
     CInst(..),
     CArgType(..),
     CFormatAtom(..),
     SInst(..),
     SArgType(..)) where
        
data CDevice
    = CDevice {
        cDeviceRomName :: String,
        cDeviceSequencer :: CSequencer,
        cDeviceComponents :: [(String,CComponent)],
        cDeviceSeqOutputs :: [String],
        cDeviceSeqInputs :: [(String,String)]}
    deriving (Show)

data CSequencer
    = CSequencer {
        cSequencerName :: String,
        cSequencerWordSize :: Int,
        cSequencerAddressSize :: Int,
        cSequencerInputs :: Int,
        cSequencerInputsSize :: Int,
        cSequencerOutputs :: Int,
        cSequencerOutputsSize :: Int,
        cSequencerInstructionSize :: Int,
        cSequencerCommandSize :: Int,
        cSequencerComponentCommandSize :: Int,
        cSequencerInstructions :: [(String,CInst)]}
    deriving (Show)

data CComponent
    = CComponent {
        cComponentName :: String,
        cComponentCommandSize :: Int,
        cComponentArgumentSize :: Int,
        cComponentOutputs :: [String],
        cComponentInstructions :: [(String,CInst)]}
    deriving (Show)

data CInst
    = CInst {
        cInstOpCode :: Int,
        cInstArguments :: [(String,CArgType)],
        cInstFormat :: [CFormatAtom]}
    deriving (Show)

data CArgType
    = CImmediate
    | CLabel
    | CComponentCommand
    | CComponentInput
    deriving (Show)

data CFormatAtom
    = CLiteral {
        cLiteralValue :: String}
    | CReference {
        cReferenceValue :: String}
    deriving (Show)

data SInst
    = SInst {
        sInstLabel :: String,
        sInstAddress :: Int,
        sInstInstName :: String,
        sInstOperands :: [SArgType],
        sInstLine :: String,
        sInstLineNumber :: Int}
    deriving (Show)

data SArgType
    = SImmediate {
        sArgImmediateValue :: String}
    | SLabel {
        sArgLabelValue :: String}
    | SComponentItemPair {
        sArgComponentCommandComponent :: String,
        sArgComponentCommandItem :: String}
    deriving (Show)
