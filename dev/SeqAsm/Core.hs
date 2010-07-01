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
        cSequencerWordSize :: Int,
        cSequencerAddressSize :: Int,
        cSequencerInputs :: Int,
        cSequencerOutputs :: Int,
        cSequencerInstructionSize :: Int,
        cSequencerCommandSize :: Int,
        cSequencerDeviceCommandSize :: Int,
        cSequencerInstructions :: [(String,CInst)]}
    deriving (Show)

data CComponent
    = CComponent {
        cComponentCommandSize :: Int,
        cComponentArgumentSize :: Int,
        cComponentOutputs :: [String],
        cComponentInstructions :: [(String,CInst)]}
    deriving (Show)

data CInst
    = CInst {
        cInstArguments :: [(String,CArgType)],
        cInstOpCode :: Int,
        cInstFormat :: [CFormatAtom]}
    deriving (Show)

data CArgType
    = CImmediate {
        cImeddiateSize :: Int}
    | CAddress
    | CDeviceCommand
    | CDeviceInput
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
        sInstOpCode :: String,
        sInstOperands :: [SArgType]}
    deriving (Show)

data SArgType
    = SArgImmediate {
        sArgImmediateValue :: String}
    | SArgLabel {
        sArgLabelValue :: String}
    | SArgDeviceItemPair {
        sArgDeviceCommandDevice :: String,
        sArgDeviceCommandItem :: String}
    deriving (Show)
