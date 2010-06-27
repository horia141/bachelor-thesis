import Data.ByteString.Char8 as C8 (ByteString(..))

data DevicesCfg
    = DevicesCfg {
        devicesCfgSequencers :: [Sequencer],
        devicesCfgComponents :: [Component]}
    deriving (Show)

data Device
    = Device {
        deviceName :: String,
        deviceSequencer :: Sequencer,
        deviceComponents :: [(String,Component)],
        deviceSeqOuputs :: [String],
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
        sequencerInstructions :: [Inst]}
    deriving (Show)

data Component
    = Component {
        componentCommandSize :: Int,
        componentArgumentSize :: Int,
        componentInstructions :: [Inst],
        componentOuputs :: [String]}
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
    | Refrence {
        referenceValue :: String}
    deriving (Show)

parseDevicesCfg :: C8.ByteString -> Either String DevicesCfg
parseDevicesCfg text = undefined

parseDevice :: C8.ByteString -> Either String Device
parseDevice text = undefined

main :: IO ()
main = do
  putStrLn "Hello"
