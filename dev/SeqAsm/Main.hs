{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.ByteString.Char8 as C8 (ByteString(..),readFile,pack,unpack)
import Data.Object (Object(..),ObjectExtractError(..),fromMapping,fromScalar,lookupObject)
import Data.Object.Yaml (YamlScalar(..),YamlObject(..),decode)
import Data.Maybe (maybe,fromMaybe)
import Data.Either (partitionEithers)
import Data.List (intercalate)

import Text.Libyaml (Tag(..),Style(..))
import Text.Regex.Posix ((=~))

import Control.Failure (Failure(..))

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
        sequencerInstructions :: [(String,Inst)]}
    deriving (Show)

data Component
    = Component {
        componentCommandSize :: Int,
        componentArgumentSize :: Int,
        componentInstructions :: [(String,Inst)],
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

toLefts :: (a -> c) -> Either a b -> Either c b
toLefts f (Left x) = Left $ f x
toLefts f (Right x) = Right x

toRights :: (b -> c) -> Either a b -> Either a c
toRights f (Left x) = Left x
toRights f (Right x) = Right $ f x

instance Failure ObjectExtractError (Either String) where
    -- failure :: ObjectExtractError -> Either String a
    failure _ = fail ""

yamlScalar :: String -> YamlScalar
yamlScalar value = YamlScalar (C8.pack value) NoTag Plain

yamlGetMap :: String -> [(YamlScalar,YamlObject)] -> Either String [(YamlScalar,YamlObject)]
yamlGetMap keyName sourceMap = do
    object <- toLefts (("Cannot find " ++ show keyName ++ " key!")++) $ lookupObject (yamlScalar keyName) sourceMap
    map <- toLefts (("Value for key " ++ show keyName ++ " is not a map!")++) $ fromMapping object

    return map

yamlGetScalar :: String -> [(YamlScalar,YamlObject)] -> Either String YamlScalar
yamlGetScalar keyName sourceMap = do
    object <- toLefts (("Cannot find " ++ show keyName ++ " key!")++) $ lookupObject (yamlScalar keyName) sourceMap
    scalar <- toLefts (("Value for key " ++ show keyName ++ " is not a scalar!")++) $ fromScalar object

    return scalar

interpretInstructions :: Int -> [(YamlScalar,YamlObject)] -> Either String [(String,Inst)]
interpretInstructions wordSize instructionsMap =
    case partitionEithers $ map interpretInstruction instructionsMap of
      ([],results) -> return results
      (errorMessages,_) -> fail $ intercalate "\n" errorMessages
    where interpretInstruction :: (YamlScalar,YamlObject) -> Either String (String,Inst)
          interpretInstruction (YamlScalar instructionName _ _,Mapping instructionMap) = do
              (name,arguments) <- extractNameAndArgs instructionName

              opCode <- yamlGetScalar "OpCode" instructionMap
              format <- yamlGetScalar "Format" instructionMap

              parsedFormat <- extractFormat format

              return $ (name,Inst {
                             instArguments :: arguments,
                             instOpCode :: readInt opCode,
                             instFormat :: parsedFormat})
              where extractNameAndArgs :: String -> (String,[(String,ArgType)])
                    extractNameAndArgs fullName =
                        let symbolRexp = "[[:alpha]_][[:alnum:]_]*"
                            nameRexp = "$[[:space:]]*" ++ symbolRexp ++ "[[:space:]]*"
                            argumentRexp = "$\\((" ++ symbolRexp ++ ") (" ++ symbolRexp ++ ")\\[\\]=[:space:]\\)[[:space:]]+"

                        (afterName,name) <- case fullName =~ nameRexp of
                                         (beforeName,captureName,afterName,[name]) ->
                                             return (afterName,name)
                                         _ ->
                                             fail $ "Invalid format for instruction " ++ show fullName ++ "!"

                        case partitionEithers $ unfoldr (extractArg argumentRexp) afterName
                          ([],args) -> return (name,args)
                          (errorsMessages,_) -> fail $ intercalate "\n" errorMessages
                    extractArg :: String -> String -> Maybe (Either String (String,ArgType),String)
                    extractArg _ "" =
                        Nothing
                    extractArg pattern whatsLeft = 
                        case whatsLeft =~ pattern of
                          (before,matched,after,[argName,argType]) -> do
                              case extractArgType argType of
                                Right finalArgType -> Just (Right (argName,finalArgType),after)
                                Left errorMessage -> Just (Left errorMessage,after)
                          _ -> do
                            Just (Left $ "Could not make sense of instruction arguments starting at " ++ show whatsLeft ++ "!","")
                    extractArgType :: String -> Either String ArgType
                    extractArgType "Immediate" = return (Immediate wordSize)
                    extractArgType "Address" = return Address
                    extractArgType "DeviceCommand" = return DeviceCommand
                    extractArgType "DeviceInput" = return DeviceInput
                    extractArgType argType =
                        case argType =~ "Immediate[[:space:]]*\\[size=([[:digit:]]+)\\]" of
                          (before,matched,"",[size]) ->
                              return $ Immediate (read size)
                          _ ->
                              fail $ "Error in argument type " ++ show argType ++ "!"

parseSequencersCfg :: C8.ByteString -> Either String [Sequencer]
parseSequencersCfg text = toLefts ("Failed parsing of sequencers configuration file!\n"++) $
    case decode text :: Maybe YamlObject of
      Nothing -> fail "The file is not in a valid YAML format!"
      Just (Scalar _) -> fail "The file contains an YAML scalar instead of a mapping!"
      Just (Sequence _) -> fail "The file contains an YAML sequence instead of a mapping!"
      Just (Mapping sequencers) -> 
          case partitionEithers $ map interpretSequencer sequencers of
            ([],results) -> return results
            (errorMessages,_) -> fail $ intercalate "\n" $ map ("Error:"++) errorMessages
    where interpretSequencer :: (YamlScalar,YamlObject) -> Either String Sequencer
          interpretSequencer (YamlScalar sequencerName _ _,Mapping sequencerMap) =
              toLefts (("Cannot process " ++ (show sequencerName) ++ "!\n")++) $ do
                  configurationMap <- yamlGetMap "Configuration" sequencerMap

                  wordSize <- yamlGetScalar "WordSize" configurationMap
                  addressSize <- yamlGetScalar "AddressSize" configurationMap
                  inputs <- yamlGetScalar "Inputs" configurationMap
                  outputs <- yamlGetScalar "Outputs" configurationMap
                  instructionSize <- yamlGetScalar "InstructionSize" configurationMap
                  commandSize <- yamlGetScalar "CommandSize" configurationMap
                  deviceCommandSize <- yamlGetScalar "DeviceCommandSize" configurationMap

                  instructionsMap <- yamlGetMap "Instructions" sequencerMap

                  instructions <- interpretInstructions instructionsMap

                  return $ Sequencer {
                           sequencerWordSize = read $ C8.unpack $ value wordSize,
                           sequencerAddressSize = read $ C8.unpack $ value addressSize,
                           sequencerInputs = read $ C8.unpack $ value inputs,
                           sequencerOutputs = read $ C8.unpack $ value outputs,
                           sequencerInstructionSize = read $ C8.unpack $ value instructionSize,
                           sequencerCommandSize = read $ C8.unpack $ value commandSize,
                           sequencerDeviceCommandSize = read $ C8.unpack $ value deviceCommandSize,
                           sequencerInstructions = instructions}
          interpretSequencer (seqName,Sequence _) =
              fail $ "Sequencer's " ++ (show $ value seqName) ++ " body is a sequence, instead of a mapping!"
          interpretSequencer (seqName,Scalar _) =
              fail $ "Sequencer's " ++ (show $ value seqName) ++ " body is a scalar value, instead of a mapping!"

parseDevice :: C8.ByteString -> Either String Device
parseDevice text = undefined

main :: IO ()
main = do
  sequencersCfg <- C8.readFile "/home/horia/work/batchelor/v6/prj/Sequencers.cfg"
  componentsCfg <- C8.readFile "/home/horia/work/batchelor/v6/prj/Components.cfg"
  deviceText    <- C8.readFile "/home/horia/work/batchelor/v6/prj/Auto2.dev"

  case parseSequencersCfg sequencersCfg of
    Right result -> putStrLn $ show result
    Left error -> putStrLn error