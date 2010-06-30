{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Configs
    (parseSequencersCfg,
     parseComponentsCfg,
     parseDeviceCfg) where

import Data.ByteString.Char8 as C8 (ByteString(..),pack,unpack)
import Data.Object (Object(..),ObjectExtractError(..),fromMapping,fromScalar,lookupObject)
import Data.Object.Yaml (YamlScalar(..),YamlObject(..),decode)
import Data.Either (partitionEithers)
import Data.List (intercalate,unfoldr)

import Text.Libyaml (Tag(..),Style(..))
import Text.Regex.Posix ((=~))

import Control.Failure (Failure(..))

import Core (DevicesCfg(..),Sequencer(..),Component(..),Inst(..),ArgType(..),FormatAtom(..),Device(..))
import Utils (toLefts,maybeToEither,gatherEithers)

parseSequencersCfg :: C8.ByteString -> Either String [(String,Sequencer)]
parseSequencersCfg text =
    toLefts ("Failed parsing of sequencers configuration file!\n"++) $
    case decode text :: Maybe YamlObject of
      Nothing -> fail "The file is not in a valid YAML format!"
      Just (Scalar _) -> fail "The file contains an YAML scalar instead of a mapping!"
      Just (Sequence _) -> fail "The file contains an YAML sequence instead of a mapping!"
      Just (Mapping sequencers) -> gatherEithers $ map interpretSequencer sequencers

parseComponentsCfg :: C8.ByteString -> Either String [(String,Component)]
parseComponentsCfg text =
    toLefts ("Failed parsing of components configuration file!\n"++) $
    case decode text :: Maybe YamlObject of
      Nothing -> fail "The file is not a valid YAML format!"
      Just (Scalar _) -> fail "The file contains an YAML scalar instead of a mapping!"
      Just (Sequence _) -> fail "The file contains an YAML scalar instead of a mapping!"
      Just (Mapping components) -> gatherEithers $ map interpretComponent components

parseDeviceCfg :: DevicesCfg -> C8.ByteString -> Either String Device
parseDeviceCfg devicesCfg text =
    toLefts ("Failed parsing device configuration file!\n"++) $
    case decode text :: Maybe YamlObject of
      Nothing -> fail "The file is not a valid YAML format!"
      Just (Scalar _) -> fail "The file contains an YAML scalar instead of a mapping!"
      Just (Sequence _) -> fail "The file contains an YAML scalar instead of a mapping!"
      Just (Mapping deviceMap) -> interpretDevice devicesCfg deviceMap

instance Failure ObjectExtractError (Either String) where
    -- failure :: ObjectExtractError -> Either String a
    failure _ = fail ""

interpretSequencer :: (YamlScalar,YamlObject) -> Either String (String,Sequencer)
interpretSequencer (YamlScalar sequencerName _ _,Mapping sequencerMap) =
    toLefts (("Cannot process " ++ show sequencerName ++ "!\n")++) $ do
        configurationMap <- yamlGetMapFromMap "Configuration" sequencerMap

        wordSize <- yamlGetScalarFromMap "WordSize" configurationMap
        addressSize <- yamlGetScalarFromMap "AddressSize" configurationMap
        inputs <- yamlGetScalarFromMap "Inputs" configurationMap
        outputs <- yamlGetScalarFromMap "Outputs" configurationMap
        instructionSize <- yamlGetScalarFromMap "InstructionSize" configurationMap
        commandSize <- yamlGetScalarFromMap "CommandSize" configurationMap
        deviceCommandSize <- yamlGetScalarFromMap "DeviceCommandSize" configurationMap

        wordSizeInt <- readInt $ C8.unpack $ value wordSize
        addressSizeInt <- readInt $ C8.unpack $ value addressSize
        inputsInt <- readInt $ C8.unpack $ value inputs
        outputsInt <- readInt $ C8.unpack $ value outputs
        instructionSizeInt <- readInt $ C8.unpack $ value instructionSize
        commandSizeInt <- readInt $ C8.unpack $ value commandSize
        deviceCommandSizeInt <- readInt $ C8.unpack $ value deviceCommandSize

        instructionsMap <- yamlGetMapFromMap "Instructions" sequencerMap

        instructions <- interpretInstructions wordSizeInt instructionsMap

        return $ (C8.unpack sequencerName,
                  Sequencer {
                   sequencerWordSize = wordSizeInt,
                   sequencerAddressSize = addressSizeInt,
                   sequencerInputs = inputsInt,
                   sequencerOutputs = outputsInt,
                   sequencerInstructionSize = instructionSizeInt,
                   sequencerCommandSize = commandSizeInt,
                   sequencerDeviceCommandSize = deviceCommandSizeInt,
                   sequencerInstructions = instructions})
interpretSequencer (seqName,Sequence _) =
    fail $ "Sequencer's " ++ (show $ value seqName) ++ " body is a sequence, instead of a mapping!"
interpretSequencer (seqName,Scalar _) =
    fail $ "Sequencer's " ++ (show $ value seqName) ++ " body is a scalar value, instead of a mapping!"

interpretComponent :: (YamlScalar,YamlObject) -> Either String (String,Component)
interpretComponent (YamlScalar componentName _ _,Mapping componentMap) =
    toLefts (("Cannot process " ++ show componentName ++ "!\n")++) $ do
        configurationMap <- yamlGetMapFromMap "Configuration" componentMap

        commandSize <- yamlGetScalarFromMap "CommandSize" configurationMap
        argumentSize <- yamlGetScalarFromMap "ArgumentSize" configurationMap
        outputs <- yamlGetScalarFromMap "Outputs" configurationMap

        commandSizeInt <- readInt $ C8.unpack $ value commandSize
        argumentSizeInt <- readInt $ C8.unpack $ value argumentSize
        outputsFinal <- return $ words $ C8.unpack $ value  outputs

        instructionsMap <- yamlGetMapFromMap "Instructions" componentMap

        instructions <- interpretInstructions argumentSizeInt instructionsMap

        return $ (C8.unpack componentName,
                  Component {
                   componentCommandSize = commandSizeInt,
                   componentArgumentSize = argumentSizeInt,
                   componentOutputs = outputsFinal,
                   componentInstructions = instructions})
interpretComponent (seqName,Sequence _) =
    fail $ "Component's " ++ (show $ value seqName) ++ " body is a sequence, instead of a mapping!"
interpretComponent (seqName,Scalar _) =
    fail $ "Component's " ++ (show $ value seqName) ++ " body is a scalar value, instead of a mapping!"

interpretDevice :: DevicesCfg -> [(YamlScalar,YamlObject)] -> Either String Device
interpretDevice devicesCfg@(DevicesCfg sequencers _) deviceMap = do
    sequencerMap <- yamlGetMapFromMap "Sequencer" deviceMap

    romName <- yamlGetScalarFromMap "RomName" sequencerMap
    stype <- yamlGetScalarFromMap "Type" sequencerMap
    outputs <- yamlGetScalarFromMap "Outputs" sequencerMap
    inputs <- yamlGetScalarFromMap "Inputs" sequencerMap

    romNameSymbol <- readSymbol $ C8.unpack $ value romName
    stypeSymbol <- readSymbol $ C8.unpack $ value stype
    sequencer <- maybeToEither ("Cannot find sequencer " ++ show stypeSymbol ++ "!") $ lookup stypeSymbol sequencers

    outputsFinal <- return $ words $ C8.unpack $ value outputs
    inputsFinal <- gatherEithers $ map extractComponentOutput $ words $ C8.unpack $ value inputs

    componentsMap <- yamlGetMapFromMap "Components" deviceMap

    components <- gatherEithers $ map (findDeviceComponents devicesCfg) componentsMap

    return $ Device {
               deviceRomName = romNameSymbol,
               deviceSequencer = sequencer,
               deviceComponents = components,
               deviceSeqOutputs = outputsFinal,
               deviceSeqInputs = inputsFinal}

interpretInstructions :: Int -> [(YamlScalar,YamlObject)] -> Either String [(String,Inst)]
interpretInstructions wordSize instructionsMap =
    gatherEithers $ map (interpretInstruction wordSize) instructionsMap
    
interpretInstruction :: Int -> (YamlScalar,YamlObject) -> Either String (String,Inst)
interpretInstruction wordSize (YamlScalar instructionName _ _,Mapping instructionMap) = do
    (name,arguments) <- extractNameAndArgs wordSize (C8.unpack instructionName)

    opCode <- yamlGetScalarFromMap "OpCode" instructionMap
    opCodeInt <- readInt $ C8.unpack $ value opCode
    format <- yamlGetScalarFromMap "Format" instructionMap

    parsedFormat <- extractFormat $ C8.unpack $ value format

    return $ (name,Inst {
                   instArguments = arguments,
                   instOpCode = opCodeInt,
                   instFormat = parsedFormat})

extractNameAndArgs :: Int -> String -> Either String (String,[(String,ArgType)])
extractNameAndArgs wordSize fullName = do
    (afterName,name) <- case fullName =~ "^([[:alpha:]_][[:alnum:]_]*)[[:space:]]*" :: (String,String,String,[String]) of
                          ("",captureName,afterName,[name]) -> return (afterName,name)
                          _ -> fail $ "Invalid format for instruction " ++ show fullName ++ "!"
    args <- gatherEithers $ unfoldr (extractArg wordSize) afterName

    return (name,args)

extractArg :: Int -> String -> Maybe (Either String (String,ArgType),String)
extractArg _ "" =
    Nothing
extractArg wordSize whatsLeft = 
    case whatsLeft =~ "^[(][[:space:]]*([[:alpha:]_][[:alnum:]_]*)[[:space:]]+([^)]*)[)][[:space:]]*" :: (String,String,String,[String]) of
      ("",matched,after,[argName,argType]) -> do
          case extractArgType wordSize argType of
            Right finalArgType -> Just (Right (argName,finalArgType),after)
            Left errorMessage -> Just (Left errorMessage,after)
      _ -> do
        Just (Left $ "Could not make sense of instruction arguments starting at " ++ show whatsLeft ++ "!","")

extractArgType :: Int -> String -> Either String ArgType
extractArgType wordSize "Immediate" = return (Immediate wordSize)
extractArgType wordSize "Address" = return Address
extractArgType wordSize "DeviceCommand" = return DeviceCommand
extractArgType wordSize "DeviceInput" = return DeviceInput
extractArgType wordSize argType =
    case argType =~ "Immediate[[:space:]]*\\[size=([[:digit:]]+)\\]" :: (String,String,String,[String]) of
      ("",matched,"",[size]) ->
          return $ Immediate (read size)
      _ ->
          fail $ "Error in argument type " ++ show argType ++ "!"

extractFormat :: String -> Either String [FormatAtom]
extractFormat format =
    gatherEithers $ unfoldr extractAtom format

extractAtom :: String -> Maybe (Either String FormatAtom,String)
extractAtom "" =
    Nothing
extractAtom whatsLeft =
    case whatsLeft =~ "([[:alnum:]\\:]*)[[:space:]]*" :: (String,String,String,[String]) of
      ("",matched,after,[supposedAtom]) ->
          case (readLiteral supposedAtom,readSymbol supposedAtom) of
            (Right _ ,Right  _) -> Just (Left $ "Could not make sense of format at " ++ show whatsLeft ++ "!","It is both a literal and a reference!")
            (Right literal ,Left _) -> Just (Right $ Core.Literal literal,after)
            (Left _, Right reference) -> Just (Right $ Core.Reference reference,after)
            (Left errorMessage0, Left errorMessage1) ->
                Just (Left $ "Could not make sense of format at " ++ show whatsLeft ++ "!" ++ "\n" ++ 
                             "It is neither a literal, nor a reference!" ++ "\n" ++
                             errorMessage0 ++ "\n" ++
                             errorMessage1,"")
      _ ->
          Just (Left $ "Could not make sense of format at " ++ show whatsLeft ++ "!","")
 
extractComponentOutput :: String -> Either String (String,String)
extractComponentOutput componentOutput =
    case componentOutput =~ "([[:alpha:]_][[:alnum:]_]*)\\.([[:alpha:]_][[:alnum:]_]*)" :: (String,String,String,[String]) of
      ("",matched,"",[component,output]) -> return (component,output)
      _ -> fail $ "Could not understand device inputs " ++ show componentOutput ++ "!"

findDeviceComponents :: DevicesCfg -> (YamlScalar,YamlObject) -> Either String (String,Component)
findDeviceComponents (DevicesCfg _ components) (YamlScalar componentName _ _, componentTypeObject) = do
    (YamlScalar componentType _ _) <- yamlGetScalarFromObject (C8.unpack componentName) componentTypeObject
    component <- maybeToEither ("Cannot find component " ++ show componentType ++ "!") $ lookup (C8.unpack componentType) components

    return (C8.unpack componentName,component)

yamlScalar :: String -> YamlScalar
yamlScalar value = YamlScalar (C8.pack value) NoTag Plain

yamlGetMapFromMap :: String -> [(YamlScalar,YamlObject)] -> Either String [(YamlScalar,YamlObject)]
yamlGetMapFromMap keyName sourceMap = do
    object <- toLefts (("Cannot find " ++ show keyName ++ " key!")++) $ lookupObject (yamlScalar keyName) sourceMap
    map <- toLefts (("Value for key " ++ show keyName ++ " is not a map!")++) $ fromMapping object

    return map

yamlGetScalarFromMap :: String -> [(YamlScalar,YamlObject)] -> Either String YamlScalar
yamlGetScalarFromMap keyName sourceMap = do
    object <- toLefts (("Cannot find " ++ show keyName ++ " key!")++) $ lookupObject (yamlScalar keyName) sourceMap
    scalar <- toLefts (("Value for key " ++ show keyName ++ " is not a scalar!")++) $ fromScalar object

    return scalar

yamlGetScalarFromObject :: String -> YamlObject -> Either String YamlScalar
yamlGetScalarFromObject keyName object = do
  scalar <- toLefts (("Value for key " ++ show keyName ++ " is not a scalar!")++) $ fromScalar object

  return scalar

readLiteral :: String -> Either String String
readLiteral supposedLiteral =
    case supposedLiteral =~ "([0-9a-fA-F]+):(b|o|h)" :: (String,String,String,[String]) of
      ("",matched,"",[literalValue,base]) -> 
          case partitionEithers $ map (digitToBase2 base) literalValue of
            ([],results) -> return $ concat results
            (errorMessages,_) -> fail $ intercalate "\n" errorMessages
      _ -> fail $ "Invalid literal value " ++ show supposedLiteral ++ "!"
    where digitToBase2 :: String -> Char -> Either String String
          digitToBase2 "b" '0' = Right "0"
          digitToBase2 "b" '1' = Right "1"
          digitToBase2 "b" digit = Left $ "Invalid binary digit " ++ show digit ++ " in literal " ++ show supposedLiteral ++ "!"
          digitToBase2 "o" '0' = Right "000"
          digitToBase2 "o" '1' = Right "001"
          digitToBase2 "o" '2' = Right "010"
          digitToBase2 "o" '3' = Right "011"
          digitToBase2 "o" '4' = Right "100"
          digitToBase2 "o" '5' = Right "101"
          digitToBase2 "o" '6' = Right "110"
          digitToBase2 "o" '7' = Right "111"
          digitToBase2 "o" digit = Left $ "Invalid octal digit " ++ show digit ++ " in literal " ++ show supposedLiteral ++ "!"
          digitToBase2 "h" '0' = Right "0000"
          digitToBase2 "h" '1' = Right "0001"
          digitToBase2 "h" '2' = Right "0010"
          digitToBase2 "h" '3' = Right "0011"
          digitToBase2 "h" '4' = Right "0100"
          digitToBase2 "h" '5' = Right "0101"
          digitToBase2 "h" '6' = Right "0110"
          digitToBase2 "h" '7' = Right "0111"
          digitToBase2 "h" '8' = Right "1000"
          digitToBase2 "h" '9' = Right "1001"
          digitToBase2 "h" 'A' = Right "1010"
          digitToBase2 "h" 'B' = Right "1011"
          digitToBase2 "h" 'C' = Right "1100"
          digitToBase2 "h" 'D' = Right "1101"
          digitToBase2 "h" 'E' = Right "1110"
          digitToBase2 "h" 'F' = Right "1111"
          digitToBase2 "h" 'a' = Right "1010"
          digitToBase2 "h" 'b' = Right "1011"
          digitToBase2 "h" 'c' = Right "1100"
          digitToBase2 "h" 'd' = Right "1101"
          digitToBase2 "h" 'e' = Right "1110"
          digitToBase2 "h" 'f' = Right "1111"
          digitToBase2 "h" digit = Left $ "Invalid hex digit " ++ show digit ++ " in literal " ++ show supposedLiteral ++ "!"

readSymbol :: String -> Either String String
readSymbol supposedReference =
    case supposedReference =~ "[[:alpha:]_][[:alnum:]_]*" :: (String,String,String) of
      ("",matched,"") ->
          return matched
      _ ->
          fail $ "Invalid symbol " ++ show supposedReference ++ "!"

readInt :: String -> Either String Int
readInt supposedInt =
    case supposedInt =~ "[0-9]+" :: (String,String,String) of
      ("",matched,"") ->
          return $ read matched
      _ ->
          fail $ "Invalid integer " ++ show supposedInt ++ "!"
