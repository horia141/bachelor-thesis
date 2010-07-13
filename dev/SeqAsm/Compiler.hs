module Compiler
    (compile) where

import Data.List (intercalate,zipWith,elemIndex)
import Data.String.Utils (strip)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm
import Text.Regex.Posix ((=~))

import Core (CDevice(..),CSequencer(..),CComponent(..),CInst(..),CArgType(..),CFormatAtom(..),SInst(..),SArgType(..))
import Utils (toLefts,toRights,gatherEithers,digitToBase2,intToBinaryString,intToDecString,intToHexString,maybeToEither)
import Configs () -- just for Either Monad instance

maxNumberOfLines = 6
maxNumberOfAddresses = 4

header0 = "Compiled Line"
header1 = "Line"
header2 = "Adddress"
header3 = "Instruction"

compile :: CDevice -> String -> Either String (String,String)
compile device text =
    toLefts ("Could not compile source program!\n"++) $ do
    insts <- parseSeqText text
    (checkedDevice,checkedInsts) <- validate device insts
    binaryLines <- gatherEithers $ map (compileInst checkedDevice (getLabelAddresses checkedInsts)) checkedInsts

    return $ (genBody checkedDevice ++ intercalate "\n" binaryLines ++ "\n",
              genBodyText checkedDevice ++ intercalate "\n" (map (show . stripLabels . strip . sInstLine) insts))

genBody :: CDevice -> String
genBody (CDevice romName sequencer components seqOutputs seqInputs) =
    "Name: " ++ romName ++ "\n" ++
    "Type: ROM" ++ "\n" ++
    "AddrSize: " ++ (show $ cSequencerAddressSize sequencer) ++ "\n" ++
    "WordSize: " ++ (show $ cSequencerInstructionSize sequencer) ++ "\n" ++
    "Format: Bin" ++ "\n" ++
    "# Generator:" ++ "\n" ++
    "#     Name: SeqAsm" ++ "\n\n" ++
    "# " ++ header0 ++ " " ++ (replicate ((cSequencerInstructionSize sequencer) - (length header0) - 2) ' ') ++ 
    "# " ++ header1 ++ " " ++ (replicate (maxNumberOfLines - (length header1)) ' ') ++ 
    header2 ++ " " ++ (replicate (maxNumberOfAddresses - (length header2) + 2) ' ') ++ 
    header3 ++ "\n"

genBodyText :: CDevice -> String
genBodyText (CDevice romName sequencer components seqOutputs seqInputs) =
    "Name: " ++ romName ++ "Text" ++ "\n" ++
    "Type: ROM" ++ "\n" ++
    "AddrSize: " ++ (show $ cSequencerAddressSize sequencer) ++ "\n" ++
    "WordSize: 4096" ++ "\n" ++
    "Format: Str" ++ "\n" ++
    "# Generator:" ++ "\n" ++
    "#     Name: SeqAsm" ++ "\n\n"

stripLabels :: String -> String
stripLabels line =
    case line =~ "^[[:space:]]*@[[:alpha:]_][[:alnum:]_]*[[:space:]]+(.+)$" :: (String,String,String,[String]) of
      ("",matched,left,[instruction]) -> instruction
      _ -> line
    
getLabelAddresses :: [SInst] -> [(String,Int)]
getLabelAddresses insts =
    map (\ inst -> (sInstLabel inst,sInstAddress inst)) $ filter ((/="") . sInstLabel) insts

compileInst :: CDevice -> [(String,Int)] -> SInst -> Either String String
compileInst device@(CDevice romName sequencer components seqOutputs seqInputs) labels (SInst label address instName operands line lineNumber) = 
    toLefts (("Error compiling line " ++ show lineNumber ++ " " ++ (show $ strip line) ++ "!\n")++) $ do
    inst <- maybeToEither ("Instruction " ++ show instName ++ " is invalid for sequencer " ++ (show $ cSequencerName sequencer) ++ "!") $ 
            lookup instName (cSequencerInstructions sequencer)
    argValues <- resolveArguments device labels (cInstArguments inst) operands
    evaledFormat <- evalFormat argValues (cInstFormat inst)

    let opCode = intToBinaryString (cSequencerCommandSize sequencer) (cInstOpCode inst)

    if length opCode + length evaledFormat == cSequencerInstructionSize sequencer
      then let body0 = opCode ++ evaledFormat
               body1 = intToDecString maxNumberOfLines lineNumber
               body2 = intToHexString maxNumberOfAddresses address ++ ":h"
               body3 = line
           in return $ body0 ++ " " ++ (replicate ((length header0) - (cSequencerInstructionSize sequencer) + 2) ' ') ++ 
                       "# " ++ body1 ++ " " ++ (replicate ((length header1) - maxNumberOfLines) ' ') ++
                       body2 ++ " " ++ (replicate ((length header2) - maxNumberOfAddresses - 2) ' ') ++ 
                       body3
      else fail "Evaluated instruction has a different size from sequencer instruction size!"

resolveArguments :: CDevice -> [(String,Int)] -> [(String,CArgType)] -> [SArgType] -> Either String [(String,String)]
resolveArguments device labels arguments operands
    | length arguments /= length operands =
        fail $ "Instruction expects " ++ (show $ length arguments) ++ " argument(s) but got " ++ (show $ length operands) ++ "!"
    | otherwise =
        gatherEithers $ zipWith (resolveArgument device labels) arguments operands

resolveArgument :: CDevice -> [(String,Int)] -> (String,CArgType) -> SArgType -> Either String (String,String)
resolveArgument (CDevice romName sequencer components seqOutputs seqInputs) labels (argumentName,CImmediate) (SImmediate value)
    | length value /= (cSequencerWordSize sequencer) =
        fail $ "Instruction expects as argument " ++ show argumentName ++ " an immediate of " ++ (show $ cSequencerWordSize sequencer) ++ " bits, but it received one of " ++ (show $ length value) ++ " bits!"
    | otherwise =
        return (argumentName,replicate (cSequencerWordSize sequencer - length value) '0' ++ value)
resolveArgument (CDevice romName sequencer components seqOutputs seqInputs) labels (argumentName,CLabel) (SLabel value) = do
    labelInst <- maybeToEither ("Label " ++ show value ++ " does not exist!") $
                 lookup value labels

    return (argumentName,intToBinaryString (cSequencerAddressSize sequencer) labelInst)
resolveArgument (CDevice romName sequencer components seqOutputs seqInputs) labels (argumentName,CComponentCommand) (SComponentItemPair componentName item) = do
    component <- maybeToEither ("Component " ++ show componentName ++ " does not exist!") $
                 lookup componentName components
    inst <- maybeToEither ("Instruction " ++ show item ++ " is invalid for " ++ (cComponentName component) ++ " " ++ show componentName ++ "!") $
            lookup item (cComponentInstructions component)
    outputIndex <- maybeToEither ("Component " ++ show componentName ++ " does not appear in the device outputs section!") $
                   elemIndex componentName seqOutputs

    return (argumentName,intToBinaryString (cSequencerOutputsSize sequencer) outputIndex ++
                         intToBinaryString (cSequencerComponentCommandSize sequencer) (cInstOpCode inst))
resolveArgument (CDevice romName sequencer components seqOutputs seqInputs) labels (argumentName,CComponentInput) (SComponentItemPair componentName item) = do
    component <- maybeToEither ("Component " ++ show componentName ++ " does not exist!") $
                 lookup componentName components
    outputIndex <- maybeToEither ("Component " ++ show componentName ++ " does not have a " ++ show item ++ " output!") $
                   elemIndex item (cComponentOutputs component)
    componentInputIndex <- maybeToEither ("Device does not have \"" ++ componentName ++ "." ++ item ++ "\" as input!") $
                           elemIndex (componentName,item) seqInputs

    return (argumentName,intToBinaryString (cSequencerInputsSize sequencer) componentInputIndex)
resolveArgument device labels (argumentName,CImmediate) (SLabel value) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " an immediate of " ++ (show $ cSequencerWordSize $ cDeviceSequencer device) ++ " bits, but it received label \"@" ++ value ++ "\"!"
resolveArgument device labels (argumentName,CImmediate) (SComponentItemPair component item) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " an immediate of " ++ (show $ cSequencerWordSize $ cDeviceSequencer device) ++ " bits, bit it received the pair \"" ++ component ++ "." ++ item ++ "\"!"
resolveArgument device labels (argumentName,CLabel) (SImmediate _) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a label, but it received an immediate!"
resolveArgument device labels (argumentName,CLabel) (SComponentItemPair component item) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a label, but it received the pair \"" ++ component ++ "." ++ item ++ "\"!"
resolveArgument device labels (argumentName,CComponentCommand) (SImmediate _) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a device command, but it received an immediate!"
resolveArgument device labels (argumentName,CComponentCommand) (SLabel value) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a device command, but it received label \"@" ++ value ++ "\"!"
resolveArgument device labels (argumentName,CComponentInput) (SImmediate _) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a device input, but it received an immediate!"
resolveArgument device labels (argumentName,CComponentInput) (SLabel value) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a device input, but it received label \"@" ++ value ++ "\"!"

evalFormat :: [(String,String)] -> [CFormatAtom] -> Either String String
evalFormat argValues format =
    toRights concat $ gatherEithers $ map (evalAtom argValues) format

evalAtom :: [(String,String)] -> CFormatAtom -> Either String String
evalAtom argValues (CLiteral value) =
    return value
evalAtom argValues (CReference name) =
    maybeToEither ("Argument " ++ show name ++ " does not exist!") $
    lookup name argValues

parseSeqText :: String -> Either String [SInst]
parseSeqText text =
    toLefts ("Could not parse sequencer program source file!\n"++) $
    gatherEithers $ 
    map parseLine $ 
    zip [0..] $ 
    filter (not . justComment . snd) $ 
    filter ((/="") . snd) $ 
    zip [1..] (lines text)

parseLine :: (Int,(Int,String)) -> Either String SInst
parseLine (address,(lineNumber,line)) =
    toLefts (("Could not make sense of line " ++ show lineNumber ++ " " ++ (show $ strip line) ++ "!\n")++) $
    toLefts show $ 
    parse (do {whiteSpace seqLexer; r <- seqInst address line lineNumber; eof; return r}) "" line

justComment :: String -> Bool
justComment line = line =~ "^[[:space:]]*#"

validate :: CDevice -> [SInst] -> Either String (CDevice,[SInst])
validate device insts =
    toLefts ("Could not validate device and program!\n"++) $
    checkProgramFitsInMemory device insts >>
    checkEveryComponentInOutput device insts >>
    checkSequencerOpCodeFits device insts >>
    checkComponentsOpCodeFits device insts >>
    checkSequencerCanCommand device insts

checkProgramFitsInMemory :: CDevice -> [SInst] -> Either String (CDevice,[SInst])
checkProgramFitsInMemory device insts =
    let instCount = length insts
        maxInstCount = 2^(cSequencerAddressSize $ cDeviceSequencer device)
        sequencerName = cSequencerName $ cDeviceSequencer device
    in if instCount <= maxInstCount
       then return (device,insts)
       else fail $ "There are too many instructions (" ++ show instCount ++ ") for sequencer of type " ++ show sequencerName ++ " (it allows just " ++ show maxInstCount ++ ")!"

checkEveryComponentInOutput :: CDevice -> [SInst] -> Either String (CDevice,[SInst])
checkEveryComponentInOutput device insts =
    toRights (head) $ gatherEithers $ 
    map (isInOutput (cDeviceSeqOutputs device)) (cDeviceComponents device)
    where isInOutput :: [String] -> (String,CComponent) -> Either String (CDevice,[SInst])
          isInOutput outputs (componentName,component) =
              if elem componentName outputs
              then return (device,insts)
              else fail $ "Component " ++ show componentName ++ " is not present as a sequencer output in the device file!"

checkSequencerOpCodeFits :: CDevice -> [SInst] -> Either String (CDevice,[SInst])
checkSequencerOpCodeFits device insts =
    toRights (head) $ gatherEithers $
    map (opCodeFits (cSequencerCommandSize $ cDeviceSequencer device)) (cSequencerInstructions $ cDeviceSequencer device)
    where opCodeFits :: Int -> (String,CInst) -> Either String (CDevice,[SInst])
          opCodeFits commandSize (instName,inst) =
              if cInstOpCode inst < 2^commandSize
              then return (device,insts)
              else fail $ "Sequencer instruction " ++ show instName ++ " (opcode: " ++ show (cInstOpCode inst) ++ ") is too big for sequencer " ++
                          show (cSequencerName $ cDeviceSequencer device) ++ " command field (size:" ++ show commandSize ++ ")!"

checkComponentsOpCodeFits :: CDevice -> [SInst] -> Either String (CDevice,[SInst])
checkComponentsOpCodeFits device insts =
    toRights (head) $ gatherEithers $ 
    map (checkComponent . snd) (cDeviceComponents device)
    where checkComponent :: CComponent -> Either String (CDevice,[SInst])
          checkComponent component =
              toRights (head) $ gatherEithers $
              map (opCodeFits component (cComponentCommandSize component)) (cComponentInstructions component)
          opCodeFits :: CComponent -> Int -> (String,CInst) -> Either String (CDevice,[SInst])
          opCodeFits component commandSize (instName,inst) =
              if cInstOpCode inst < 2^commandSize
              then return (device,insts)
              else fail $ "Component instruction " ++ show instName ++ " (opcode: " ++ show (cInstOpCode inst) ++ ") is too big for component " ++
                          show (cComponentName component) ++ " command field (size: " ++ show commandSize ++ ")!"
                               
checkSequencerCanCommand :: CDevice -> [SInst] -> Either String (CDevice,[SInst])
checkSequencerCanCommand device insts =
    toRights (head) $ gatherEithers $
    map (checkCommand (cSequencerComponentCommandSize $ cDeviceSequencer device)) (cDeviceComponents device)
    where checkCommand :: Int -> (String,CComponent) -> Either String (CDevice,[SInst])
          checkCommand componentCommandSize (componentName,component) =
              if cComponentCommandSize component <= componentCommandSize
              then return (device,insts)
              else fail $ "Sequencer " ++ show (cSequencerName $ cDeviceSequencer device) ++ " cannot command component " ++
                          show componentName ++ " of type " ++ show (cComponentName component) ++ "!\n" ++ 
                          "The latter's command field (size: " ++ show (cComponentCommandSize component) ++
                          ") is greater than the one for the sequencer (size: " ++
                          show (cSequencerComponentCommandSize $ cDeviceSequencer device) ++ ")!"
 
seqLexer :: TokenParser ()
seqLexer = makeTokenParser (LanguageDef {
                             commentStart    = "",
                             commentEnd      = "",
                             commentLine     = "#",
                             nestedComments  = True,
                             identStart      = letter <|> char '_',
                             identLetter     = letter <|> digit <|> char '_',
                             opStart         = oneOf "",
                             opLetter        = oneOf "",
                             reservedOpNames = [],
                             reservedNames   = [],
                             caseSensitive   = True})

seqImmediate2 :: Parser SArgType
seqImmediate2 = do
  digits <- many1 $ choice [char '0',char '1']
  char ':'
  char 'b'

  return $ SImmediate digits

seqImmediate8 :: Parser SArgType
seqImmediate8 = do
  digits <- many1 $ octDigit
  char ':'
  char 'o'

  return $ SImmediate $ concat $ either (error "Should Not Be Here 8") id $ gatherEithers $ map (digitToBase2 "o") digits

seqImmediate16 :: Parser SArgType
seqImmediate16 = do
  digits <- many1 $ hexDigit
  char ':'
  char 'h'

  return $ SImmediate $ concat $ either (error "Should Not Be Here 16") id $ gatherEithers $ map (digitToBase2 "h") digits

seqLabel :: Parser SArgType
seqLabel = do 
  char '@'
  label <- identifier seqLexer

  return $ SLabel label

seqComponentItemPair :: Parser SArgType
seqComponentItemPair = do
  device <- identifier seqLexer
  symbol seqLexer "."
  item <- identifier seqLexer

  return $ SComponentItemPair device item

seqInstName :: Parser String
seqInstName = do
  identifier seqLexer

seqInst :: Int -> String -> Int -> Parser SInst
seqInst address line lineNumber = do
  label <- try (seqLabel) <|> return (SLabel "")
  instName <- seqInstName
  operands <- many $ choice [try seqComponentItemPair,try seqLabel,try seqImmediate2,try seqImmediate8,seqImmediate16]

  return $ SInst {
            sInstLabel = sArgLabelValue $ label,
            sInstAddress = address,
            sInstInstName = instName,
            sInstOperands = operands,
            sInstLine = line,
            sInstLineNumber = lineNumber}
