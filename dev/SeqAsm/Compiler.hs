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
import Utils (toLefts,toRights,gatherEithers,digitToBase2,intToBinaryString,maybeToEither)
import Configs () -- just for Either Monad instance

compile :: CDevice -> String -> Either String String
compile device text = do
    insts <- parseSeqText text
    binaryLines <- gatherEithers $ map (compileInst device (getLabelAddresses insts)) insts

    return $ genBody device ++ intercalate "\n" binaryLines

genBody :: CDevice -> String
genBody (CDevice romName sequencer components seqOutputs seqInputs) =
    "Name: " ++ romName ++ "\n" ++
    "AddrSize: " ++ (show $ cSequencerAddressSize sequencer) ++ "\n" ++
    "WordSize: " ++ (show $ cSequencerInstructionSize sequencer) ++ "\n" ++
    "Format: Bin" ++ "\n\n"

getLabelAddresses :: [SInst] -> [(String,Int)]
getLabelAddresses insts =
    map (\ inst -> (sInstLabel inst,sInstAddress inst)) $ filter ((/="") . sInstLabel) insts

compileInst :: CDevice -> [(String,Int)] -> SInst -> Either String String
compileInst device@(CDevice romName sequencer components seqOutputs seqInputs) labels (SInst label address instName operands line lineNumber) = 
    toLefts (("Error compiling line " ++ show lineNumber ++ " " ++ show line ++ "!\n")++) $ do
    inst <- maybeToEither ("Instruction " ++ show instName ++ " is invalid for sequencer " ++ (show $ cSequencerName sequencer) ++ "!") $ 
            lookup instName (cSequencerInstructions sequencer)
    argValues <- resolveArguments device labels (cInstArguments inst) operands
    evaledFormat <- evalFormat argValues (cInstFormat inst)

    let opCode = intToBinaryString (cSequencerCommandSize sequencer) (cInstOpCode inst)

    if length opCode + length evaledFormat == cSequencerInstructionSize sequencer
      then return $ opCode ++ evaledFormat
      else fail "Evaluated instruction has a different size from sequencer instruction size!"

resolveArguments :: CDevice -> [(String,Int)] -> [(String,CArgType)] -> [SArgType] -> Either String [(String,String)]
resolveArguments device labels arguments operands
    | length arguments /= length operands =
        fail $ "Instruction expects " ++ (show $ length arguments) ++ " argument(s) but got " ++ (show $ length operands) ++ "!"
    | otherwise =
        gatherEithers $ zipWith (resolveArgument device labels) arguments operands

resolveArgument :: CDevice -> [(String,Int)] -> (String,CArgType) -> SArgType -> Either String (String,String)
resolveArgument device labels (argumentName,CImmediate size) (SImmediate value)
    | length value /= size =
        fail $ "Instruction expects as argument " ++ show argumentName ++ " an immediate of " ++ show size ++ " bits, but it received one of " ++ (show $ length value) ++ " bits!"
    | otherwise =
        return (argumentName,value)
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
resolveArgument device labels (argumentName,CImmediate size) (SLabel value) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " an immediate of " ++ show size ++ " bits, but it received label \"@" ++ value ++ "\"!"
resolveArgument device labels (argumentName,CImmediate size) (SComponentItemPair component item) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " an immediate of " ++ show size ++ " bits, bit it received the pair \"" ++ component ++ "." ++ item ++ "\"!"
resolveArgument device labels (argumentName,CLabel) (SImmediate _) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a label, but it received an immediate!"
resolveArgument device labels (argumentName,CLabel) (SComponentItemPair component item) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a label, but it received the pair \"" ++ component ++ "." ++ item ++ "\"!"
resolveArgument device labels (argumentName,CComponentCommand) (SImmediate _) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a device command, but it received an immediate!"
resolveArgument device labels (argumentName,CComponentCommand) (SLabel value) =
    fail $ "Instruction expects as argument " ++ show argumentName ++ " a device command, but it received label \"@" ++ value ++ "\"!"
resolveArgument device labels (argumentName,CComponentInput) (SImmediate value) =
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
    toLefts (("Could not make sense of line " ++ show lineNumber ++ " " ++ show line ++ "!\n")++) $
    toLefts show $ 
    parse (do {whiteSpace seqLexer; r <- seqInst address line lineNumber; eof; return r}) "" line

justComment :: String -> Bool
justComment line = line =~ "^[[:space:]]*#"

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
            sInstLine = strip line,
            sInstLineNumber = lineNumber}
