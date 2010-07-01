module Compiler where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm
import Text.Regex.Posix ((=~))

import Core (CDevice(..),CSequencer(..),CComponent(..),CInst(..),CArgType(..),CFormatAtom(..),SInst(..),SArgType(..))
import Utils (toLefts,gatherEithers,digitToBase2)

compile :: CDevice -> String -> String
compile device text =
    undefined

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
    parse (do {whiteSpace seqLexer; r <- seqInst address; eof; return r}) "" line

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

  return $ SArgImmediate digits

seqImmediate8 :: Parser SArgType
seqImmediate8 = do
  digits <- many1 $ octDigit
  char ':'
  char 'o'

  return $ SArgImmediate $ concat $ either (error "Should Not Be Here 8") id $ gatherEithers $ map (digitToBase2 "o") digits

seqImmediate16 :: Parser SArgType
seqImmediate16 = do
  digits <- many1 $ hexDigit
  char ':'
  char 'h'

  return $ SArgImmediate $ concat $ either (error "Should Not Be Here 16") id $ gatherEithers $ map (digitToBase2 "h") digits

seqLabel :: Parser SArgType
seqLabel = do 
  char '@'
  label <- identifier seqLexer

  return $ SArgLabel label

seqDeviceItemPair :: Parser SArgType
seqDeviceItemPair = do
  device <- identifier seqLexer
  symbol seqLexer "."
  item <- identifier seqLexer

  return $ SArgDeviceItemPair device item

seqOpCode :: Parser String
seqOpCode = do
  identifier seqLexer

seqInst :: Int -> Parser SInst
seqInst address = do
  label <- try (seqLabel) <|> return (SArgLabel "")
  opCode <- seqOpCode
  operands <- many $ choice [try seqDeviceItemPair,try seqLabel,try seqImmediate2,try seqImmediate8,seqImmediate16]

  return $ SInst {
            sInstLabel = sArgLabelValue $ label,
            sInstAddress = address,
            sInstOpCode = opCode,
            sInstOperands = operands}
