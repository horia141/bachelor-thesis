module Parser
    (parseProgram) where

import Data.List (intercalate)
import Data.Either (partitionEithers)
import Data.Char (digitToInt,isHexDigit,isDigit)
import Numeric (readInt)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

import Defines (SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..))

pLexer = makeTokenParser (LanguageDef {
                            commentStart    = "",
                            commentEnd      = "",
                            commentLine     = "#",
                            nestedComments  = True,
                            identStart      = letter <|> char '_',
                            identLetter     = letter <|> char '_' <|> digit <|> char ':',
                            opStart         = oneOf "",
                            opLetter        = oneOf "",
                            reservedOpNames = ["+","-","*","/","%","++","!"],
                            reservedNames   = ["module","import","define"],
                            caseSensitive   = True})

pExprNumb :: Parser SeqExpr
pExprNumb = (lexeme pLexer $ 
             choice [try $ do digits <- many1 $ oneOf "0123456789ABCDEFabcdef"
                              char 'h'
                                          
                              return $ Numb ((fst . head) (readInt 16 isHexDigit digitToInt digits)),
                     try $ do digits <- many1 $ oneOf "0123456789"
                              char 'd'

                              return $ Numb ((fst . head) (readInt 10 isDigit digitToInt digits)),
                     try $ do digits <- many1 $ oneOf "01"
                              char 'b'

                              return $ Numb ((fst . head) (readInt 2 (\ x -> x == '0' || x == '1') digitToInt digits)),
                     do digits <- many1 $ oneOf "0123456789"

                        return $ Numb ((fst . head) (readInt 10 isDigit digitToInt digits))]) <?> "number"

pExprCall :: Parser SeqExpr
pExprCall = do function <- identifier pLexer
               arguments <- option [] $ parens pLexer $ commaSep1 pLexer pExpr

               return (Call function arguments) <?> "function call"

pExprTable :: OperatorTable Char () SeqExpr
pExprTable = [[Infix (pExprOper "!") AssocRight],
              [Infix (pExprOper "*") AssocLeft,Infix (pExprOper "/") AssocLeft,Infix (pExprOper "%") AssocLeft],
              [Infix (pExprOper "+") AssocLeft,Infix (pExprOper "-") AssocLeft],
              [Infix (pExprOper "~") AssocLeft]]
    where pExprOper :: String -> Parser (SeqExpr -> SeqExpr -> SeqExpr)
          pExprOper op = do reservedOp pLexer op
                            return (\ x y -> Call op [x,y])

pExprFactor :: Parser SeqExpr
pExprFactor = pExprNumb <|> 
              pExprCall <|> 
              parens pLexer pExpr <?> "simple expression"

pExpr :: Parser SeqExpr
pExpr = buildExpressionParser pExprTable pExprFactor <?> "expression"

pDefine :: Parser SeqDefine
pDefine = do reserved pLexer "define"
             name <- identifier pLexer
             arguments <- option [] $ parens pLexer $ commaSep1 pLexer $ identifier pLexer
             lexeme pLexer $ string "="
             body <- pExpr

             return $ Define name arguments body

pInstruction :: Parser SeqInstruction
pInstruction = do label <- option "" $ try $ do char '@'
                                                id <- identifier pLexer
                                                return id
                  expr <- pExpr

                  return $ Instruction 0 label expr

pImport :: Parser (String,[String])
pImport = do reserved pLexer "import"
             name <- identifier pLexer
             imports <- option [] $ parens pLexer $ 
                        ((try $ commaSep1 pLexer $ identifier pLexer) <|>
                         (do lexeme pLexer $ string "*"
                             return []))

             return (name,imports)

pModule :: Parser SeqModule
pModule = do reserved pLexer "module"
             name <- identifier pLexer
             exports <- option [] $ parens pLexer $
                        ((try $ commaSep1 pLexer $ identifier pLexer) <|>
                         (do lexeme pLexer $ string "*"
                             return []))

             imports <- many pImport
             defines <- many pDefine
             instructions <- many pInstruction

             return $ Module name exports imports defines instructions

parseProgram :: String -> [(String,String)] -> Either String SeqProgram
parseProgram entry fncPairs = 
    case partitionEithers $ map parseModule fncPairs of
      ([],modules) -> Right $ Program entry modules
      (errs,_) -> Left $ intercalate "\n" errs
    where parseModule :: (String,String) -> Either String SeqModule
          parseModule (fileName,contents) = 
              case parse fullModule fileName contents of
                Left err -> Left $ show err
                Right res -> Right res
              where fullModule :: Parser SeqModule
                    fullModule = do whiteSpace pLexer
                                    res <- pModule
                                    eof
                                    return res
