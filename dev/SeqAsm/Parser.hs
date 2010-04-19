module Parser
    (parseProgram) where

import Data.List (intercalate,groupBy,sortBy)
import Data.Either (partitionEithers)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

import Defines (SeqSourceInfo(..),SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..),SeqOpFixity(..),SeqOpAssoc(..))
import Primitives (primitives)
import Utils (strToInt)

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

sourcePosToSourceInfo :: String -> SourcePos -> SeqSourceInfo
sourcePosToSourceInfo sourceInit position =
    SourceInfo (sourceName position) (sourceLine position) (sourceColumn position) (take 10 sourceInit)

pLexer = makeTokenParser (LanguageDef {
                            commentStart    = "",
                            commentEnd      = "",
                            commentLine     = "#",
                            nestedComments  = True,
                            identStart      = letter <|> char '_',
                            identLetter     = letter <|> char '_' <|> digit,
                            opStart         = oneOf "",
                            opLetter        = oneOf "",
                            reservedOpNames = ["+","-","*","/","%","++","!"],
                            reservedNames   = ["module","import","define","as"],
                            caseSensitive   = True})

pExprNumb :: Parser SeqExpr
pExprNumb = do sourceInit <- getInput
               position <- getPosition

               value <- lexeme pLexer $ 
                        choice [try $ do digits <- many1 $ oneOf "0123456789ABCDEFabcdef"
                                         char 'h'
                                          
                                         return $ strToInt 16 digits,
                                try $ do digits <- many1 $ oneOf "0123456789"
                                         char 'd'

                                         return $ strToInt 10 digits,
                                try $ do digits <- many1 $ oneOf "01"
                                         char 'b'

                                         return $ strToInt 2 digits,
                                do digits <- many1 $ oneOf "0123456789"

                                   return $ strToInt 10 digits]

               return (Numb value (sourcePosToSourceInfo sourceInit position)) <?> "number"

pExprCall :: Parser SeqExpr
pExprCall = do sourceInit <- getInput
               position <- getPosition

               pmodule <- option "" $ do pmodule <- identifier pLexer
                                         colon pLexer
                                         return pmodule
               function <- identifier pLexer
               arguments <- option [] $ parens pLexer $ commaSep1 pLexer pExpr

               return (Call pmodule function arguments (sourcePosToSourceInfo sourceInit position)) <?> "function call"

pExprTable :: OperatorTable Char () SeqExpr
pExprTable = makeExprTable $ filter isPrimOperator $ moduleDefines primitives
    where isPrimOperator :: SeqDefine -> Bool
          isPrimOperator (PrimOper _ _ _ _) = True
          isPrimOperator _ = False

          makeExprTable :: [SeqDefine] -> OperatorTable Char () SeqExpr
          makeExprTable primitiveOps = map (map toOpTableEntry) $ groupBy isPriorityEq $ sortBy priorityCmp primitiveOps
              where isPriorityEq (PrimOper _ _ _ (_,_,a)) (PrimOper _ _ _ (_,_,b)) = a == b
                    priorityCmp (PrimOper _ _ _ (_,_,a)) (PrimOper _ _ _ (_,_,b)) = compare a b
                    toOpTableEntry (PrimOper name _ _ (OpInfix,OpLeft,_)) = Infix (pExprOperBin name) AssocLeft
                    toOpTableEntry (PrimOper name _ _ (OpInfix,OpRight,_)) = Infix (pExprOperBin name) AssocRight
                    toOpTableEntry (PrimOper name _ _ (OpPostfix,_,_)) = Postfix (pExprOperUn name)
                    toOpTableEntry (PrimOper name _ _ (OpPrefix,_,_)) = Prefix (pExprOperUn name)
                                              
                    pExprOperBin :: String -> Parser (SeqExpr -> SeqExpr -> SeqExpr)
                    pExprOperBin op = do sourceInit <- getInput
                                         position <- getPosition
                            
                                         reservedOp pLexer op
                                         return (\ x y -> Call "Primitives" op [x,y] (sourcePosToSourceInfo sourceInit position))

                    pExprOperUn :: String -> Parser (SeqExpr -> SeqExpr)
                    pExprOperUn op = do sourceInit <- getInput
                                        position <- getPosition

                                        reservedOp pLexer op
                                        return (\ x -> Call "Primitives" op [x] (sourcePosToSourceInfo sourceInit position))

pExprFactor :: Parser SeqExpr
pExprFactor = pExprNumb <|> 
              pExprCall <|> 
              parens pLexer pExpr <?> "simple expression"

pExpr :: Parser SeqExpr
pExpr = buildExpressionParser pExprTable pExprFactor <?> "expression"

pDefine :: Parser SeqDefine
pDefine = do sourceInit <- getInput
             position <- getPosition

             reserved pLexer "define"
             name <- identifier pLexer
             arguments <- option [] $ parens pLexer $ commaSep1 pLexer $ identifier pLexer
             lexeme pLexer $ string "="
             body <- pExpr

             return (UserFunc name arguments body (sourcePosToSourceInfo sourceInit position)) <?> "define"

pInstruction :: Parser SeqInstruction
pInstruction = do sourceInit <- getInput
                  position <- getPosition

                  label <- option "" $ try $ do char '@'
                                                id <- identifier pLexer
                                                return id
                  expr <- pExpr

                  return (Instruction 0 label expr (sourcePosToSourceInfo sourceInit position)) <?> "instruction"

pImport :: Parser (String,String,[String])
pImport = do reserved pLexer "import"
             name <- identifier pLexer
             imports <- option [] $ parens pLexer $ 
                        ((try $ commaSep1 pLexer $ identifier pLexer) <|>
                         (do lexeme pLexer $ string "*"
                             return []))
             alias <- option "" $ do reserved pLexer "as"
                                     alias <- identifier pLexer
                                     return alias

             return (name,alias,imports) <?> "import"

pModule :: Parser SeqModule
pModule = do sourceInit <- getInput
             position <- getPosition

             reserved pLexer "module"
             name <- identifier pLexer
             exports <- option [] $ parens pLexer $
                        ((try $ commaSep1 pLexer $ identifier pLexer) <|>
                         (do lexeme pLexer $ string "*"
                             return []))

             imports <- many pImport
             defines <- many pDefine
             instructions <- many pInstruction

             return (Module name exports imports defines instructions (sourcePosToSourceInfo sourceInit position)) <?> "module"
