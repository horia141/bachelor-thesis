module Compiler
    (compileProgram,
     evalExpr,
     putEntryFirst,
     placeModules) where

import Data.List (find,findIndex,mapAccumL,genericLength)
import Data.Either (partitionEithers)
import Data.Char (intToDigit)

import Numeric (showIntAtBase)

import Defines (SeqSourceInfo(..),SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..))

(|->) :: a -> (a -> Either [String] b) -> Either [String] b
(|->) value f = f value

(>->) :: Either [String] a -> (a -> Either [String] b) -> Either [String] b
(>->) (Right value) f = f value
(>->) (Left errString) f = Left errString

eLift :: (a -> b) -> Either [String] a -> Either [String] b
eLift f (Right value) = Right (f value)
eLift f (Left errString) = Left errString

eReduce :: [Either [String] a] -> Either [String] [a]
eReduce eithers = 
    case partitionEithers eithers of
      ([],values) -> Right values
      (errStrings,_) -> Left $ concat errStrings

findName :: [SeqModule] -> SeqModule -> [(String,String)] -> String -> ([String],String,(SeqModule,SeqDefine))
findName modules currentModule namedArguments name =
    case lookupArguments of
      Just value -> ([],value,undefined)
      Nothing -> 
          case lookupInstruction of
            Just value -> ([],[],(currentModule,Define name [] (Numb (instructionAddress value) SourceStub) SourceStub))
            Nothing ->
                case lookupLocal of
                  Just define -> ([],[],(currentModule,define))
                  Nothing ->
                      case lookupImport of
                        Just (parentModule,imported) -> ([],[],(parentModule,imported))
                        Nothing -> ([name ++ " is not defined or not visible in module " ++ moduleName currentModule],[],undefined)
    where lookupArguments :: Maybe String
          lookupArguments = lookup name namedArguments

          lookupInstruction :: Maybe SeqInstruction
          lookupInstruction = find ((name==) . instructionLabel) $ moduleInstructions currentModule

          lookupLocal :: Maybe SeqDefine
          lookupLocal = find ((name==) . defineName) $ moduleDefines currentModule

          lookupImport :: Maybe (SeqModule,SeqDefine)
          lookupImport = Nothing

evalExpr :: [SeqModule] -> SeqModule -> [(String,String)] -> SeqExpr -> Either [String] String
evalExpr modules currentModule namedArguments (Null) = Right ""
evalExpr modules currentModule namedArguments (Numb value isource) = Right $ showIntAtBase 2 intToDigit value ""
evalExpr modules currentModule namedArguments (Call function arguments isource) = 
    case findName modules currentModule namedArguments function of
      ([],[],(foundParentModule,(Define foundName foundArguments foundBody foundISource))) ->
          if length arguments == length foundArguments
          then let possibleArguments = map (evalExpr modules currentModule namedArguments) arguments
               in case partitionEithers possibleArguments of
                    ([],evaledArguments) -> evalExpr modules foundParentModule (zip foundArguments evaledArguments) foundBody
                    (errStrings,_) -> Left $ concat (map (("Error in " ++ function ++ " at line " ++ (show $ sourceInfoLine isource)):) errStrings)
          else Left ["Error in " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n" ++
                     "  Function " ++ function ++ " takes " ++ (show $ length foundArguments) ++ " argument(s) but was supplied with " ++ (show $ length arguments)]
      ([],value,_) -> 
          if length arguments == 0
          then Right value
          else Left ["Error in " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n" ++
                     "  Function " ++ function ++ " takes no arguments but was supplied with " ++ (show $ length arguments)]
      (errStrings,_,_) -> 
          Left (("Error in " ++ function ++ " at line " ++ (show $ sourceInfoLine isource)):errStrings)

genBitStream :: [SeqModule] -> Either [String] String
genBitStream modules = eLift concat $ eReduce $ map genModule modules
    where genModule :: SeqModule -> Either [String] String
          genModule mod = eLift concat $ 
                          eReduce $ 
                          map (evalExpr modules mod [] . instructionExpr) $ 
                          moduleInstructions mod

placeModules :: [SeqModule] -> Either [String] [SeqModule]
placeModules modules = Right $ snd $ mapAccumL placeModule 0 modules
    where placeModule :: Integer -> SeqModule -> (Integer,SeqModule)
          placeModule base mod@(Module _ _ _ _ modInstructions _) =
              (base + genericLength modInstructions,
               mod {moduleInstructions = zipWith setAddress [base..] modInstructions})

          setAddress :: Integer -> SeqInstruction -> SeqInstruction
          setAddress address instruction = 
              instruction {instructionAddress = address}

putEntryFirst :: String -> [SeqModule] -> Either [String] [SeqModule]
putEntryFirst entry modules =
    case findIndex ((entry==) . moduleName) modules of
      Just entryIndex ->
          let (beforeEntry,entry:afterEntry) = splitAt entryIndex modules
          in Right (entry:beforeEntry ++ afterEntry)
      Nothing ->
          Left ["Cannot find entry module " ++ entry]

compileProgram :: SeqProgram -> Either [String] String
compileProgram (Program entry modules) = 
    modules |-> putEntryFirst entry
            >-> placeModules
            >-> genBitStream
