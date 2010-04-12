module Compiler
    (compileProgram) where

import Data.List (find,findIndex,mapAccumL,genericLength)
import Data.Either (partitionEithers)

import Defines (SeqSourceInfo(..),SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..))
import Utils (intToStr)

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

findName :: [SeqModule] -> SeqModule -> [(String,String)] -> String -> Either [String] (SeqModule,SeqDefine)
findName modules currentModule namedArguments name =
    case lookupArguments of
      Just value -> Right (currentModule,TempReturn value)
      Nothing ->
          case lookupInstruction of
            Just value -> Right (currentModule,TempReturn $ intToStr 2 $ instructionAddress value)
            Nothing ->
                case lookupLocal of
                  Just define -> Right (currentModule,define)
                  Nothing ->
                      case lookupImport of
                        Just (parentModule,imported) -> Right (parentModule,imported)
                        Nothing -> Left [name ++ " is not defined or not visible from module " ++ moduleName currentModule]
    where lookupArguments :: Maybe String
          lookupArguments = lookup name namedArguments

          lookupInstruction :: Maybe SeqInstruction
          lookupInstruction = find ((name==) . instructionLabel) $ moduleInstructions currentModule

          lookupLocal :: Maybe SeqDefine
          lookupLocal = find ((name==) . defineName) $ moduleDefines currentModule

          lookupImport :: Maybe (SeqModule,SeqDefine)
          lookupImport = Nothing

          defineName :: SeqDefine -> String
          defineName (UserFunc name _ _ _) = name
          defineName (PrimFunc name _ _) = name
          defineName (PrimOper name _ _ _) = name
          defineName (TempReturn _) = error "Invalid call to defineName!"

evalExpr :: [SeqModule] -> SeqModule -> [(String,String)] -> SeqExpr -> Either [String] String
evalExpr modules currentModule namedArguments (Null) = Right ""
evalExpr modules currentModule namedArguments (Numb value isource) = Right (intToStr 2 value)
evalExpr modules currentModule namedArguments (Call function arguments isource) =
    case findName modules currentModule namedArguments function of
      Right (foundParentModule,(UserFunc foundName foundArguments foundBody foundISource)) ->
          if length arguments == length foundArguments
          then case eReduce $ map (evalExpr modules currentModule namedArguments) arguments of
                 Right evaledArguments -> evalExpr modules foundParentModule (zip foundArguments evaledArguments) foundBody
                 Left errStrings -> Left $ ("Error when calling " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n"):errStrings
          else Left ["Error when calling " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n",
                     "Function " ++ function ++ " takes " ++ (show $ length foundArguments) ++ " argument(s) but was supplied with " ++ (show $ length arguments)]
      Right (foundParentModule,(PrimFunc foundName foundArguments foundBody)) ->
          if length arguments == length foundArguments
          then case eReduce $ map (evalExpr modules currentModule namedArguments) arguments of
                 Right evaledArguments -> foundBody evaledArguments
                 Left errStrings -> Left $ ("Error when calling " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n"):errStrings
          else Left ["Error when calling " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n",
                     "Function " ++ function ++ " takes " ++ (show $ length foundArguments) ++ " argument(s) but was supplied with " ++ (show $ length arguments)]
      Right (foundParentModule,(PrimOper foundName foundArguments foundBody foundPriority)) ->
          if length arguments == length foundArguments
          then case eReduce $ map (evalExpr modules currentModule namedArguments) arguments of
                 Right evaledArguments -> foundBody evaledArguments
                 Left errStrings -> Left $ ("Error when calling " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n"):errStrings
          else Left ["Error when calling " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n",
                     "Function " ++ function ++ " takes " ++ (show $ length foundArguments) ++ " argument(s) but was supplied with " ++ (show $ length arguments)]
      Right (foundParentModule,(TempReturn foundValue)) ->
          if length arguments == 0
          then Right foundValue
          else Left ["Error when calling " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n",
                     "Function " ++ function ++ " takes no arguments but was supplied with " ++ (show $ length arguments)]
      Left errStrings ->
          Left $ ("Error in " ++ function ++ " at line " ++ (show $ sourceInfoLine isource) ++ "\n"):errStrings

genBitStream :: [SeqModule] -> Either [String] String
genBitStream modules = eLift concat $ eReduce $ map genModule modules
    where genModule :: SeqModule -> Either [String] String
          genModule mod = eLift concat $ 
                          eReduce $ 
                          map (evalExpr modules mod [] . instructionExpr) $ 
                          moduleInstructions mod

insertPrimitives :: [SeqModule] -> Either [String] [SeqModule]
insertPrimitives modules = Left []

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
            >-> insertPrimitives
            >-> genBitStream
