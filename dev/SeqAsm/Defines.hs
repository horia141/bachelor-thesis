module Defines
    (SeqSourceInfo(..),
     SeqExpr(..),
     SeqDefine(..),
     SeqInstruction(..),
     SeqModule(..),
     SeqProgram(..),
     SeqOpFixity(..),
     SeqOpAssoc(..),
     SeqDesc(..),
     defineName) where

data SeqSourceInfo
    = SourceStub
    | SourceInfo {
        sourceInfoName :: String,
        sourceInfoLine :: Int,
        sourceInfoColumn :: Int,
        sourceInfoSource :: String}
    deriving (Show)

data SeqExpr
    = Null
    | Numb {
        numbValue :: Integer,
        numbISource :: SeqSourceInfo}
    | Call {
        callModule :: String,
        callFunction :: String,
        callArguments :: [SeqExpr],
        callISource :: SeqSourceInfo}
    deriving (Show)

data SeqDefine
    = UserFunc {
        userFuncName :: String,
        userFuncArguments :: [String],
        userFuncBody :: SeqExpr,
        userFuncISource :: SeqSourceInfo}
    | PrimFunc {
        primFuncName :: String,
        primFuncArguments :: [String],
        primFuncBody :: [String] -> Either [String] String}
    | PrimOper {
        primOperName :: String,
        primOperArguments :: [String],
        primOperBody :: [String] -> Either [String] String,
        primOperParseRule :: (SeqOpFixity,SeqOpAssoc,Integer)}
    | TempReturn {
        tempReturnValue :: String}

data SeqInstruction
    = Instruction {
        instructionAddress :: Integer,
        instructionLabel :: String,
        instructionExpr :: SeqExpr,
        instructionISource :: SeqSourceInfo}
    deriving (Show)

data SeqModule
    = Module {
        moduleName :: String,
        moduleExports :: [String],
        moduleImports :: [(String,String,[String])],
        moduleDefines :: [SeqDefine],
        moduleInstructions :: [SeqInstruction],
        moduleISource :: SeqSourceInfo}

data SeqProgram
    = Program {
        programEntry :: String,
        programModules :: [SeqModule]}

data SeqDesc
    = Desc {
        descWordSize :: Integer,
        descAddrSize :: Integer}
    deriving (Show)

data SeqOpFixity
    = OpInfix
    | OpPostfix
    | OpPrefix
    deriving (Show)

data SeqOpAssoc
    = OpLeft
    | OpRight
    deriving (Show)

defineName :: SeqDefine -> String
defineName (UserFunc name _ _ _) = name
defineName (PrimFunc name _ _) = name
defineName (PrimOper name _ _ _) = name
defineName _ = undefined
