module Defines
    (SeqSourceInfo(..),
     SeqExpr(..),
     SeqDefine(..),
     SeqInstruction(..),
     SeqModule(..),
     SeqProgram(..)) where

data SeqSourceInfo
    = SourceInfo {
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
        primOperPriority :: Integer}
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
        moduleImports :: [(String,[String])],
        moduleDefines :: [SeqDefine],
        moduleInstructions :: [SeqInstruction],
        moduleISource :: SeqSourceInfo}

data SeqProgram
    = Program {
        programEntry :: String,
        programModules :: [SeqModule]}
