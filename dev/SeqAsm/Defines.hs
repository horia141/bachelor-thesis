module Defines
    (SeqSourceInfo(..),
     SeqExpr(..),
     SeqDefine(..),
     SeqInstruction(..),
     SeqModule(..),
     SeqProgram(..)) where

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
        callFunction :: String,
        callArguments :: [SeqExpr],
        callISource :: SeqSourceInfo}
    deriving (Show)

data SeqDefine
    = Define {
        defineName :: String,
        defineArguments :: [String],
        defineBody :: SeqExpr,
        defineISource :: SeqSourceInfo}
    deriving (Show)

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
    deriving (Show)

data SeqProgram
    = Program {
        programEntry :: String,
        programModules :: [SeqModule]}
    deriving (Show)
