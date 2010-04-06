module Defines
    (SeqExpr(..),
     SeqDefine(..),
     SeqInstruction(..),
     SeqModule(..),
     SeqProgram(..)) where

data SeqExpr
    = Null
    | Numb {
        numbValue :: Integer}
    | Call {
        callFunction :: String,
        callArguments :: [SeqExpr]}
    deriving (Show)

data SeqDefine
    = Define {
        defineName :: String,
        defineArguments :: [String],
        defineBody :: SeqExpr}
    deriving (Show)

data SeqInstruction
    = Instruction {
        instructionAddress :: Integer,
        instructionLabel :: String,
        instructionExpr :: SeqExpr}
    deriving (Show)

data SeqModule
    = Module {
        moduleName :: String,
        moduleExports :: [String],
        moduleImports :: [(String,[String])],
        moduleDefines :: [SeqDefine],
        moduleInstructions :: [SeqInstruction]}
    deriving (Show)

data SeqProgram
    = Program {
        programEntry :: String,
        programModules :: [SeqModule]}
    deriving (Show)
