module Compiler
    (compileProgram) where

import Data.List (find)
import Data.Maybe (mapMaybe)

import Utils (imaybe,zipmap)
import Reports (repNoEntry,repNoExports,repNoImportModule)
import ResErr (ResErr(..),(|->),(>->),reReduce)
import Defines (SeqDesc(..),SeqProgram(..),SeqModule(..),SeqInstruction(..),defineName)

compileProgram :: SeqDesc -> SeqProgram -> [SeqModule] -> ResErr String
compileProgram desc program builtins =
    program |-> preCheck
            >-> putEntryFirst
            >-> addBuiltins builtins
            >-> placeModules
            >-> genBitStream
            >-> postCheck desc
            >-> genOutStream desc

preCheck :: SeqProgram -> ResErr SeqProgram
preCheck program =
    program |-> checkEntry
            >-> checkExports
            >-> checkImportModules
            >-> checkImports
    where checkEntry :: SeqProgram -> ResErr SeqProgram
          checkEntry program@(Program entry modules) = 
              case find ((entry==) . moduleName) modules of
                Just mod -> Res program
                Nothing -> Err [repNoEntry entry]

          checkExports :: SeqProgram -> ResErr SeqProgram
          checkExports program@(Program entry modules) =
              case zipmap moduleName checkExport modules of
                [] -> Res program
                trouble -> Err $ map repNoExports $ filter ((/=[]) . snd) trouble
              where checkExport :: SeqModule -> [String]
                    checkExport (Module name exports _ defines _ _) =
                        mapMaybe (\x -> imaybe x $ find (==x) $ map defineName defines) exports

          checkImportModules :: SeqProgram -> ResErr SeqProgram
          checkImportModules program@(Program entry modules) =
              case zipmap moduleName checkImportModule modules of
                [] -> Res program
                trouble -> Err $ map repNoImportModule $ filter ((/=[]) . snd) trouble
              where checkImportModule :: SeqModule -> [String]
                    checkImportModule (Module name _ imports _ _ _) =
                        mapMaybe (\x -> imaybe x $ find (==x) $ map moduleName modules) $ map fst imports

          checkImports :: SeqProgram -> ResErr SeqProgram
          checkImports program@(Program entry modules) = Res program
              where checkImport :: SeqModule -> [(String,[String])]
                    checkImport (Module name _ imports _ _ _) = []
                        where checkImportFromModule :: SeqModule -> 

putEntryFirst :: SeqProgram -> ResErr SeqProgram
putEntryFirst program@(Program entry modules) = Res program

addBuiltins :: [SeqModule] -> SeqProgram -> ResErr SeqProgram
addBuiltins builtins program@(Program entry modules) = Res program

placeModules :: SeqProgram -> ResErr SeqProgram
placeModules program@(Program entry modules) = Res program

genBitStream :: SeqProgram -> ResErr [(String,SeqInstruction)]
genBitStream program@(Program entry modules) = Res []

postCheck :: SeqDesc -> [(String,SeqInstruction)] -> ResErr [(String,SeqInstruction)]
postCheck desc bitstream = Res []

genOutStream :: SeqDesc -> [(String,SeqInstruction)] -> ResErr String
genOutStream desc bitstream = Res ""
