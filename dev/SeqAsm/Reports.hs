module Reports
    (repNoEntry,
     repNoExports,
     repNoImportModule) where

import Data.List (intercalate)

repNoEntry :: String -> String
repNoEntry entry = 
    "Entry module " ++ entry ++ " is not defined!"

repNoExports :: (String,[String]) -> String
repNoExports (moduleName,moduleExports) =
    "Module " ++ moduleName ++ " exports undefined name(s) : " ++ intercalate "," moduleExports

repNoImportModule :: (String,[String]) -> String
repNoImportModule (moduleName,moduleImports) =
    "Module " ++ moduleName ++ " imports undefined module(s) : " ++ intercalate "," moduleImports
