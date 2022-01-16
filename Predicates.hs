module Predicates where

import FileSystem (FileSystem(..))

isNameOfFolder :: String -> FileSystem -> Bool 
isNameOfFolder name (Root name' _) = name == name'
isNameOfFolder _ _                 = False

isNameOfFile :: String -> FileSystem -> Bool 
isNameOfFile name (File name' _) = name == name'
isNameOfFile _ _                 = False

isFilePath :: String -> Bool 
isFilePath ('/' : _) = True 
isFilePath _ = False 

validName :: (String -> FileSystem -> Bool) -> String -> FileSystem -> Bool 
validName f name (Root n xs) = n /= name && foldr (\x r -> f name x || r) False xs
validName _ _ _              = True
