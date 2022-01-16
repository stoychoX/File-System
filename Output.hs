module Output where

import FileSystem(FileSystem(..))

printFile :: FileSystem -> String
printFile (File name content) = "File name: " ++ name ++ "\nContent: \n" ++ content ++ "\n"
printFile _                   = "Error, not rigth type of file"

printSystem :: [FileSystem] -> String
printSystem ((Root "/" _) : xs) = "/" ++ printSystem xs
printSystem ((Root n _) : xs)   = n ++ "/" ++ printSystem xs
printSystem _                   = ""

printEntity :: FileSystem -> String 
printEntity (Root n _) = "Root " ++ n ++ "\n"
printEntity (File n _) = "File " ++ n ++ "\n"
