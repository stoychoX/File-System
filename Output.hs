module Output where

import FileSystem(FileSystem(..))

-- Used for show file command
printFile :: FileSystem -> String
printFile (File name content) = "File name: " ++ name ++ "\nContent: \n" ++ content ++ "\n"
printFile _                   = "Error, not rigth type of file"

-- Used for printing path at pwd cmd
printSystem :: [FileSystem] -> String
printSystem ((Root "/" _) : xs) = "/" ++ printSystem xs
printSystem ((Root n _) : xs)   = n ++ "/" ++ printSystem xs
printSystem _                   = ""

-- Used for printing files/forders at ls cmd
printEntity :: FileSystem -> String 
printEntity (Root n _) = "Root " ++ n ++ "\n"
printEntity (File n _) = "File " ++ n ++ "\n"
