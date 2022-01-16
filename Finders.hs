module Finders where 

import FileSystem(FileSystem(..))
import Predicates (isNameOfFolder, isNameOfFile)
import Parser (getNextDir)

findFile :: String -> [FileSystem] -> Maybe FileSystem
findFile name xs = 
    case filter (isNameOfFile name) xs of 
        [] -> Nothing
        (x : _) -> Just x

findFolder :: String -> FileSystem -> Maybe FileSystem
findFolder name (Root _ xs) = 
    case filter (isNameOfFolder name) xs of 
        [] -> Nothing
        (x : _) -> Just x
findFolder _ _ = Nothing

findFileInRoot :: String -> FileSystem -> Maybe FileSystem
findFileInRoot name (Root _ xs) = findFile name xs
findFileInRoot _ _ = Nothing 

findFileByDir :: String -> FileSystem -> Maybe FileSystem
findFileByDir input x@(Root _ xs) = 
    case getNextDir input of
        Just ("", file) -> findFile file xs
        Just (rest, curr) -> case findFolder curr x of 
            Nothing -> Nothing 
            Just cFolder -> findFileByDir rest cFolder
findFileByDir _ _ = Nothing 