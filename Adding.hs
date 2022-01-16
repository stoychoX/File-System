module Adding where

import FileSystem (FileSystem(..))
import ChangingDirections ( changeDir, changeEntity )
import Parser (getNextDir)

add :: String -> FileSystem -> Maybe FileSystem -> Maybe FileSystem
add path toAdd (Just old@(Root n xs)) = case getNextDir path of 
    Just ("", "")     -> Just (Root n (toAdd : xs))
    Just (rest, curr) -> case changeDir curr xs of 
        Nothing -> Nothing 
        Just dir@(Root path' files) -> case add rest toAdd (Just dir) of
            Nothing -> Nothing
            Just new@(Root dir xs'') -> Just (changeEntity new old)
    Nothing -> Nothing 
add _ _ _ = Nothing 

addFile :: String -> String -> String -> Maybe FileSystem -> Maybe FileSystem
addFile path name val = add path (File name val)

addFolder :: String -> String -> Maybe FileSystem -> Maybe FileSystem
addFolder path name = add path (Root name []) 