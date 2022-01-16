module ExtendedOperations where

import FileSystem(FileSystem(..))

listMaybe :: [Maybe a] -> Maybe [a]
listMaybe [] = Just []
listMaybe ((Just x) : xs) = 
    case listMaybe xs of 
        (Just res) -> Just (x : res)
        Nothing -> Nothing
listMaybe (Nothing : xs) = listMaybe xs

dirs :: [FileSystem] -> [String]
dirs (Root n _ : xs') = up xs' : dirs xs'
 where up :: [FileSystem] -> String 
       up (Root "/" _ : xss) = "/" ++ up xss
       up (Root n _ : xss) = "/" ++ n ++ up xss
       up _ = "" 
dirs _ = []

catFiles :: String -> FileSystem -> FileSystem -> Maybe FileSystem
catFiles newName (File _ cnt) (File _ cnt') = Just $ File newName (cnt' ++ cnt)
catFiles _ _ _                              = Nothing

nameOfRoot :: FileSystem -> Maybe String 
nameOfRoot (Root n _) = Just n
nameOfRoot _ = Nothing 
