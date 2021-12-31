module FileOps where

import Parser(getNextDir)

data FileSystem =
     File String String     | -- File <name> <Value>, only support .txt
     Root String [FileSystem] -- Root <name> [<content>]
     deriving(Show)

isNameOfFolder :: String -> FileSystem -> Bool 
isNameOfFolder name (Root name' _) = name == name'
isNameOfFolder _ _ = False

headMaybe :: [a] -> Maybe a
headMaybe (x : xs) = Just x
headMaybe _ = Nothing

-- Returns dir found by name if such exists...
changeDir :: String -> [FileSystem] -> Maybe FileSystem 
changeDir name xs = headMaybe (filter (isNameOfFolder name) xs)

-- Change Root found by name if such exits.
changeEntity :: FileSystem -> FileSystem -> FileSystem
changeEntity new old@(Root n xs) = Root n (changeEntityDeep new xs) 
    where changeEntityDeep :: FileSystem -> [FileSystem] -> [FileSystem]
          changeEntityDeep new@(Root n xs) (old@(Root n' xss) : xs')
           | n == n' = new : xs'
           | otherwise = Root n' (changeEntityDeep new xss) : changeEntityDeep new xs'
          changeEntityDeep _ x = x

add :: String -> FileSystem -> Maybe FileSystem -> Maybe FileSystem
add path toAdd (Just old@(Root n xs)) = case getNextDir path of 
    Just ("", "") -> Just (Root n (toAdd : xs))
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

-- Used for pwd
printSystem :: [FileSystem] -> String
printSystem ((Root "/" _) : xs) = "/" ++ printSystem xs
printSystem ((Root n _) : xs) = n ++ "/" ++ printSystem xs
printSystem _ = ""

-- Used for ls
printEntity :: FileSystem -> String 
printEntity (Root n _) = "Root " ++ n ++ "\n"
printEntity (File n _) = "File " ++ n ++ "\n"

-- Used for mkdir
listMaybe :: [Maybe a] -> Maybe [a]
listMaybe [] = Just []
listMaybe ((Just x) : xs) = 
    case listMaybe xs of 
        (Just res) -> Just (x : res)
        Nothing -> Nothing 
listMaybe _ = Nothing

dirs :: [FileSystem] -> [String]
dirs (Root n _ : xs') = up xs' : dirs xs'
 where up :: [FileSystem] -> String 
       up (Root "/" _ : xss) = "/" ++ up xss
       up (Root n _ : xss) = "/" ++ n ++ up xss
       up _ = "" 
dirs _ = []

mySystem :: FileSystem
mySystem = Root "/" [Root "dir1" [Root "dir2" [Root "dir3" [File "No print" ""], Root "dir4" [Root "dir5" []]]], Root "dir2.2" [Root "dir3.2" []]]