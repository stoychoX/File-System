module FileOps where

import Parser(getNextDir)

data FileSystem =
     File String String     | -- File <name> <Value>, only support .txt
     Root String [FileSystem] -- Root <name> [<content>]
     deriving(Show)

isNameOfFolder :: String -> FileSystem -> Bool 
isNameOfFolder name (Root name' _) = name == name'
isNameOfFolder _ _                 = False

isNameOfFile :: String -> FileSystem -> Bool 
isNameOfFile name (File name' _) = name == name'
isNameOfFile _ _                 = False

headMaybe :: [a] -> Maybe a
headMaybe (x : xs) = Just x
headMaybe _        = Nothing

-- Used for show file command
findFile :: String -> [FileSystem] -> Maybe FileSystem
findFile _ [] = Nothing 
findFile name (f@(File n _) : xs) 
    | n == name = Just f
    | otherwise = findFile name xs
findFile name (_ : xs) = findFile name xs

-- Used for show file command
printFile :: FileSystem -> String
printFile (File name content) = "File name: " ++ name ++ "\nContent: \n" ++ content ++ "\n"
printFile _                   = "Error, not rigth type of file"

-- Used for validation at mkdir and mkfile
validName :: (String -> FileSystem -> Bool) -> String -> FileSystem -> Bool 
validName f name (Root n xs) = n /= name && foldr (\x r -> f name x || r) False xs
validName _ _ _              = True

-- Returns dir found by name if such exists
changeDir :: String -> [FileSystem] -> Maybe FileSystem 
changeDir name xs = headMaybe (filter (isNameOfFolder name) xs)

-- Change Root found by name if such exits.
changeEntity :: FileSystem -> FileSystem -> FileSystem
changeEntity new old@(Root n xs) = Root n (changeEntityDeep new xs) 
    where changeEntityDeep :: FileSystem -> [FileSystem] -> [FileSystem]
          changeEntityDeep new@(Root n xs) (old@(Root n' xss) : xs')
           | n == n'   = new : xs'
           | otherwise = Root n' (changeEntityDeep new xss) : changeEntityDeep new xs'
          changeEntityDeep _ x = x

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

-- Used for adding files at mkfile cmd
addFile :: String -> String -> String -> Maybe FileSystem -> Maybe FileSystem
addFile path name val = add path (File name val)

-- Used for adding folders at mkdir cmd
addFolder :: String -> String -> Maybe FileSystem -> Maybe FileSystem
addFolder path name = add path (Root name []) 

removeFileFromRoot :: String -> FileSystem -> FileSystem
removeFileFromRoot name (Root n xs) = Root n (removeFile' xs)
    where 
         removeFile' :: [FileSystem] -> [FileSystem]
         removeFile' (x@(File name' _) : xs')
          | name == name' = xs'
          | otherwise     = x : removeFile' xs'
         removeFile' (x : xs')   = x : removeFile' xs'
         removeFile' []          = [] 

-- Used for printing path at pwd cmd
printSystem :: [FileSystem] -> String
printSystem ((Root "/" _) : xs) = "/" ++ printSystem xs
printSystem ((Root n _) : xs)   = n ++ "/" ++ printSystem xs
printSystem _                   = ""

-- Used for printing files/forders at ls cmd
printEntity :: FileSystem -> String 
printEntity (Root n _) = "Root " ++ n ++ "\n"
printEntity (File n _) = "File " ++ n ++ "\n"

-- Used for validation at mkdir cmd
listMaybe :: [Maybe a] -> Maybe [a]
listMaybe [] = Just []
listMaybe ((Just x) : xs) = 
    case listMaybe xs of 
        (Just res) -> Just (x : res)
        Nothing -> Nothing 
listMaybe _ = Nothing

-- Used for generating directions (and passing them to addFolder function)
-- at mkdir cmd
dirs :: [FileSystem] -> [String]
dirs (Root n _ : xs') = up xs' : dirs xs'
 where up :: [FileSystem] -> String 
       up (Root "/" _ : xss) = "/" ++ up xss
       up (Root n _ : xss) = "/" ++ n ++ up xss
       up _ = "" 
dirs _ = []

mySystem :: FileSystem
mySystem = Root "/" [Root "dir1" [Root "dir2" [Root "dir3" [File "No print" ""], Root "dir4" [Root "dir5" []]]], Root "dir2.2" [Root "dir3.2" []]]