module Driver where 

import FileOps(FileSystem(..), changeDir, mySystem, printSystem, findFile,
 printEntity, validName, addFolder, changeEntity, listMaybe, dirs, addFile, isNameOfFolder, isNameOfFile, printFile)
import Data.Char(toLower)
import Parser(parseCmd, getNextDir, wordParser, eofParser)
import MyStack(push, pop, top)

cd :: String -> [FileSystem] -> Maybe [FileSystem]
cd input xs = case getNextDir input of
    Just ("", "") -> Just xs
    Just (rest, "..") -> case pop xs of 
        []   -> Nothing 
        back -> cd rest back
    Just (rest, curr) -> case changeDir curr xs' of
        Just res -> cd rest (push res xs)
        Nothing  -> Nothing
    Nothing -> Nothing 
 where syst@(Root name xs') = top xs

-- Supports: multiple folder additions: mkdir <folder1> <folder2> ... <folder n>
-- Checks for duplicate files, can't add root to the same dir twice..
mkdir :: String -> [FileSystem] -> Maybe [FileSystem]
mkdir input syst = case wordParser input of 
    Just ("", last)     -> case makeDir last syst of 
        Nothing      -> Just syst
        (Just res)   -> Just res
    Just (rest', curr') -> case makeDir curr' syst of
        Nothing      -> mkdir rest' syst
        (Just syst') -> mkdir rest' syst'
 where
     makeDir :: String -> [FileSystem] -> Maybe [FileSystem]
     makeDir cInput cSystem = if validName isNameOfFolder cInput (top cSystem) then Nothing 
     else let paths = zip cSystem (dirs cSystem) in 
            listMaybe $ map (\(f, s) -> addFolder s cInput (Just f)) paths 

-- Suports: Validation of names, no multiple file additions bc I think it's useless
mkFile :: String -> [FileSystem] -> Maybe [FileSystem]
mkFile input syst = case wordParser input of 
    Just (rest', fileName) -> case eofParser rest' of
        Just(_, fileContent) -> if validName isNameOfFile fileName (top syst) then Nothing 
        else let paths = zip syst (dirs syst) in
            listMaybe $ map (\(f, s) -> addFile s fileName fileContent (Just f)) paths
        Nothing -> Nothing 
    Nothing -> Nothing

eval :: String -> [FileSystem] -> Maybe [FileSystem]
eval input syst = case parseCmd input of
    Nothing -> Nothing 
    Just (rest, curr) -> case curr of 
        "cd"     -> cd ("/" ++ rest) syst
        "mkdir"  -> mkdir rest syst
        "mkfile" -> mkFile rest syst
        _ -> Nothing

ls :: String -> Maybe FileSystem -> String 
ls input (Just syst) = case eval ("cd" ++ input) [syst] of
    (Just res) -> case top res of 
        (Root _ xs) -> concatMap printEntity xs
        _           -> ""
    _          -> ""
ls _ _ = ""

pwd :: [FileSystem] -> IO() 
pwd xs = let system = printSystem xs in 
    putStrLn $ "Path\n" ++ replicate (length system) '-' ++ "\n" ++ system ++ "\n"

showFile :: String -> FileSystem -> String 
showFile name (Root _ xs) = case findFile name xs of
    Nothing    -> "No such file\n"
    (Just res) -> printFile res
showFile _ _ = "Bad use of function showFile\n"

repl :: [FileSystem] -> IO()
repl xs = do
    putStr $ printSystem xs ++ "> "
    input <- getLine
    case eval input xs of
        Nothing -> case parseCmd input of
            Just (_, ":q")  -> putStrLn "Exit..."
            Just (_, "pwd") -> do pwd xs
                                  repl xs
            Just (rest, "ls") -> case rest of 
                ('/' : path)  -> do putStrLn $ ls path (Just (head xs))
                                    repl xs
                _             -> do putStrLn $ ls rest (Just (top xs))
                                    repl xs
            Just (l, "show") -> do putStrLn (showFile l (top xs))
                                   repl xs
            _  -> repl xs
        Just res -> repl res

main :: IO()
main = repl [mySystem]