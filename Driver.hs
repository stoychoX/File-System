module Driver where 

import FileOps(FileSystem(..), changeDir, mySystem, printSystem, printEntity, addFolder, changeEntity, listMaybe, dirs, badNameOfFolder)
import Data.Char(toLower)
import Parser(parseCmd, getNextDir, wordParser)
import MyStack(push, pop, top)

cd :: String -> [FileSystem] -> Maybe [FileSystem]
cd input xs = case getNextDir input of
    Just ("", "") -> Just xs
    Just (rest, "..") -> case pop xs of 
        [] -> Nothing 
        back -> cd rest back
    Just (rest, curr) -> case changeDir curr xs' of
        Just res -> cd rest (push res xs)
        _ -> Nothing
    _ -> Nothing 
 where syst@(Root name xs') = top xs

-- Supports: mulriple folder additions: mkdir <folder1> <folder2> ... <folder n>
-- Checks for duplicate files, can't add root to the same dir twice..
mkdir :: String -> [FileSystem] -> Maybe [FileSystem]
mkdir input syst = case wordParser input of 
    Just ("", last) -> case makeDir last syst of 
        Nothing -> Just syst
        (Just res) -> Just res
    Just (rest', curr') -> case makeDir curr' syst of
        Nothing -> mkdir rest' syst
        (Just syst') -> mkdir rest' syst'
 where
     makeDir :: String -> [FileSystem] -> Maybe [FileSystem]
     makeDir cInput cSystem = if badNameOfFolder cInput (top cSystem) then Nothing 
     else let paths = zip cSystem (dirs cSystem) in 
            listMaybe $ map (\(f, s) -> addFolder s cInput (Just f)) paths 

eval :: String -> [FileSystem] -> Maybe [FileSystem]
eval input syst = case parseCmd input of
    Nothing -> Nothing 
    Just (rest, curr) -> case curr of 
        "cd" -> cd ("/" ++ rest) syst
        "mkdir" -> mkdir rest syst 
        "mkfile" -> undefined    
        _ -> Nothing

ls :: String -> Maybe FileSystem -> String 
ls input (Just syst) = case eval ("cd" ++ input) [syst] of
    (Just res) -> case top res of 
        (Root _ xs) -> concatMap printEntity xs
        _ -> ""
    _ -> ""
ls _ _ = ""

pwd :: [FileSystem] -> IO() 
pwd xs = let system = printSystem xs in 
    putStrLn ("Path\n" ++ replicate (length system) '-' ++ "\n" ++ system ++ "\n")

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
            _ -> repl xs
        Just res -> repl res

main :: IO()
main = repl [mySystem]