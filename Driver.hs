module Driver where 

import FileOps(FileSystem(..), changeDir, mySystem, printSystem, printEntity, addFolder, changeEntity, listMaybe, dirs)
import Data.Char(toLower)
import Parser(parseCmd, getNextDir)
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

eval :: String -> [FileSystem] -> Maybe [FileSystem]
eval input syst = case parseCmd input of
    Nothing -> Nothing 
    Just (rest, curr) -> case curr of 
        "cd" -> cd ("/" ++ rest) syst
        "mkdir" -> let pathsAndSystems = zip syst (dirs syst) in
           listMaybe $ map (\x -> addFolder (snd x) rest $ Just $ fst x) pathsAndSystems                
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