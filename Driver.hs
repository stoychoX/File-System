module Driver where 

import FileSystem(FileSystem(..))
import Data.Char(toLower)
import Parser(parseCmd, getNextDir, wordParser, eofParser)
import MyStack(push, pop, top)
import ChangingDirections (changeDir, changeEntity)
import Predicates
    ( isNameOfFolder, isNameOfFile, isFilePath, validName )
import Adding (addFile, addFolder)
import RemovingFiles (removeFileFromRoot, removeFileFromPath)
import ExtendedOperations (listMaybe, dirs, catFiles)
import Finders (findFile, findFileInRoot, findFileByDir)
import Output (printFile, printSystem, printEntity)

cd :: String -> [FileSystem] -> Maybe [FileSystem]
cd input xs = case getNextDir input of
    Just ("", "")     -> Just xs
    Just (rest, "..") -> case pop xs of 
        []   -> Nothing 
        back -> cd rest back
    Just (rest, curr) -> case changeDir curr xs' of
        Just res -> cd rest (push res xs)
        Nothing  -> Nothing
    Nothing           -> Nothing 
 where syst@(Root name xs') = top xs

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

mkFile :: String -> [FileSystem] -> Maybe [FileSystem]
mkFile input syst = case wordParser input of 
    Just (rest', fileName) -> case eofParser rest' of
        Just(_, fileContent) -> if validName isNameOfFile fileName (top syst) then Nothing 
        else let paths = zip syst (dirs syst) in
            listMaybe $ map (\(f, s) -> addFile s fileName fileContent (Just f)) paths
        Nothing -> Nothing 
    Nothing -> Nothing

cat :: String -> [FileSystem] -> [FileSystem]
cat = catHelper (File "" "") 
    where
        catHelper :: FileSystem -> String -> [FileSystem] -> [FileSystem] 
        catHelper currFile input syst = 
            case wordParser input of
                Just (fileName, ">") -> case catFiles fileName (File "" "") currFile of
                    Nothing -> syst 
                    Just (File n' c') -> case mkFile (n' ++ " " ++ c' ++ "~") syst of
                        Nothing -> syst
                        Just res -> res
                Just ("", "") -> syst
                Just(rest, curr) -> if isFilePath curr then 
                    case findFileByDir curr (head syst) of
                        Nothing -> catHelper currFile rest syst
                        Just file -> case catFiles "" file currFile of
                            Nothing -> catHelper currFile rest syst
                            Just resFile -> catHelper resFile rest syst
                    else 
                        case findFileInRoot curr (top syst) of 
                            Nothing -> catHelper currFile rest syst
                            Just file -> case catFiles "" file currFile of
                                Nothing -> catHelper currFile rest syst
                                Just resFile -> catHelper resFile rest syst
                Nothing -> syst 

eval :: String -> [FileSystem] -> Maybe [FileSystem]
eval input syst = case parseCmd input of
    Nothing -> Nothing 
    Just (rest, curr) -> case curr of 
        "cd"     -> cd ("/" ++ rest) syst
        "mkdir"  -> mkdir rest syst
        "mkfile" -> mkFile rest syst
        "rm"     -> Just $ removeFile rest syst
        "cat"    -> Just $ cat rest syst
        _        -> Nothing

ls :: String -> Maybe FileSystem -> String 
ls input (Just syst) = case eval ("cd" ++ input) [syst] of
    (Just res) -> case top res of 
        (Root _ xs) -> concatMap printEntity xs
        _           -> ""
    _          -> ""
ls _ _ = ""

pwd :: [FileSystem] -> IO() 
pwd xs = let system = printSystem xs in 
    putStr $ "Path\n" ++ replicate (length system) '-' ++ "\n" ++ system ++ "\n"

showFile :: String -> FileSystem -> String 
showFile name (Root _ xs) = case findFile name xs of
    Nothing    -> "No such file\n"
    (Just res) -> printFile res
showFile _ _ = "Bad use of function showFile\n"

removeFile :: String -> [FileSystem] -> [FileSystem]
removeFile _ [] = [] -- this should never happen
removeFile input xs = 
    case wordParser input of
        Just ("", last) -> let paths = zip xs (map (\x -> x ++ "/" ++ last) (dirs xs)) in
            map (\(f, s) -> removeFileFromPath s f) paths 
        Just (rest, curr) -> let paths = zip xs (map (\x -> x ++ "/" ++ curr) (dirs xs)) in 
            removeFile rest $ map (\(f, s) -> removeFileFromPath s f) paths
        Nothing -> xs

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
                ('/' : path)  -> do putStr $ ls path $ Just $ head xs
                                    repl xs
                _             -> do putStr $ ls rest $ Just $ top xs
                                    repl xs
            Just (l, "show") -> do putStr $ showFile l $ top xs
                                   repl xs
            _  -> repl xs
        Just res -> repl res

main :: IO()
main = repl [mySystem]

mySystem :: FileSystem
mySystem = Root "/" [File "name" "",File "nameOne" "", Root "dir1" [Root "dir2" [File "" "", Root "dir3" [File "No print" ""], Root "dir4" [Root "dir5" []]]], Root "dir2.2" [Root "dir3.2" []]]