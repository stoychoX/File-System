module RemovingFiles where
import FileSystem (FileSystem(..))
import Parser (getNextDir)
import ChangingDirections (changeDir, changeEntity)
import Predicates(isFilePath)

removeFileFromRoot :: String -> FileSystem -> FileSystem
removeFileFromRoot name (Root n xs) = Root n (removeFile' xs)
    where 
         removeFile' :: [FileSystem] -> [FileSystem]
         removeFile' (x@(File name' _) : xs')
          | name == name' = xs'
          | otherwise     = x : removeFile' xs'
         removeFile' (x : xs')   = x : removeFile' xs'
         removeFile' []          = [] 
removeFileFromRoot _ x             = x

removeFileFromPath :: String -> FileSystem -> FileSystem 
removeFileFromPath input r@(Root _ _) = 
    if isFilePath input then 
    changeEntity (removeFileFromPathHelper input r) r else removeFileFromRoot input r
 where
  removeFileFromPathHelper :: String -> FileSystem -> FileSystem
  removeFileFromPathHelper input r@(Root name xs) = case getNextDir input of 
      Just ("", fileName)      -> removeFileFromRoot fileName r
      Just(nextPath, currPath) -> case changeDir currPath xs of 
          Nothing   -> r 
          Just next -> removeFileFromPathHelper nextPath next
  removeFileFromPathHelper _ f = f -- this should never happen
removeFileFromPath _ f = f         -- this should never happen