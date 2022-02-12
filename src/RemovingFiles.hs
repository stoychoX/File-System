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
removeFileFromPath inp root@(Root _ _) = 
    if isFilePath inp then 
    changeEntity (removeFileFromPathHelper inp root) root else removeFileFromRoot inp root
 where
  removeFileFromPathHelper :: String -> FileSystem -> FileSystem
  removeFileFromPathHelper input r@(Root _ xs) = case getNextDir input of 
      Just ("", fileName)      -> removeFileFromRoot fileName r
      Just(nextPath, currPath) -> case changeDir currPath xs of 
          Nothing   -> r 
          Just next -> removeFileFromPathHelper nextPath next
      Nothing                  -> error "this should never happen"
  removeFileFromPathHelper _ f = f -- this should never happen
removeFileFromPath _ f = f         -- this should never happen