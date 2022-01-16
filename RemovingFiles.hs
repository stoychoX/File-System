module RemovingFiles where
import FileSystem (FileSystem(..))

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