module ChangingDirections where
import FileSystem (FileSystem(..))
import MyStack (headMaybe)
import Predicates (isNameOfFolder)

changeDir :: String -> [FileSystem] -> Maybe FileSystem 
changeDir name xs = headMaybe $ filter (isNameOfFolder name) xs

changeEntity :: FileSystem -> FileSystem -> FileSystem
changeEntity new old@(Root n xs) = Root n $ changeEntityDeep new xs
    where 
        changeEntityDeep :: FileSystem -> [FileSystem] -> [FileSystem]
        changeEntityDeep new@(Root n xs) (old@(Root n' xss) : xs')
           | n == n'   = new : xs'
           | otherwise = Root n' (changeEntityDeep new xss) : changeEntityDeep new xs'
        changeEntityDeep new (old : xs') = old : changeEntityDeep new xs'
        changeEntityDeep _ x = x
