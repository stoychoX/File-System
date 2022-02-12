module ChangingDirections where
import FileSystem (FileSystem(..))
import MyStack (headMaybe)
import Predicates (isNameOfFolder)
import ExtendedOperations ( nameOfRoot )

changeDir :: String -> [FileSystem] -> Maybe FileSystem 
changeDir name xs = headMaybe $ filter (isNameOfFolder name) xs

changeEntity :: FileSystem -> FileSystem -> FileSystem
changeEntity new old@(Root n xs) = 
    case nameOfRoot new of 
        Nothing    -> old
        Just name' -> if name' == n then new else Root n $ changeEntityDeep new xs
    where 
        changeEntityDeep :: FileSystem -> [FileSystem] -> [FileSystem]
        changeEntityDeep deepNew@(Root name _) ((Root n' xss) : xs')
           | name == n'   = deepNew : xs'
           | otherwise    = Root n' (changeEntityDeep deepNew xss) : changeEntityDeep deepNew xs'
        changeEntityDeep new' (old' : xs') = old' : changeEntityDeep new' xs'
        changeEntityDeep _ x = x
changeEntity _ old = old
