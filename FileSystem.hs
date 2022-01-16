module FileSystem where
     
data FileSystem =
     File String String     | -- File <name> <Value>, only support .txt
     Root String [FileSystem] -- Root <name> [<content>]
     deriving(Show)