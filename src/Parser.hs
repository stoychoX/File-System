module Parser where
import Control.Applicative ( Alternative((<|>), empty) )
import Data.Char (isSpace)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
         (input', x) <- p input
         Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \x' -> Just (x', x)
    (Parser p) <*> (Parser p') =
        Parser $ \x -> do
         (x', f) <- p x
         (x'', a) <- p' x'
         Just (x'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p) <|> (Parser p') = Parser $ \x -> p x <|> p' x

charP :: Char -> Parser Char
charP x = Parser go
 where
     go (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
     go [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input in Just (rest, token)

ws :: Parser String 
ws = spanP isSpace

slashParser :: Parser String
slashParser = charP '/' *> spanP (/='/')

getNextDir :: String -> Maybe (String, String)
getNextDir "" = Just ("", "")
getNextDir x = runParser slashParser x

cdParser :: Parser String
cdParser = stringP "cd"

pwdParser :: Parser String 
pwdParser = stringP "pwd"

lsParser :: Parser String 
lsParser = stringP "ls"

showParser :: Parser String 
showParser = stringP "show" 

catParser :: Parser String 
catParser = stringP "cat"

quitParser :: Parser String 
quitParser = stringP ":q"

rmParser :: Parser String 
rmParser = stringP "rm"

dirParser :: Parser String 
dirParser = stringP "mkdir" <|> stringP "mkfile"

wordParser :: String -> Maybe (String, String)
wordParser = runParser $ spanP (/= ' ') <* ws

eofParser :: String -> Maybe (String, String) 
eofParser = runParser $ spanP (/= '~') <* charP '~' <* ws

cmdParser :: Parser String 
cmdParser = cdParser <|> pwdParser <|> lsParser <|> catParser <|> rmParser <|> quitParser <|> dirParser <|> showParser

parseCmd :: String -> Maybe (String, String)
parseCmd = runParser (cmdParser <* ws)