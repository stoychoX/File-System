-- import Control.Applicative
-- import Data.Char (isDigit, isSpace)

-- data Val = 
--  Null                 |
--  BoolVal       Bool   |
--  IntegerVal    Integer|
--  StringVal     String |
--  ArrayVal      [Val]
--  deriving (Show, Eq)

--  -- Just type synonim
-- newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

-- instance Functor Parser where 
--     fmap f (Parser p) = Parser $ \input -> do 
--          (input', x) <- p input 
--          Just (input', f x)
 
--  -- Can't do Applicative without a functor..
-- instance Applicative Parser where
--     pure x = Parser $ \x' -> Just (x', x)
--     (Parser p) <*> (Parser p') = 
--         Parser $ \x -> do 
--          (x', f) <- p x
--          (x'', a) <- p' x'
--          Just (x'', f a) 

-- instance Alternative Parser where
--     empty = Parser $ const Nothing 
--     (Parser p) <|> (Parser p') = Parser $ \x -> p x <|> p' x

-- -- We need to be able to parse a single character...
-- charParser :: Char -> Parser Char
-- charParser x = Parser go
--  where 
--      go :: String -> Maybe (String, Char)
--      go (y : ys) 
--       | y == x = Just (ys, x)
--       | otherwise = Nothing 
--      go _ = Nothing 
 
-- stringParser :: String -> Parser String 
-- stringParser = traverse charParser

-- --Cant use traverse for non-applicative types...
-- nullParser :: Parser Val
-- nullParser = Null <$ stringParser "null" 
 
-- boolParser :: Parser Val 
-- boolParser = go <$> (stringParser "true" <|> stringParser "false")
--     where 
--         go :: String -> Val
--         go "true" = BoolVal True 
--         go _ = BoolVal False
 
-- dig :: Char -> Bool 
-- dig x = '0' <= x && x <= '9'

-- parseWhile :: (Char -> Bool) -> Parser String
-- parseWhile f = Parser $ \input ->
--  let (token, rest) = span f input
--   in Just (rest, token)
 
-- notNull :: Parser [a] -> Parser [a] 
-- notNull (Parser p) = 
--     Parser $ \x -> do
--         (input, xs ) <- p x
--         if null xs then Nothing else Just (input, xs)

-- numberParser :: Parser Val 
-- numberParser = go <$> notNull (parseWhile isDigit)
--  where
--      go = IntegerVal . read

-- stringLiteral' :: Parser String 
-- stringLiteral' = parseWhile (/= '"')

-- stringParser' :: Parser Val 
-- stringParser' = StringVal <$> (charParser '"' *>  stringLiteral' <* charParser '"')

-- ws :: Parser String 
-- ws = parseWhile isSpace

-- parser :: Parser Val 
-- parser = nullParser <|> stringParser' <|> numberParser <|> boolParser

-- sepBy :: Parser a -> Parser b -> Parser [b] 
-- sepBy sep element = (:) <$> element <*> many (sep *> element)  <|> pure []

-- arrayParser :: Parser Val 
-- arrayParser = ArrayVal <$> (charParser '[' *> ws *> elements <* ws <* charParser '[')
--  where 
--      elements = sepBy (ws *> charParser ',' <* ws) parser
--  -- More combinations?? runParser (boolParser <|> nullParser) <bool or null>


-- -- Info on <* and *> -> <Parser> <* <Parser> --> Does both of them but returns only the left one!!