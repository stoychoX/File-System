module MyStack where

headMaybe :: [a] -> Maybe a
headMaybe (x : _) = Just x
headMaybe _        = Nothing

push :: a -> [a] -> [a]
push e stack = stack ++ [e]

pop :: [a] -> [a] 
pop [_]      = []
pop (x : xs) = x : pop xs
pop _        = error "this should never happen"

top :: [a] -> a
top [x]      = x
top (_ : xs) = top xs
top _        = error "this sholud never happen"