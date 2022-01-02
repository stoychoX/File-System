module MyStack where

push :: a -> [a] -> [a]
push e stack = stack ++ [e]

pop :: [a] -> [a] 
pop [x]      = []
pop (x : xs) = x : pop xs

top :: [a] -> a
top [x]      = x
top (x : xs) = top xs