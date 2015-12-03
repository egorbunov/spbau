module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- 1. (0.5 балла)
symbol :: (Eq lex) => lex -> Parser lex ()
symbol c = (\x->()) <$> satisfy (\x -> x == c)

-- 2. (0.5 балла)
anySymbol :: Parser lex lex
anySymbol = satisfy $ \x->True 

-- 3. (0.5 балла)
digit :: Parser Char Int
digit = digitToInt <$> satisfy (\x -> elem x $ take 9 ['0'..])

-- 4. (0.5 балла)
string :: (Eq lex) => [lex] -> Parser lex ()
string [] = pure ()
string (x:xs) = pure const <*> symbol x <*> string xs

-- 5. (0.5 балла)
oneOf :: (Eq lex) => [lex] -> Parser lex lex
oneOf xs = satisfy (\x -> elem x xs)

-- 6. (0.5 балла)
many :: Parser lex a -> Parser lex [a]
many p = many1 p <|> pure []

-- 7. (0.5 балла)
many1 :: Parser lex a -> Parser lex [a]
many1 p = (:) <$> p <*> (many p)

-- 8. (0.5 балла)
natural :: Parser Char Integer
natural = read <$> many1 (satisfy (\x -> elem x $ take 9 ['0'..]))

-- 9. (0.5 балла)
integer :: Parser Char Integer
integer = natural <|> ((\_->negate) <$> symbol '-' <*> natural)

-- 10. (0.5 балла)
spaces :: Parser Char ()
spaces = (\x->()) <$> many (symbol ' ')

-- 11. (0.5 балла)
try :: Parser lex a -> Parser lex (Maybe a)
try parser = Just <$> parser <|> pure Nothing

-- 12. (0.5 балла)
endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy p1 p2 = many $ pure const <*> p1 <*> p2  

-- 13. (0.5 балла)
endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1  p1 p2 = many1 $ pure const <*> p1 <*> p2  

-- 14. (0.5 балла)
sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy p1 p2 = sepBy1 p1 p2 <|> pure []

-- 15. (0.5 балла)
sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 p1 p2 = pure (:) <*> p1 <*> (many (pure (\x y-> y) <*> p2 <*> p1))

-- 16. (0.1 балла)
between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between l r m = pure const <*> (pure (\x y-> y) <*> l <*> m) <*> r

-- 17. (0.1 балла)
brackets :: Parser Char a -> Parser Char a
brackets = between (symbol '[') (symbol ']')

-- 18. (0.1 балла)
parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')

-- 19. (0.1 балла)
braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')

-- 20. (0.1 балла)
angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')

-- 21. (1 балл)
foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P = undefined

-- 22. (1 балл)
foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P = undefined
