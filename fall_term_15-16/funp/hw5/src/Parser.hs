-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTestOK
    , parserTestFail
    ) where

-- Список экспорта менять нельзя

import Control.Applicative
import Test.HUnit
import Data.Foldable(toList)

type Error = Either String -- Можно заменить на Maybe, если есть желание.
newtype Parser lex a = Parser { runParser :: [lex] -> Error (a, [lex]) }

-- 1. (0.5 балла)
evalParser :: Parser lex a -> [lex] -> Error a
evalParser parser xs = 
    case runParser parser xs of
        Right (res, _) -> Right res
        Left s -> Left s 

-- 2. (0.5 балла)
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy p = Parser $ satisfy'
    where satisfy' [] = Left "error: empty input"
          satisfy' (x:xs) = if p x then Right (x, xs) else Left "error: no satisfaction"

-- 3. (0.5 балла)
eof :: Parser lex ()
eof = Parser $ eof'
    where eof' [] = Right ((), [])
          eof' _ = Left "error: not empty"

instance Functor (Parser lex) where
    fmap f (Parser parse) = Parser $ \s -> f' (parse s) where
        f' (Left msg) = Left msg
        f' (Right (res, xs)) = Right (f res, xs)

-- 4. (1.5 балла)
instance Applicative (Parser lex) where
    pure x = Parser $ \ls -> Right (x, ls)
    (Parser pf) <*> parser = Parser $ \ls -> helper (pf ls) where
        helper (Left msg) = Left msg
        helper (Right (f, xs)) = runParser (fmap f parser) xs

-- 5. (1 балла)
instance Alternative (Parser lex) where
    empty = Parser $ \_ -> Left "empty"
    (Parser p1) <|> (Parser p2) = Parser $ \ls -> helper (p1 ls) ls where
        helper (Left msg) ls = p2 ls
        helper r _ = r

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ null $ toList (p s)
