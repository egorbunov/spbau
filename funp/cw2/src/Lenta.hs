module Lenta where

import Data.Maybe as M
import Data.List as L
import Text.HTML.TagSoup as TS
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as C

-- Реализовать функцию которая выводит на экран нынешний курс доллара с ленты (lenta.ru)
-- Использовать монады если они подходят.

url = "http://lenta.ru/parts/currency/usd"
tag = "div"
attr = "b-currency-rate-today__rate js-currency-today-rate-usd"

lenta :: IO ()
lenta = do
	str <- simpleHttp url
	let tags = TS.parseTags (str)
	let x = find tagMathcer tags where
					tagMathcer (TS.TagOpen t xs) = (t == tag) && elem ("class","\\\"" ++ attr ++ "\\\"") xs
					tagMathcer _ = False
	let mi = case x of (Just y) -> (L.elemIndex y tags)
	                   Nothing -> Nothing
	--print mi
	let printUSDRate Nothing = print "!!!!" 
	let printUSDRate (Just i) = print (tags !! (i))
	--printUSDRate mi
	print tags
