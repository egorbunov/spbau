module Lenta where
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as C
import Text.HTML.TagSoup
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..))

-- Реализовать функцию которая выводит на экран нынешний курс доллара с ленты (lenta.ru)
-- Использовать монады если они подходят.

url :: String
url = "http://lenta.ru/parts/currency/usd"

chromeUserAgent = "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

lenta :: IO ()
lenta = do
    manager <- newManager tlsManagerSettings
    initReq <- parseUrl url
    let request = initReq
                  { requestHeaders = [("User-Agent", chromeUserAgent)]
                  }
    response <- httpLbs request manager
    C.hPutStrLn h $ responseBody response
        
