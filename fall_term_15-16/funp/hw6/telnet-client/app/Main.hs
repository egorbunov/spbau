import Network.Socket
import Network
import System.IO
import System.Environment
import System.Exit
import qualified Text.Read as T
import Control.Monad
import Control.Concurrent(forkIO)

{-
Реализуйте простой телнет-клиент.
Это программа, которая коннектится по указанному порту и отправляет весь ввод с stdin, а ответ выводит в stdout.
(2 балла)

Пример вызова:
    ./telnet-client-exe 55667
-}

main :: IO ()
main = withSocketsDo $ do
        port <- parse
        let hints = defaultHints { addrFlags = [AI_PASSIVE] }
        addrinfos <- getAddrInfo (Just hints) Nothing (Just port)
        let serveraddr = head addrinfos
        s <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect s (addrAddress serveraddr)
        h <- socketToHandle s ReadWriteMode
        forkIO $ reader h
        writer h

parse :: IO String
parse = getArgs >>= parse' where
    parse' [] = putStrLn "error: bad arguments, no port passed" >> exitFailure
    parse' (p:as) = 
        case T.readMaybe p :: Maybe Int of 
            Nothing -> putStrLn "error: cannot parse first arg as port" >> exitFailure
            Just _  -> return p

writer :: Handle -> IO ()
writer h = forever $ getLine >>= hPutStrLn h . ((++) "FROM CLIENT: ")

reader :: Handle -> IO ()
reader h = forever $ hGetLine h >>= putStrLn . ((++) "FROM SERVER: ")