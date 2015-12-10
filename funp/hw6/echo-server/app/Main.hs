import Network
import Control.Concurrent(forkIO)
import System.IO
import System.Environment
import System.Directory
import System.Exit
import qualified Text.Read as T

{-
Реализуйте простой эхо-сервер.
Это программа, которая слушает определенный порт, принимает соединения, и всё, что присылает клиент, отправляет ему обратно.
(2 балла)

Пример вызова:
    ./echo_server-exe 5566
-}

main :: IO ()
main = getArgs >>= parse >>= withSocketsDo . processPort

parse :: [String] -> IO Int
parse [] = putStrLn "error: bad arguments, no port passed" >> exitFailure
parse (p:as) = 
    case T.readMaybe p :: Maybe Int of 
        Nothing -> putStrLn "error: cannot parse first arg as port" >> exitFailure
        Just x -> return x

processPort :: Int -> IO ()
processPort port = do 
    s <- listenOn $ PortNumber (fromIntegral port)
    waitForConnection s

waitForConnection :: Socket -> IO ()
waitForConnection s = do
    (h, _, _) <- accept s
    forkIO $ echoService h
    waitForConnection s

echoService :: Handle -> IO()
echoService h = do
    eof <- hIsEOF h
    case eof of
        False -> hGetLine h >>= hPutStrLn h >> echoService h
        True -> return ()
