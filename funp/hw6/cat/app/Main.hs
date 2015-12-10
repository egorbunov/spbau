import Control.Exception(catch)
import System.Environment
import System.Directory

{-
cat принимает имена файлов и выводит их содержимое на экран.
Если в cat не передаются параметры, то она копирует stdin в stdout.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(2 балла)

Пример вызова:
    ./cat-exe foo.txt bar.txt ../baz.txt ~/qux.txt
-}

main :: IO ()
main = getArgs >>= catFiles >>= putStr . concat

catFiles :: [String] -> IO [String]
catFiles [] = getContents >>= \x -> return (x : [])
catFiles xs = mapM catFileOrError xs

catFileOrError :: String -> IO String
catFileOrError f = doesFileExist f >>= catFileOrError'
    where catFileOrError' False = return ("error: [" ++ f ++ "] no such file\n")
          catFileOrError' True = readFile f
