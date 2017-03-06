import Control.Exception(catch)
import System.Environment
import System.Directory
import qualified Data.Text as T
import qualified Data.String as S

{-
grep принимает строку и от 0 и больше имен файлов, выводит строки, в которых встречается как подстрока переданная первым параметром строчка.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)

Пример использования:
    grep-exe 'foo bar qux' foo.txt bar.txt
-}

main :: IO ()
main = getArgs >>= grep >>= putStr . concat

grep :: [String] -> IO [String]
grep [] = return ["error: bad number of argumetns\n"]
grep (p:fs) = mapM (grepFileOrError p) fs

grepFileOrError :: String -> String -> IO String
grepFileOrError p f = doesFileExist f >>= grepFileOrError'
    where grepFileOrError' False = return ("error: [" ++ f ++ "] no such file\n")
          grepFileOrError' True = do
              str <- readFile f
              return $ unlines $ filter (containsStr p) (lines str)

containsStr :: String -> String -> Bool
containsStr p str = not $ null $ T.breakOnAll (S.fromString p) (S.fromString str)

