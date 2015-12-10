import System.Random(randomRIO)
import qualified Text.Read as T

{-
Реализуйте следующую программу.
Программа загадывает число от 1 до 100, пользователь должен отгадать его.
После каждой попытки программа говорит больше ее число или меньше.
Если пользователь не отгадал за 5 попыток, то проигрыш, иначе победа.
(1.5 балла)

Пример вызова:
    ./guess-exe
-}

main :: IO ()
main = do
    n <- randomRIO (1, 100) :: IO Int
    print n
    play n 5

play :: Int -> Int -> IO()
play n 0 = putStrLn "You lose!"
play n i = do
    putStrLn "Guess?"
    g <- getLine
    case T.readMaybe g :: Maybe Int of 
        Nothing -> putStrLn "Not a number...try again!" >> play n i
        Just n' -> answer where
            answer
                | n' == n = putStrLn "You won!" 
                | n' > n  = putStrLn "Not taht big! :)" >> play n (i - 1)
                | n' < n  = putStrLn "Not so small! :)" >> play n (i - 1)
