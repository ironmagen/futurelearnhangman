import System.IO
import System.Random
import Data.Char


main = do
     handle <- openFile "wordbank.txt" ReadMode      
     contents <- hGetContents handle
     let wordsIO = words contents
     let listLength = length wordsIO
     randWordPos <- randomRIO (0, (listLength))
     let playWord = wordsIO !! (randWordPos)
     hangman playWord 7

check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x == c
         then c
         else y | (x,y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn ("Game over, the correct word was " ++ word)
       else if word == display
               then putStrLn "You WIN!!"
               else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ " " ++ take n (repeat '*'))
     putStr " Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

hangman :: String -> Int -> IO ()
hangman word n = turn word ['-' | x <- word] n 
