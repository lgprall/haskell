import System.Random

check :: String -> String -> Char -> (Bool, String)
check word display c =
  (c `elem` word, [ if x == c
    then c
    else y
    | (x,y) <- zip word display ])


turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n == 0
     then putStrLn ("You lose! The word was " ++ word)
     else if word == display
       then putStrLn "You win!"
       else mkguess word display n


mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStr (display ++ "   " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n - 1
     turn word display' n'

starman :: IO ()
starman = do
  words <- readFile "words"
  let list = lines words
  index <- randomRIO (0, 228981) :: IO Int
  let word = list !! index
  let n = length word
  turn word [ '-' | x <- word ] n
  