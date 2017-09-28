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
       then putStrLn ("You win! The word is " ++ word)
       else mkguess word display n


mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStr (display ++ "   " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     if q == ""
       then mkguess word display n
       else do
         let char = q !! 0
         if (elem char ['a'..'z'])
           then do
             let (correct, display') = check word display char
             let n' = if correct then n else n - 1
             turn word display' n'
           else do mkguess word display n

starman :: IO ()
starman = do
  words <- readFile "words"
  let list = lines words
  index <- randomRIO (0, (length list)-1) :: IO Int
  let word = list !! index
  let n = length word
  turn word [ '-' | x <- word ] n
  
