import System.IO
import Data.Char

main :: IO ()
main = do
       inh <- openFile "/usr/share/dict/web2" ReadMode
       outh <- openFile "./words" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
  do ineof <- hIsEOF inh
     if ineof
       then return ()
       else do inpStr <- hGetLine inh
               if ((length inpStr) > 4)
                 then hPutStrLn outh (map toLower inpStr)
                 else putStr "."
               mainloop inh outh
