-- H24.11.24 16:45:00 begin the world
module Main where
import System.Environment

main :: IO ()
main = do
  name <- getLine
  putStrLn ("Hello, " ++ name)

-- ghc -o hello.exe --make Hello.hs && hello
-- world!