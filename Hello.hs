-- H24.11.24 16:45:00 begin the world
module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let num1 = read (args !! 0)
      num2 = read (args !! 1) in
      putStrLn ("Hello, " ++ show (num1 + num2) ++ "," ++ show (num1 `div` num2))

-- ghc -o hello.exe --make Hello.hs && hello 8 3