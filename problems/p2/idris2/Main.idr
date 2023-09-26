module Main

import System
import P2

main : IO ()
main = do
  [_, x] <- getArgs
    | xs => printLn xs
  printLn $ solve (cast x)
