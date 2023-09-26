import System
import P3

main : IO ()
main = do
  [_, x] <- getArgs
    | xs => printLn xs
  printLn $ factors (cast x)
