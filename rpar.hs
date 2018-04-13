import Control.Exception (evaluate)
import Control.Parallel.Strategies (Eval, rpar, rseq, runEval)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import Text.Printf (printf)


fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


main :: IO ()
main = do
  [n] <- getArgs
  let test = [test1,test2,test3,test4] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0


test1 :: Eval (Integer, Integer)
test1 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x,y)


test2 :: Eval (Integer, Integer)
test2 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x,y)


test3 :: Eval (Integer, Integer)
test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  _ <- rseq x
  return (x,y)


test4 :: Eval (Integer, Integer)
test4 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  _ <- rseq x
  _ <- rseq y
  return (x,y)


printTimeSince :: UTCTime -> IO ()
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

