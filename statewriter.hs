import Data.List
import Control.Monad.State
import Control.Monad.Trans.Writer    

get_primes :: Integer -> WriterT String (State [Int]) Char
get_primes limit = do
    put [10]
    tell "one"
    tell " two"
    return 'c'
    
    
main = do
  let ((res, log), primes) = runState (runWriterT (get_primes 12)) []
  putStrLn $ show primes
