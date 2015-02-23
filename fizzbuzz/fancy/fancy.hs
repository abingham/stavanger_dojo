import Data.List
import Data.Monoid

data FizzBuzz = Number Integer | Fizz Integer | Buzz Integer | FizzBuzz Integer

instance Show FizzBuzz where
  show (Number x) = show x
  show (Fizz _) = "Fizz"
  show (Buzz _) = "Buzz"
  show (FizzBuzz _) = "FizzBuzz"

instance Monoid FizzBuzz where
  mempty = Number 0
  mappend (Fizz a) (Buzz b)
    | a == b = FizzBuzz a
  mappend (Buzz a) (Fizz b)
    | a == b = FizzBuzz a
  mappend (Number _) b = b
  mappend a (Number _) = a

fizz = cycle [Number, Number, Fizz]
buzz =  cycle [Number, Number, Number, Number, Buzz]
fizzbuzz = zipWith3 (\f b n -> mappend (f n) (b n)) fizz buzz [1..]

foo n = intercalate " " $ take n $ map show fizzbuzz

main = do
  -- putStrLn $ foo
  -- sequence $ map putStrLn.show fizzbuzz
  return ()
