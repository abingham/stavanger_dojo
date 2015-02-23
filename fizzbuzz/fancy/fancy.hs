import Data.List

data FizzBuzz = Number | Fizz | Buzz | FizzBuzz
    deriving (Show)

combine :: FizzBuzz -> FizzBuzz -> FizzBuzz
combine Fizz Buzz = FizzBuzz
combine Number b = b
combine a Number = a

fizzy :: [FizzBuzz]
fizzy = cycle [Number, Number, Fizz]

buzzy :: [FizzBuzz]
buzzy =  cycle [Number, Number, Number, Number, Buzz]

fizzbuzzy :: [FizzBuzz]
fizzbuzzy = zipWith combine fizzy buzzy

display :: FizzBuzz -> Integer -> String
display Number n = show n
display t _ = show t

fizzbuzz :: Int -> [String]
fizzbuzz n = zipWith display (take n fizzbuzzy) [1..]

main :: IO ()
main = do
  putStrLn $ intercalate " " $ fizzbuzz 100
  -- sequence $ map putStrLn.show fizzbuzz
  return ()
