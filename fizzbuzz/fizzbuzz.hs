import Data.List

fizzy :: Int -> Bool
fizzy n = mod n 3 == 0

buzzy :: Int -> Bool
buzzy n = mod n 5 == 0

fizzbuzz :: Int -> String
fizzbuzz n
  | fn && bn = "FizzBuzz"
  | fn = "Fizz"
  | bn = "Buzz"
  | otherwise = show n
  where fn = fizzy n
        bn = buzzy n

main :: IO ()
main = do
  putStrLn $ intercalate "\n" $ map fizzbuzz [1..100]
