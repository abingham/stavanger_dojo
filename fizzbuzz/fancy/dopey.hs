import Data.List

display f b n
  | length (f ++ b) > 0 = f ++ b
  | otherwise = show n

fizz = cycle ["", "", "Fizz"]
buzz = cycle ["", "", "", "", "Buzz"]
fizzbuzz = zipWith3 display fizz buzz [1..]


main = do
  putStrLn $ intercalate " " $ take 100 fizzbuzz
  return ()
