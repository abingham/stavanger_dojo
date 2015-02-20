import Data.Functor
import Control.Applicative

fizz = [Nothing, Nothing, Just "Fizz"]

buzz = [Nothing, Nothing, Nothing, Nothing, Just "Buzz"]

vals = zipWith (\a b -> (++) <$> a <*> b) (cycle fizz) (cycle buzz)

toStr :: Int -> Maybe Int -> String
toStr i Nothing = show i
toStr i (Just x) = show x

main = do
  fmap putStr take 100 vals
