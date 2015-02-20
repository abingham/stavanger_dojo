fizz :: [Maybe String]
fizz = [Nothing, Nothing, Just "Fizz"]

buzz :: [Maybe String]
buzz = [Nothing, Nothing, Nothing, Nothing, Just "Buzz"]

combine :: Maybe String -> Maybe String -> Maybe String
combine Nothing x = x
combine x Nothing = x
combine (Just x) (Just y) = Just (x ++ y)

combined :: [Maybe String]
combined = zipWith combine (cycle fizz) (cycle buzz)

toString :: Int -> Maybe String -> String
toString i Nothing = show i
toString _ (Just x) = x

main :: IO ()
main = do
  putStrLn.show $ zipWith toString [0..99] combined
