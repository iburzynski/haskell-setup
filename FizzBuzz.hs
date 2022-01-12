module FizzBuzz where

main :: IO ()
main = do
  mapM_ (putStrLn . fizzBuzz) [1 .. 100]

fizzBuzz :: Int -> String
fizzBuzz x
  | divis3 x && divis5 x = "FizzBuzz"
  | divis3 x = "Fizz"
  | divis5 x = "Buzz"
  | otherwise = show x
  where
    divisible m n = n `mod` m == 0
    divis3 = divisible 3
    divis5 = divisible 5
