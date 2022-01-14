-- Haskell File Anatomy

{-- 1. Language extensions go before anything else
       these modify compiler behavior -}
{-# LANGUAGE OverloadedStrings #-}

-- 2. Module declaration goes next (name should match filename)
module FileAnatomy where

{-- 3. Import statements go next
       these add functionality beyond Prelude
       (use qualified imports to prevent name collisions) --}
import qualified Data.Text as Text

{-- 4. Type declarations go next
       these are used to define custom data types --}
type FizzBuzz = Text.Text

-- Every module must have a main function to compile
main :: IO ()
main = do
  mapM_ (putStrLn . Text.unpack . fizzBuzz) [1 .. 100]

-- Impure functions: above this line!
-- ------------------------------------------------------------
-- Pure functions: below this line!

-- Top level functions should have type signatures
fizzBuzz :: Int -> FizzBuzz
fizzBuzz x
  | divis3 x && divis5 x = "FizzBuzz"
  | divis3 x = "Fizz"
  | divis5 x = "Buzz"
  | otherwise = Text.pack $ show x
  where
    -- Helper functions are often defined without type signatures
    divisible m n = n `mod` m == 0
    divis3 = divisible 3
    divis5 = divisible 5
