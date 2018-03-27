-- PA5.hs  INCOMPLETE
-- Glenn G. Chappell
-- 21 Mar 2018
--
-- For CS F331 / CSCE A331 Spring 2018
-- Solutions to Assignment 5 Exercise B

module PA5 where
import Data.List

-- collatzCounts
collatzCounts :: [Integer]
collatzCounts = map collatzCount [1..]

collatzCount :: Integer -> Integer
collatzCount a = toInteger (length (collatz a)) - 1

collatz :: Integer -> [Integer]
collatz 0 = error "Can't converge Collatz to 1 from 0"
collatz 1 = [1]
collatz a
      | (a `mod` 2) == 0 = a : collatz (a `div` 2)
      | otherwise = a : collatz ((3*a) + 1)




-- findList
findList :: Eq a => [a] -> [a] -> Maybe Int
findList [] _ = Just 0
findList (x:xs) a
        | findList xs a >= Just 0 = elemIndex x a
        | otherwise = Nothing


-- operator ##
(##) :: Eq a => [a] -> [a] -> Int
_ ## _ = 42  -- DUMMY; REWRITE THIS!!!


-- filterAB
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB _ _ bs = bs  -- DUMMY; REWRITE THIS!!!


-- sumEvenOdd
sumEvenOdd :: Num a => [a] -> (a, a)
{-
  The assignment requires sumEvenOdd to be written using a fold.
  Something like this:

    sumEvenOdd xs = fold* ... xs where
        ...

  Above, "..." should be replaced by other code. The "fold*" must be
  one of the following: foldl, foldr, foldl1, foldr1.
-}
sumEvenOdd _ = (0, 0)  -- DUMMY; REWRITE THIS!!!
