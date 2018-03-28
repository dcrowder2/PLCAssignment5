{-
PA5.hs
Dakota Crowder
dcrowder2@alaska.edu
2018, March 27th

CSCE A331 Programming Language Concepts

Based on the skeleton code provided by
Glenn Chappell
https://projects.cs.uaf.edu/redmine/projects/cs331_2018_01/repository/revisions/41621484cea3c02b7c4e1160fd79645e590d7427
-}

module PA5 where


-- collatzCounts, returns an infinte list of the number of iterations it takes
-- to reach 1 via the Collatz Function
collatzCounts :: [Integer]
collatzCounts = map collatzCount [1..]

-- collatzCount, types the length the result of the collatz fucntion to Integer
-- and decrements by 1 to account for the fact that the final number is always 1
-- and therefore doesn't count as an iteration
collatzCount :: Integer -> Integer
collatzCount a = toInteger (length (collatz a)) - 1

-- collatz, preforms the collatz function with a single parameter, the starting
-- value, and creates a list of the results from each iteration of the collatz
-- function
collatz :: Integer -> [Integer]
collatz 0 = error "Can't converge Collatz to 1 from 0"
collatz 1 = [1]
collatz a
      | even a = a : collatz (a `div` 2)
      | otherwise = a : collatz ((3*a) + 1)




-- findList, returns the first index of the sublist a in list b
findList :: Eq a => [a] -> [a] -> Maybe Int
findList a b
            | findList' a b >= 0 = Just (findList' a b)
            | otherwise = Nothing

-- findList', finds the first time in the array x:xs that the full array a can
-- be found then increments how many recursive calls were made to get to that
-- index in the array, effectively finding the first index of the subset
findList' :: Eq a => [a] -> [a] -> Int
findList' _ [] = minBound::Int
findList' a (x:xs)
  | length a > length (x:xs)    = minBound::Int
  | and (zipWith (==) a (x:xs)) = 0
  | otherwise                   = 1 + findList' a xs


-- operator ##, counts the number of elements in the lists a:as b:bs that are
-- the same
(##) :: Eq a => [a] -> [a] -> Int
[] ## _ = 0
_ ## [] = 0
(a:as) ## (b:bs)
      | a == b = 1 + (as ## bs)
      | otherwise = (as ## bs)



-- filterAB, returns a list of all the elements in a:as that are also in b:bs
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB f [] _ = []
filterAB f _ [] = []
filterAB f (a:as) (b:bs)
        | f a = b : (filterAB f as bs)
        | otherwise = filterAB f as bs


-- sumEvenOdd, creates a tuple of the sums of the elements in the even indicies
-- and the sum of the elements in the odd indicies
sumEvenOdd :: Num a => [a] -> (a, a)
{-
  The assignment requires sumEvenOdd to be written using a fold.
  Something like this:

    sumEvenOdd xs = fold* ... xs where
        ...

  Above, "..." should be replaced by other code. The "fold*" must be
  one of the following: foldl, foldr, foldl1, foldr1.
-}
sumEvenOdd as = (foldr1 (+) bs, foldr1 (+) cs) where
  bs = getEvens as 0
  cs = getOdds as 0

-- getEvens, returns a list of the elements in list a:as that are the even indicies
getEvens :: Num a => [a] -> Integer -> [a]
getEvens [] _ = [0]
getEvens (a:as) i
      | even i = a : getEvens as (i+1)
      | otherwise = getEvens as (i+1)
-- getOdds, returns a list of the elements in list a:as that are the odd indicies
getOdds :: Num a => [a] -> Integer -> [a]
getOdds [] _ = [0]
getOdds (a:as) i
        | odd i = a : getOdds as (i+1)
        | otherwise = getOdds as (i+1)
