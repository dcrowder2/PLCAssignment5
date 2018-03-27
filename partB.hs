import Data.List


findList :: Eq a => [a] -> [a] -> Maybe Int
findList [] _ = Just 0
findList (x:xs) a
        | findList xs a >= Just 0 = elemIndex x a
        | otherwise = Nothing
