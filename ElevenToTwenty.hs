module ElevenToTwenty where

import OneToTen(encode)

-- | Problem 11
data Encode a = Multiple Int a
              | Single a deriving (Show)

encodeModified :: Eq a => [a] -> [Encode a]
encodeModified = map go . encode
    where go (c,a) = if c == 1 then Single a else Multiple c a

-- | Problem 12
decodeModified :: [Encode a] -> [a]
decodeModified = concatMap go
    where go (Single a)     = [a]
          go (Multiple c a) = replicate c a

-- | Problem 13
encodeDirect :: Eq a => [a] -> [Encode a]
encodeDirect []  = []
encodeDirect lst@(x:xs)
    | dupes == 1 = Single         x : encodeDirect xs
    | otherwise  = Multiple dupes x : encodeDirect rest
        where dupes  = length $ takeWhile (== x) lst
              rest   = dropWhile (== x) lst

-- | Problem 14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

-- | Problem 15
repli :: [a] -> Int -> [a]
repli []     _ = []
repli xs     0 = xs
repli xs     1 = xs
repli (x:xs) n = replicate n x ++ repli xs n

-- | Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _     = []
dropEvery _  0     = error "Enter 1 or greater."
dropEvery _  1     = []
dropEvery xs n     = (map fst . filter go) lst
    where lst      = xs `zip` [1..length xs]
          go (_,c) = c `mod` n /= 0

-- | Problem 17
split :: [a] -> Int -> ([a], [a])
split []     _ = error "List is empty."
split _      0 = error "Enter 1 or greater."
split (x:xs) n = go [x] xs
    where go front back@(y:ys)
            | length front == n = (front, back)
            | otherwise         = go (front ++ [y]) ys

-- | Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = error "List is empty."
slice xs i k
    | i == 0 || k == 0               = error "Invalid index."
    | i > length xs || k > length xs = error "Out of bounds."
    | otherwise                      = go xs
        where go = take (k - i + 1) . drop (i - 1)

-- | Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = error "List is empty."
rotate xs 0 = xs
rotate xs n
    | n > 0     = drop n xs ++ take n xs
    | otherwise = front     ++ back
        where front = drop (length xs - (-1 * n)) xs 
              back  = take (length xs - (-1 * n)) xs

-- | Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "List is empty."
removeAt 0  _ = error "enter 1 or greater."
removeAt n xs = (e, lst)
    where e   = (last . take n) xs
          lst = (init . take n) xs ++ drop n xs













