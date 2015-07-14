module OneToTen where

-- | Problem 1
myLast :: [a] -> a
myLast []     = error "error"
myLast [l]    = l
myLast (_:xs) = myLast xs

-- | Problem 2
myButLast :: [a] -> a
myButLast []     = error "error"
myButLast (x:xs) = if length xs == 1
                      then x
                      else myButLast xs

-- | Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "List is empty."
elementAt _  0 = error "0 is not an element."
elementAt xs 1 = head xs
elementAt xs n
    | n <= length xs = head . drop (n - 1) $ xs
    | otherwise      = error "Not enough elements."

-- | Problem 4
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- | Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = error "Empty list"
isPalindrome xs = xs == myReverse xs

-- | Problem 8
compress :: Eq a => [a] -> [a]
compress []  = []
compress [a] = [a]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise    = x : compress xs

-- | Problem 9
pack :: Eq a => [a] -> [[a]]
pack = init . go
    where go []  = [[]]
          go lst@(x:xs)
            | null dupes  = [x]   : go xs
            | otherwise   = dupes : go rest
                where dupes = takeWhile (== x) lst
                      rest  = dropWhile (== x) lst

-- | Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = zip lens elems
    where lst   = pack xs
          elems = map head lst
          lens  = map length lst





















