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
