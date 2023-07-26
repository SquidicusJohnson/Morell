module Permutation (
  Permutation
  , inverse
  , size
  , parity
  , period
  , compose
  , actOnList
  , inverseActOnList
  , toIndices
  , toIndices'
  , toInverseIndices
  , toInverseIndices'
  , toFactoriadic
  , fromFactoriadic
  , toTranspositions
  , fromCycles
  , toCycles
  , imageOfIndex
  , preimageOfIndex
) where

import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Map ((!))

-- | Wraps an Integer and represents an abstract permutation.
--   Particulary every positive integer represents a finite set of transpositions.
data Permutation = Permutation Integer deriving (Eq, Show)

unwrap (Permutation n) = n

instance Semigroup Permutation where
  (<>) = compose

instance Monoid Permutation where
  mempty = Permutation 0

-- This package establishes a Permutation type, representing an abstract permutation
-- on the set of positive integers [1..] which moves finitely many indices.
--
-- This type only wraps an Integer. 0 and all negative numbers represent
-- the identity permutation, or the permutation that moves no indices. Every
-- positive integer corresponds to a unique permutation in a similar way to Lehmer codes
-- or inversion vectors.
--
-- The integers from 1 to n! represent the permutations on a set of size n.
-- Using factorial number representation each integer between 1 and n! is
-- interpreted as an ordered series of no more than n transpositions, swaps between two indices.
--
-- In the order of least significant digit to the most.
-- A factorial digit of zero means no swap is performed for that digit.
-- Otherwise for a digit n at place m!, n is swapped with m+1.
--
-- As an example, 15 = 3*4! + 0*3! + 1*2! + 1*1! represents swapping
-- 1 with 2  then  1 with 3  then  3 with 5
--
-- This package uses the convention that for two permutations p and q
-- p <> q is the permutation equivalent to permuting by q then p.
-- and mconcat [p1, p2.. pn-1, pn] is the same as permuting by pn then p-1 and so on.
-- However, when working with a list of transpositions,
-- it is treated as a stack such that the first transposition in the list
-- occurs before those beneath it.

-- | Get the inverse permutation of a permutation p.
inverse :: Permutation -> Permutation
inverse = fromReverseTranspositions . toTranspositions

-- | The largest index moved by a permutation.
size :: Permutation -> Int
size = (+1) . length . toFactoriadic

-- | Returns false for permutations with an even number of transpositions,
--   true for odd.
parity :: Permutation -> Bool
parity = odd . length . filter ( /= 0 ) . toFactoriadic

-- | The number of times you would have to compose a permutation p with itself
--   to get the identity permutation.
period :: Permutation -> Int
period = foldr (lcm . length) 1 . toCycles

-- | compose g h computes the permutation equivalent to permuting a set by h then
--   permuting it by g. Use (<>) for infix.
compose :: Permutation -> Permutation -> Permutation
compose (Permutation 0) h = h
compose g (Permutation 0) = g
compose g h = fromReverseTranspositions $ foldl (flip leftMultiplyTransposition) (reverse $ toTranspositions g) (toTranspositions h)


-- | Permute a list by a permutation, returns Nothing if the list is
--   shorter than the size of the permutation.
actOnList :: Permutation -> [a] -> Maybe [a]
actOnList p xs = if length xs < size p
    then Nothing
    else Just $ map (\i -> xs!!i) ( take (length xs) (toIndices' p) )

-- | Permute a list by the inverse of a permutation, returns Nothing if
--   the list is shorter than the size of the permutation.
inverseActOnList :: Permutation -> [a] -> Maybe [a]
inverseActOnList p xs = if length xs < size p
    then Nothing
    else Just $ map (\i -> xs!!i) ( take (length xs) (toInverseIndices' p))


-- | Converts a permutation p to the result of it acting on the list [0 .. size p - 1].
toIndices :: Permutation -> [Int]
toIndices p = map ( subtract 1 . preimageOfIndex p ) [1 .. size p]

-- | Converts a permutation p to the result of it acting on the list [0..].
--   The result is an infinite list
toIndices' :: Permutation -> [Int]
toIndices' p = toIndices p ++ drop (size p) [0..]

-- | Converts a permutation p to the result of its inverse acting on the list [0 .. size p - 1].
toInverseIndices :: Permutation -> [Int]
toInverseIndices p = map ( subtract 1 . imageOfIndex p ) [1 .. size p]

-- | Converts a permutation p to the result of its inverse acting on the list [0..].
--   The result is an infinite list.
toInverseIndices' :: Permutation -> [Int]
toInverseIndices' p = toInverseIndices p ++ drop (size p) [0..]



-- | Converts a Permutations stored integer into a left to right list of its digits in factoriadic.
toFactoriadic :: Permutation -> [Int]
toFactoriadic = unfoldr (\(n, place) ->
                  if n < 1
                    then Nothing
                    else Just ( fromInteger (n `mod` place) , (n `div` place, place + 1) ) )
              . (\n -> (n, 2) )
              . unwrap

-- | Converts a left to right list of factoriadic digits to the corresponding Integer
fromFactoriadic :: [Int] -> Integer
fromFactoriadic = unwrap . fromTranspositions . zip [2..]

-- | Converts a permutation to a left to right list of transpositions it represents.
toTranspositions :: Permutation -> [(Int, Int)]
toTranspositions = zip [2..] . toFactoriadic

-- | Convert a list of cycles to the corresponding Permutation.
--   Individual cycles must not contain duplicate indices, but cycles do not
--   need to be disjoint.
fromCycles :: [[Int]] -> Permutation
fromCycles = reduceTranspositions
           . concatMap (
                map (\(i, j) -> (max i j, min i j) )
              . (\(x:xs) -> zip (repeat x) xs )
           )
           . filter (not . null)

-- | Convert a permutation to cycle notation
toCycles :: Permutation -> [[Int]]
toCycles = map (\(i:is) -> i : reverse is ) . toInverseCycles

-- | Convert a permutation to cycle notation for its inverse
toInverseCycles :: Permutation -> [[Int]]
toInverseCycles = map (\(i,is) -> i:is )
                . M.toList
                . foldr buildCycles M.empty
                . filter (not . (== 0) . snd)
                . toTranspositions

buildCycles :: (Int,Int) -> M.Map Int [Int] -> M.Map Int [Int]
buildCycles (i, j) xss = case M.lookup j xss of
  Nothing -> case M.lookup i xss of
    Nothing -> M.insert j [i] xss
    Just ks -> M.delete i $ M.insert j (i : xss!i) xss
  Just ks -> case M.lookup i xss of
    Nothing -> M.adjust (i:) j xss
    Just ls -> M.delete i $ M.insert j (xss!j ++ i : xss!i) xss



-- | Given a permutation p and an index i, returns the index that i moves to under p
imageOfIndex :: Permutation -> Int -> Int
imageOfIndex p n = foldl (flip swap) n (toTranspositions p)

-- | Given a permutation p and an index i, returns the index that moves to i under p
preimageOfIndex :: Permutation -> Int -> Int
preimageOfIndex p n = foldr swap n (toTranspositions p)

swap (i, 0) n = n
swap (i, j) n
  | n == i = j
  | n == j = i
  | otherwise = n


fromTranspositions :: [(Int, Int)] -> Permutation
fromTranspositions = Permutation . foldr (\(i,j) n -> (n + toInteger j) * toInteger (i-1) ) 0

fromReverseTranspositions :: [(Int,Int)] -> Permutation
fromReverseTranspositions = Permutation . foldl (\n (i,j) -> (n + toInteger j) * toInteger (i-1) ) 0

-- This function assumes that i > j, k > l, k is strictly descending, and (i 0) represents
-- swapping i with nothing, or the identity permutation.
leftMultiplyTransposition :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
leftMultiplyTransposition (i, j) ((k, l) : xs)
  | i > k = (i, j) : (k, l) : xs

  -- (i 0) (k l) = (k l)
  | j == 0 = (k, l) : xs
  -- (i j) (i 0) = (i j)
  | i == k && l == 0 = (i, j) : xs
  -- (i j) (k 0) = (k 0) (i j)
  | i <  k && l == 0 = (k, l) : leftMultiplyTransposition (i, j) xs

  -- (i j) (k i) = (k j) (i j)
  | i == l = (k, j) : leftMultiplyTransposition (i, j) xs

  -- (i j) (k j) = (k i) (i j)
  | i < k && j == l = (k, i) : leftMultiplyTransposition (i, j) xs

  -- (i j) and (k l) are disjoint so (k l) (i j) = (i j) (k l)
  | i < k && j /= l = (k, l) : leftMultiplyTransposition (i, j) xs

  -- (i j) (i j) = (i 0)
  | i == k && j == l = (i, 0) : xs

  -- (i l) (i j) = (j l) (i l) = (i j) (j l)
  -- this case is why we reduce g into h and not h into g, in order to move
  -- these transpositions past each other one of them must have its larger index reduced.
  -- If our place values are descending it is not an issue, if they are ascending then
  -- this would require backtracking to guarantee we end up with valid factoriadic.
  | i == k && j /= l = (i, l) : leftMultiplyTransposition (max j l, min j l) xs
