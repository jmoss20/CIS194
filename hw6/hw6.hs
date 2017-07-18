{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.List

-- # Exercise 1
-- Fib definition...
fib   :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Naive attempt...
fibs1 :: [Integer]
fibs1 = map (fib) [0..] 

-- Exercise 2
-- O(n) method (hopefully)
{-- unfoldr: dual of foldr
	instead of "reducing" list to value by application of f, takes seed value and expands to list by application of f
	*** Using tuple as seed because we need more history/memory -- could use a 3-tuple if we needed more
	Here: 	takes seed (0,1), adds 0 to the list, computes new seed to be (1, 0+1)
		takes new seed (1, 1), adds 1 to the list, computes new seed to be (1, 1+1)
		takes new seed (1, 2), adds 1 to the list, computes new seed to be (2, 1+2)
		takes new seed (2, 3), adds 2 to the list, computes new seed to be (3, 3+2)
		takes new seed (3, 5), adds 3 to the list, computes new seed to be (5, 5+3)
		... (current list: 0, 1, 1, 2, 3; which is correct)
--}
fibs2 :: [Integer]
fibs2 = unfoldr (\(a,b) -> Just (a, (b, a+b))) (0,1)

-- Getting a feel for unfoldr
{-- Generates the set of integers by:
	takes initial seed 0, adds it to the list
	computes new seed, 0+1
	repeat...
--}
z :: [Integer]
z = unfoldr (\a -> Just (a, a+1)) 0

-- # Exercise 3
-- Note that a possible finite list (as opposed to a guaranteed infinite stream) would have a constructor for an empty list
data Stream a = StreamCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (StreamCons s sc) = s : streamToList sc

-- Implementing custom show
instance Show a => Show (Stream a) where
	show a = show $ take 20 (streamToList a)

-- # Exercise 4
streamRepeat   	:: a -> Stream a
streamRepeat s 	= StreamCons s $ streamRepeat s

streamMap   			:: (a -> b) -> Stream a -> Stream b
streamMap f (StreamCons s sc) 	= StreamCons (f s) (streamMap f sc)

streamFromSeed 			:: (a -> a) -> a -> Stream a
streamFromSeed f sd		= StreamCons sd (streamFromSeed f (f sd)) -- really elegant, constructs head with seed, tail defined recursively 
									  -- 	with new seed = f sd

-- # Exercise 5
nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

-- # Exercise 6
x :: Stream Integer
x = StreamCons 0 $ StreamCons 1 $ streamRepeat 0

instance Num (Stream Integer) where
	fromInteger a 						= StreamCons a $ streamRepeat 0
	negate s						= streamMap (\x -> -1 * x) s
	(+) (StreamCons x_0 xs_0) (StreamCons x_1 xs_1)		= StreamCons (x_0 + x_1) $ (+) xs_0 xs_1
	(*) (StreamCons x_0 xs_0) s_2@(StreamCons x_1 xs_1)	= StreamCons (x_0 * x_1) $ (streamMap (\x -> x * x_0) s_2) + (xs_0 * s_2)

instance Fractional (Stream Integer) where
	(/) s1@(StreamCons x_0 xs_0) s2@(StreamCons x_1 xs_1)	= (StreamCons (x_0 `div` x_1)) $
								  streamMap (`div` x_1) (xs_0 - ((s1 / s2) * xs_1))
-- Grand finale: fibonacci, somehow
{--
	F(x) = x / (1 - x - x^2)
--}
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- # Exercise 7
data Matrix = Mat Integer Integer Integer Integer deriving (Eq, Show)
instance Num Matrix where
	(*) (Mat i1 i2 i3 i4) (Mat j1 j2 j3 j4) = (Mat (i1*j1 + i2*j3) (i1*j2 + i2*j4) (i3*j1 + i4*j3) (i3*j2 + i4*j4))

-- Use sneaky fast matrix exponentiation (for free from Haskell's ^ implementation)
fib4 :: Integer -> Integer
fib4 n = (\(Mat a b c d) -> b) $ (Mat 1 1 1 0)^n
