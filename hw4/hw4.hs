{--
Lecture notes:

## Anonymous functions
 - lambda functions: \x y -> x * y
	defines anonymous function f(x, y) = x * y
 - partial application: (?x)
	uses partial application to get a function of one less var from ? (applying x as first arg)
	(can also use (x?) to apply argument to the other side of an infix op)
	ex:	(>100) === \x -> x > 100

## Function composition
 - (.) operator =~= f g = \x -> f (g x)
	. :: (b -> c) -> (a -> b) -> a -> c
	("return value" doesnt have parens for __partial application__)

## Partial application & Currying
 - "currying"
	Haskell Curry (1900-1982), idea of one-argument functions returning functions as a way to represent mult-argument functions
 - true n-arg functions?
	Use an n-tuple as the single argument
 - Switching between curry-style and tuple-style
	curry :: ((a, b) -> c) -> a -> b -> c
	uncurry :: (a -> b -> c) -> (a, b) -> c
	ex: uncurry (+) (2,3) === 5
 - Tip: Haskell doesn't make it (as) easy to partially apply to non-first arg
	Thus args should be ordered from "least to greatest variation"

## __point-free style__
 - "in which we define a function without reference to its arguments -- in some sense saying what a function __is__ rather than what it __does__"

--}

import Data.List

-- ## Wholemeal programming (i.e. def f1 and f2 more idiomatically)
fun1 :: [Integer] -> Integer
fun1 []			= 1
fun1 (x:xs)
	| even x	= (x - 2) * fun1 xs
	| otherwise	= fun1 xs

fun2 :: Integer -> Integer
fun2 1			= 0
fun2 n
	| even n	= n + fun2 (n `div` 2)
	| otherwise 	= fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> if even x then (x - 2) * y else y) 1

fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (/= 1) . iterate (\x -> if even x then (x `div` 2) else (3 * x + 1))

-- ## Folding w/ trees
data Tree a 	= Leaf 
		| Node Integer (Tree a) a (Tree a) 
		deriving (Show, Eq)

-- (Creates a *balanced* tree from a list)
foldTree :: [a] -> Tree a
foldTree = foldr addToTree Leaf

-- What's that weird 3rd condition?
-- ... we known the height increases just after the tree is "full" === log_2(num_leaves) \in Z
addToTree x Leaf = Node 0 (Leaf) x (Leaf)
addToTree x y@(Node h lt v rt)
	| numLeaves lt < numLeaves rt 	= Node h (addToTree x lt) v rt
	| numLeaves lt > numLeaves rt 	= Node h lt v (addToTree x rt)
	| (numLeaves lt == numLeaves rt) && floor (logBase 2 (numLeaves rt)) == ceiling (logBase 2 (numLeaves rt)) = Node (h+1) lt v (addToTree x rt)
	| otherwise		 	= Node h lt v (addToTree x rt)

numLeaves 	Leaf 			= 1
numLeaves 	(Node _ lt _ rt)	= (numLeaves lt) + (numLeaves rt)

-- ## XOR
xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not (x && y)) True

-- map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> [f x] ++ y) []

-- foldl
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f)  base xs

-- ## Sieve of Sundaram
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

exclusionList n = filter (<= n) [i + j + 2*i*j | i <- [1..n], j <- [i..n]]
sieve n = map (\x -> 2*x + 1) ([1..n] \\ exclusionList n)
