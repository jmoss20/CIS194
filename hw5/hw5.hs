{-# LANGUAGE FlexibleInstances #-}

{-- Lecture notes:

- Type classes
	Used to restrain/guarantee properties of generic types
	ex) (==) :: Eq a => a -> a -> Bool
	(Says that == returns a bool from generic a, iff a is of type class Eq (i.e. implements == and /=))
 - Type class syntax
	---
	class Eq a where
		(==), (/-) :: a -> a -> Bool
		x == y = not (x /= y)
		x /= y = not (x == y)
	---
 - Standard type classes
	Ord : types whose elements can be totally ordered, provides ops like (<) and (<=)
	Num : numeric types, can do things like arithmetic
	Show: converts to strings
	Read: converts string -> a
	Integral: Int-esque types
 - Type class example
	--- (Class example, providing method toList)
	class Listable a where
		toList :: a -> [Int]
	---
	--- (Instance of class type, implementing class methods)
	instance Listable Int where
		toList x = [x]
	instance Listable Bool where
		toList True = [1]
		toList False = [0]
	---
--}

import ExprT
import Parser
import Data.Maybe
import qualified Data.Map.Strict as M

{-- Definition below...
data ExprT = Lit Integer
	   | Add ExprT ExprT
	   | Mul ExprT ExprT
	   deriving (Show, Eq)
--}

eval 			:: ExprT -> Integer
eval (Mul exp1 exp2) 	= (eval exp1) * (eval exp2)
eval (Add exp1 exp2)	= (eval exp1) + (eval exp2)
eval (Lit i)		= i

-- Style doesn't seem right...
evalStr 		:: String -> Maybe Integer
evalStr	s		= if isJust c then Just (eval (fromJust c)) else Nothing 
			where c = parseExp Lit Add Mul s
-- Type class
class Expr a where
	mul :: a -> a -> a
	add :: a -> a -> a
	lit :: Integer -> a
instance Expr ExprT where
	mul e1 e2 = Mul e1 e2
	add e1 e2 = Add e1 e2
	lit e     = Lit e

-- For use in ghci...
reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
	mul i1 i2 = i1 * i2
	add i1 i2 = i1 + i2
	lit i	  = i
instance Expr Bool where
	mul b1 b2 = b1 && b2
	add b1 b2 = b1 || b2
	lit b	  = if b <= 0 then False else True

newtype MinMax	= MinMax Integer deriving (Eq, Show)
newtype Mod7	= Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
	mul (MinMax m1) (MinMax m2) = MinMax (max m1 m2)
	add (MinMax m1) (MinMax m2) = MinMax (min m1 m2)
	lit m			    = MinMax m
instance Expr Mod7 where
	mul (Mod7 m1) (Mod7 m2)	= Mod7 (m1 * m2 `mod` 7)
	add (Mod7 m1) (Mod7 m2)	= Mod7 (m1 + m2 `mod` 7)
	lit m			= Mod7 (m `mod` 7)

-- For testing ex. 4...
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger	= testExp :: Maybe Integer
testBool	= testExp :: Maybe Bool
testMM		= testExp :: Maybe MinMax
testSat		= testExp :: Maybe Mod7

-- Exercise 6
class HasVars a where
	var :: String -> a

data VarExprT = Lit' Integer
	      | Add' VarExprT VarExprT
	      | Mul' VarExprT VarExprT
	      | Var' String
	      deriving (Show, Eq)
instance Expr VarExprT where
	add v1 v2 = Add' v1 v2
	mul v1 v2 = Mul' v1 v2
	lit v     = Lit' v
instance HasVars VarExprT where
	var s     = Var' s

-- For use in ghci...
reify' :: VarExprT -> VarExprT
reify' = id

instance HasVars (M.Map String Integer -> Maybe Integer) where
	var s m = M.lookup s m
	
-- Inspired by https://github.com/xyc/cis194/blob/master/cis194-spring13/hw5/WithVars.hs
instance Expr (M.Map String Integer -> Maybe Integer) where
	lit v _   = Just v
	add v1 v2 = let c (Just v1) (Just v2) = Just (v1 + v2)
			c _ _ = Nothing
			in \m -> c (v1 m) (v2 m)
	mul v1 v2 = let c (Just v1) (Just v2) = Just (v1 * v2)
			c _ _ = Nothing
			in \m -> c (v1 m) (v2 m)
	
withVars :: [(String, Integer)]
	 -> (M.Map String Integer -> Maybe Integer)
	 -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
-- f $ g x === f (g x)
