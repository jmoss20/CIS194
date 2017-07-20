{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

import Data.List

data JoinList m a = Empty
		  | Single m a
		  | Append m (JoinList m a) (JoinList m a)
		  deriving (Eq, Show)

-- # Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append ((tag l) `mappend` (tag r)) l r

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- # Exercise 2
-- 2.1
-- Spec: (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
index i Empty				= Nothing
index i jl
	| i < 0				= Nothing
indexJ i jl@(Append m jll jlr)
	| i > getSize (size m) 		= Nothing
	| i < getSize (size $ tag jll)	= indexJ i jll
	| i >= getSize (size $ tag jll)	= indexJ (i - (getSize $ size $ tag jll)) jlr
indexJ i (Single m a)
	| i == 0			= Just a
	| otherwise			= Nothing 
indexJ _ _				= Nothing

-- For testing above and below...
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0	= Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- 2.2
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl
	| i <= 0 			= jl
	| i >= getSize (size $ tag jl)	= Empty
dropJ _ Empty				= Empty
dropJ _ (Single _ _)			= Empty
dropJ i (Append m jll jlr)
	| leftSize <= i			= dropJ (i - leftSize) jlr
	| otherwise			= dropJ i jll +++ jlr 
	where leftSize = getSize . size . tag $ jll

-- 2.3
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl
	| i >= jlSize 	= jl
	| i <= 0	= Empty
	| otherwise 	= reverseJ . dropJ (jlSize - i) . reverseJ $ jl
	where jlSize = getSize . size . tag $ jl

reverseJ :: (Sized b, Monoid b) => JoinList b a -> JoinList b a
reverseJ Empty 			= Empty
reverseJ s@(Single _ _) 	= s
reverseJ (Append m l r)		= Append m (reverseJ r) (reverseJ l)

-- # Exercise 3 found in Scrabble.hs

-- For testing Scrabble implementation...
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- # Exercise 4
{-- Defined already in GHC.base...
instance (Monoid a, Monoid b) => Monoid (a, b) where
	mempty = (mempty, mempty)
	mappend (a1, b1) (a2, b2) = (mappend a1 a2, mappend b1 b2)
--}

type JoinListBuffer = JoinList (Score, Size) String

instance Buffer JoinListBuffer where
	toString Empty = ""
	toString (Single _ s) = s
	toString (Append _ l r) = (toString l) ++ (toString r)

	fromString s = foldl (+++) Empty $ map (\x -> Single (scoreString x, (Size 1)) x) (lines s)

	line = indexJ

	replaceLine n ln buf = takeJ n buf +++ fromString ln +++ dropJ (n + 1) buf

	numLines = getSize . size . snd . tag

	value = (\(Score i) -> i) . fst . tag 

main = runEditor editor (fromString $ unlines ["This buffer is for notes you don't want to save, and for",
				 	       "evaluation of steam valve coefficients.",
					       "To load a different file, type the character L followed",
					       "by the name of the file."] :: JoinListBuffer)
