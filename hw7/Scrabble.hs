module Scrabble where

data Score = Score Int deriving (Eq, Show)

instance Monoid Score where
	mempty = Score 0
	mappend (Score a) (Score b) = Score (a + b)

score :: Char -> Score
score c
	| any (==c) "aeilnorstuAEILNORSTU"	= Score 1
	| any (==c) "dgDG"			= Score 2
	| any (==c) "bcmpBCMP"			= Score 3
	| any (==c) "fhvwyFHVWY"		= Score 4
	| any (==c) "kK"			= Score 5
	| any (==c) "jxJX"			= Score 8
	| any (==c) "qzQZ"			= Score 10
	| otherwise				= Score 0

scoreString :: String -> Score
scoreString = mconcat . map (\x -> score x)
