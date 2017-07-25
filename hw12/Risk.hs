{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
	deriving Show

-- # Exercise 2
rollDice :: Int -> Rand StdGen [DieValue]
rollDice n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) = rollDice (aNum + dNum) >>= (\ds -> return (updateBF b ds))
	where	aNum = min (a - 1) 3
		dNum = min d 3

updateBF :: Battlefield -> [DieValue] -> Battlefield
updateBF (Battlefield a b) ds = Battlefield (a - casA) (b - casB)
	where 	casA = fst $ calcCas a b ds
		casB = snd $ calcCas a b ds

calcCas :: Army -> Army -> [DieValue] -> (Int, Int)
calcCas a b ds = sortedDiceToCas (fmap2 (reverse . sort) (splitAt a ds)) (0,0)
	where fmap2 = (\f (x,y) -> (f x, f y))

sortedDiceToCas :: ([DieValue], [DieValue]) -> (Int, Int) -> (Int, Int)
sortedDiceToCas ([], _) t = t
sortedDiceToCas (_, []) t = t
sortedDiceToCas (currA:a, currd:d) (ac, dc) = sortedDiceToCas (a, d) (if a > d then ac else ac + 1, if a > d then dc + 1 else dc)

-- # Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
	| a < 2 = pure b
	| d <= 0 = pure b
	| otherwise = battle b >>= invade

-- # Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb b = replicateM 1000 (invade b) >>= eachSuccess

-- Annoying that this can't be inline... how else to give GHC the desired type signature?
eachSuccess :: [Battlefield] -> Rand StdGen Double
eachSuccess bf = return $ fromIntegral (length (filter ((==0) . defenders) bf)) / fromIntegral 1000
