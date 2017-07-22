{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-tabs #-}

module Party where

import Data.Tree
import Data.Monoid
import System.Environment

import Employee
{--
	Data types:
	
	type Name = String
	type Fun = Integer
	
	data Employee = Emp { empName :: Name, empFun :: Fun }
		deriving (Show, Read, Eq)

	data GuestList = GL [Employee] Fun
		deriving (Show, Eq)
--}

-- Exercise 1
-- 1.1
-- Naive (w/r/t total fun update) append of Employee to GuestList
glCons :: Employee -> GuestList -> GuestList
glCons e (GL el f) = GL (e:el) (f + empFun e)

-- 1.2
-- Monoid instance for GuestList
instance Monoid GuestList where
	mempty = GL [] 0
	mappend (GL ll lf) (GL rl rf) = GL (ll ++ rl) (lf + rf)

-- 1.3
-- Comparing GuestLists by fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2 
-- Folding Data.Tree
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rl sf) = f rl (map (treeFold f) sf)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e dgl = (mconcat . map (uncurry moreFun) $ dgl , glCons e . mconcat . map fst $ dgl)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . treeFold (nextLevel)

-- Exercise 5
formatGL :: GuestList -> String
formatGL (GL gl fun) = "Fun: " ++ show fun ++ "\n" ++ unlines (map empName gl)

main = do
	args <- getArgs
	file <- readFile . head $ args
	putStrLn . formatGL . maxFun . read $ file
	return ()
