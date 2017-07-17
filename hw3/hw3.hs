-- Golf

import Data.List

{--
Take 1D list, output 2D list like so
"abcd" -> [["abcd"], ["bd"], ["c"], ["d"]
--}
skips 			:: 	[a] -> [[a]]
skipN			::	[a] -> Int -> [a]
skipN xs n		=	map (\x -> (fst x)) (filter (\x -> (snd x) `mod` n == 0) (zip xs [1..]))
skips xs		=	map (\x -> (skipN (fst x) (snd x))) (zip (map (\x -> xs) xs) [1..])

{--
Take list of Integers, return list of "local maxima", defined as i_j \in list larger than  i_j-1 and i_j+1
--}
localMaxima		:: 	[Integer] -> [Integer]
localMaxima []		=	[]
localMaxima xs		=	map (\(x,y,z) -> y) (filter (\(x,y,z) -> ((y > x) && (y > z))) (zip3 (tail xs) xs ((head xs):xs)))
