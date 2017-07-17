--
-- Section 1:
-- VALIDATING CREDIT VARD NUMBERS
--
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
validate :: Integer -> Bool

toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : toDigits (n `div` 10)

toDigits n = reverse (toDigitsRev n)

{-
	doubleEveryOther: double every other Int in a list of them, starting from the right
	Take the list:
		zip it with integers 1 .. Infinity, (start at 2 to "start from the right")
		Look at these indices % 2 to figure out "every other digit", filter out the rest
		map to get rid of the indices, and to double
-}
doubleEveryOther n = map (\x -> (fst x) * 2) (filter (\x -> (snd x) `mod` 2 == 0) (zip n [2..]))

{-
	sumDigits: sums all digits in the list of numbers
	Take the list:
		split all numbers into digits
		sum each resulting array of digits
		sum all resulting sums
-}
sumDigits x = sum (map sum (map (\a -> toDigits a) x))

-- Full algorithm
validate n = (sum (doubleEveryOther (toDigitsRev n))) `mod` 10 == 0

--
-- Section 2:
-- THE TOWERS OF HANOI
--
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

{-
	hanoi: solve the Towers of Hanoi problem with the following algorithm
	Alg (to move n discs a -> b with c as temp storage):
		move n - 1 discs a -> c with b as temp storage
		move top disc a -> b
		move n - 1 discs c -> b using a as temp storage
-}

hanoi 1 s d t = [(s, d)]
hanoi n s d t = (hanoi (n-1) s t d) ++ [(s,d)] ++ (hanoi (n-1) t d s)
