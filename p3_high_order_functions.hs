compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

devideBy10 :: (Floating a) =>a ->a
devideBy10 = (/10)

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

applyTwice :: (a->a) ->a ->a
applyTwice f x = f (f x)

zipWithFunction :: (a->a->a) ->[a] ->[a] ->[a]
zipWithFunction _ [] _ = []
zipWithFunction _ _ [] = []
zipWithFunction f (x:xs) (y:ys) = f x y : zipWithFunction f xs ys

containsUpper :: [Char] ->Bool
containsUpper [] = False
containsUpper (x:xs)
  | x `elem` ['A'..'Z'] = True
  | otherwise = containsUpper xs

map' :: (a->b) -> [a] ->[b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) ->[a]->[a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x:filter' f xs
  | otherwise = filter' f xs

quicksort' :: (Ord a) => [a]->[a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerSort = quicksort' (filter (<=x) xs)
      biggerSort = quicksort' (filter (>x) xs)
  in smallerSort ++ [x] ++ biggerSort

largestDevidable :: (Integral a) =>a
largestDevidable = head (filter p [100000,99999..])
  where p x = mod x 3829 == 0

sumOddSquares :: (Integral a) => a->a
sumOddSquares n
  | n < 1 = 0
sumOddSquares n
  | n < 10000   = sum (takeWhile (<n) (filter odd (map (^2) [1..])))
sumOddSquares n = sum (takeWhile (<n) [n^2 | n <- [1..], odd (n^2)])


collatz :: (Integral a) =>a ->[a]
collatz 1 = [1]
collatz n
  | odd n = n:collatz (n*3 + 1)
  | even n = n:collatz (n `div` 2)


numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
  where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- Map
-- map (+3) [1,2,3]
-- let listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) * 5 -->20

-- Lambda
-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

--Fold (Reduce)
sum' :: (Num a) =>[a]->a
sum' xs = foldl (\acc x -> acc + x) 0 xs

map'' :: (a->b)->[a]->[b]
map'' f xs = foldr (\x acc -> f x:acc) [] xs
-- with left
-- map' f xs = foldl (\x acc ->acc ++ f x) [] xs


divideLeft :: (Floating a) => [a]->a
divideLeft xs = foldl1 (\acc x -> acc/x) xs
--divideLeft = foldl1 (/)

divideRight :: (Floating a) =>[a]->a
divideRight xs = foldr1 (\x acc ->acc/x) xs

maximum' :: (Ord a) =>[a]->a
maximum' xs = foldl1 (\acc x -> if x > acc then x else acc) xs

-- scan (map with fold)
-- scanl (+) 0 [3,5,2,1] --> [0,3,5,2,1]
-- scanr (+) 0 [3,5,2,1] --> [11,8,3,1,0]
-- scanl (flip (:)) [] [3,2,1] --> [[],[3],[2,3],[1,2,3]]

sqrtSum :: Int
sqrtSum = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- Function application
-- sum (map (*3) [2,3,4])
-- sum $ map (*3) [2,3,4]

-- map ($ 3) [(4+), (10*), (^2), sqrt] --> [7.0,30.0,9.0,1.7320508075688772]

-- Function Composition
-- map (\x -> negate (abs x)) [4,-5,6,-2]
-- map (negate.abs) [4,-5,6,-2]

-- sum (replicate 5 6)
-- (sum.replicate 5) 6

-- let fn x = ceiling (negate (tan (cos (max 50 x))))
-- let fn = ceiling .negate .tan .cos .max 50
-- fn 5 --> -1.4
