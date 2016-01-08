maximum' :: (Ord a) => [a]->a
maximum' [] = error "list is empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

maximum'' :: (Ord a) => [a]->[a]->a
maximum'' [] [] = error "both are empty"
maximum'' x [] = error "second is empty"
maximum'' [] x = error "first is empty"
maximum'' x1 x2 = max (maximum' x1) (maximum' x2)

array_fill :: (Num i, Ord i) => i -> a->[a]
array_fill x n
  | x <= 0 = []
  | otherwise = n:array_fill (x-1) n


take' :: (Num i , Ord i) => i ->[i]->[i]
take' n _
 | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a]->[a]
reverse' [] = []
reverse' (x:xs) = xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

quicksort :: (Ord a) => [a]->[a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSort = quicksort [a | a <- xs, a <= x]
      biggerSort = quicksort [a | a <- xs, a>x]
  in smallerSort ++ [x] ++ biggerSort
