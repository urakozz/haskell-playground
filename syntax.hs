doubleMe x = x + x
doubleUs x y = x + x +y +y

doubleSmall x = if x > 100 then x else doubleMe x
doubleSmall' x = (if x >= 100 then x else doubleMe x) + 1

lucky :: (Integral a) => a -> String
lucky 7 = "SEVEN!"
lucky 8 = "--8!"
lucky x = if x > 100 then "too much" else "Sorry!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial x = product [1 .. x]

sayMe :: (Integral a, Show a) =>a ->String
sayMe 0 = "zero"
sayMe 1 = "one"
sayMe n = "that [" ++ (show n) ++ "] is too much"

speller :: (Char ) -> String
speller 'a' = "Alb"
speller 'b' = "Bor"
speller 'c' = "Car"
speller n = "hz " ++ (show n)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1)(x2,y2) = (x1+x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

head' :: (Num a) => [a] -> a
head' [] = error "empty list"
head' (_:[]) = error "only one elem"
head' (x:y:_) = x + y

len' :: (Num a) => [a] -> a
len' [] = 0
len' (_:xs) = 1+len' xs

sum' :: (Num a) => [a] ->a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

sumStr :: (Num a, Show a) => [a] -> String
sumStr [] = show 0
sumStr all@(x:xs) = "Input :" ++ show all ++ ", sum: " ++ show (x + sum xs)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
  | bmi <= s = "low"
  | bmi <= m = "ok"
  | bmi <= l = "much"
  | otherwise = "overload"
  where bmi = w/h^2
        (s, m, l) = (19, 25, 30)

max' :: (Ord a) => a->a->a
max' a b
  | a>b = a
  | otherwise = b

initials :: String -> String -> String
initials firstn lastn = [f] ++ [l]
  where (f:_) = firstn
        (l:_) = lastn

calcBmis :: (RealFloat a) => [(a,a)] ->[a]
calcBmis xs = [(bmi w h)| (w,h)<-xs]
  where bmi we he = we/he^2

cyl :: (RealFloat a) => a->a->a
cyl r h =
  let side = 2*pi*r*h
      top = pi*r^2
  in side + 2*top

cases:: (Show a)=>[a]->String
cases xs = case xs of [] -> error "empty"
                      (x:_) -> show x

descList :: (Show a)=> [a] -> String
descList xs = "List case is: " ++ what xs
  where what [] ="empty"
        what [x] = "single, only " ++ show x
        what xs = "long " ++ show xs
