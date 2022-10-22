import Data.List (intercalate)

-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite list with
-- the value provided as every element of the list.
--
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...

repeat' :: a -> [a]
repeat' n = n : repeat' n

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson (comes with Haskell),
-- create a function called `replicate'` that takes a number `n` and a value `x` and creates a list
-- of length `n` with `x` as the value of every element. (`n` has to be Integer.)
--
-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]

-- replicate2 n x = take n $ repeat' x

replicate' n x = (take n . repeat') x

replicate2 n x = [x | _ <-[1..n]] 

replicate3 n x | n > 0 = x : replicate3 (n-1) x
               | otherwise = []

-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.
--
-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]

concat' :: [[a]] -> [a]
concat' xs = [e | x <- xs, e <-x]

-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:
--
-- >>> zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
--
-- >>> zip' [1] ['a', 'b']
-- [(1,'a')]
-- >>> zip' [1, 2] ['a']
-- [(1,'a')]
-- >>> zip' [] [1..]
-- []
-- >>> zip' [1..] []
-- []

zip' ([]) _ = []; zip' _ ([]) = []; zip' (a:as) (b:bs) = (a,b) : zip' as bs

-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]

zipWith' _ [] _ = []; zipWith' _ _ [] = []; zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

-- Question 6
-- Write a function called `takeWhile'` that takes a precate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

takeWhile' _ [] = []; takeWhile' p (x:list) | p x = x : takeWhile' p list | otherwise = []

-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.

-- f n = [1..n] >>= \n -> let show n  ++ "*"

fatShow n = init str ++ "=" ++ fat ++ "!"
      where str = [1..n] >>= \n -> show n ++ "*" 
          --str = concat $ intercalate ["*"] [[show n] | n <-[1..n]]
            fat = show $ foldl (*) 1 [1..n]

--  concat $ Data.List.intercalate ["*"] [[show n] | n <-[1..5]]

--fat2 = show $ foldl (*) 1 [1..5]

-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery. Assume that the two lists have the beers in the same order.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Double)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50

xs = zip' bevogBeerPrices orderList
x = head xs

f ((a,b),(c,d)) = (b,d)
g ((s1, p1), (s2, p2)) = (p1*p2)
f3 ((a,b),(c,d)) = sum [b*d] 

p = (sum . map g) xs
u = sum $ map (\((s1, p1), (s2, p2)) -> p1 * p2) xs
w [] = 0; w ( ((s1, p1),(s2, p2)): ps ) = (p1*p2) + w ps

fx ordL = (+) (calc tup) deliveryCost
   where tup = zip' bevogBeerPrices ordL
         calc [] = 0
         calc (((s1, p1),(s2, p2)):xs) = (p1*p2) + calc xs

main :: IO ()
main = do
--  print $ repeat' 17
--------------------
  print $ replicate' 0 True -- []

  print $ replicate2 (-1) True -- []

  print $ replicate3 4 True -- [True,True,True,True]
---------------------
  print $ concat' [[1,2],[3],[4,5,6]] -- [1,2,3,4,5,6]
---------------------
  print $ zip' [1, 2] ['a', 'b'] -- [(1,'a'),(2,'b')]
  print $ zip' [1] ['a', 'b'] -- [(1,'a')]
  print $ zip' [1, 2] ['a'] -- [(1,'a')]
--  print $ zip' [] [1..] -- []
--  print $ zip' [1..] [] -- []
---------------------
  print $ zipWith' (+) [1, 2, 3] [4, 5, 6] -- [5,7,9]
---------------------
  print $ takeWhile' (< 3) [1,2,3,4,1,2,3,4] -- [1,2]
  print $ takeWhile' (< 9) [1,2,3] -- [1,2,3]
  print $ takeWhile' (< 0) [1,2,3] -- []
---------------------
  print $ fatShow 5
---------------------
  print $ fx orderList