-- Create a higher-order function that takes 3 parameters: A function and the two parameters that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`

--f1 :: (a -> b -> c) -> a -> b -> c
--f1 f x y = f y x
{- 
 • Couldn't match expected type ‘a’ with actual type ‘b’
      ‘b’ is a rigid type variable bound by
        the type signature for:
          f1 :: forall a b c. (a -> b -> c) -> a -> b -> c
-}
-- ** Resposta: o erro ocorre porque ao inverter os parametros, a funcao f espera 'a' 'b', e não 'b' 'a' definidos na assinatura (a -> b -> c)

flip :: (a -> b -> c) -> b -> a -> c  
flip f y x = f x y  -- recebe 'b' 'a' e aplica a funcao na ordem esperada 'a' 'b' em f

f1 :: (a -> b -> c) -> b -> a -> c
f1 f x y = f y x -- (a -> b)
--    b a     a b

flip' :: (a -> b -> c) ->  b -> a -> c  
flip' f = Prelude.flip f -- exemplo: flip inverte a ordem que é aceita por f

flip'' :: (a -> b -> c) -> b -> a -> c  
flip'' f = (\b a -> f a b)

-- print $ f1 (/) 6 2 -- `0.3333333333`

-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x,y) =  f x y

f2 :: (a -> b -> c) -> (a,b) -> c
f2 f (a,b) = f a b 
-- -----------------------------------------------
f2' :: (a -> a -> a) -> a -> a -> a
f2' f = f -- f is a curring expecting two parameters - ok

x =  f2' (+) 1 -- x :: Num a => a -> a
-- x 2 --> 3
-- -----------------------------------------------

-- print $ f2 (+) (1,2)

-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y =  f (x, y)

fstCurring :: ((a, b) -> a) -> a -> b -> a
fstCurring f x y = f (x, y)


-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 

hasUppercase :: String -> Bool
hasUppercase = any (`elem` ['A'..'Z'])

f4 :: String -> Bool
f4 = or . (<$>) ( `elem` ['A'..'Z'])

-- Those below only counts True occorrencies
f4' :: String -> [Bool]
f4' [] = []
f4' (x:xs) = elem x ['A'..'Z'] : f4' xs

f4'' :: String -> [Bool]
f4'' str = [True | s <- str, elem s ['A'..'Z']]

-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

data Team = Red | Blue | Green deriving (Show, Eq)

count :: Team -> Int
count x = length [ 1 | v <- votes, show x == v]

count' :: Team -> Int
count' x = sum   [ 1 | v <- votes, show x == v] 

count'' :: Team -> [String] -> Int
count'' _ [] = 0
count'' x (v:vs) | show x == v = (+) 1 $ count'' x vs
                 | otherwise = count'' x vs

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

anyXCarsLeft :: String -> Bool
anyXCarsLeft x = any (\(_, cars) -> cars > 0) . filter (\(brand, _) -> brand == x) $ cars

anyXCarsLeft' x = [ True | (br,n) <- cars, and [x == br, n > 0]] 

-- -----------------------------------

xs = ["aaaa","bbbb","cccc","dddd","eeee"]

drop3list :: [String] -> [String]
drop3list lst = case lst of
  (_:_:_:z) -> z     -- ([_,_,_,d])
  _         -> []

main = do 
  print $ drop3list xs