-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).

{- Parameters: hourly consumption electrical device;
               hours of daily use; 
               maximum monthly consumption allowed
-}
 {-# LANGUAGE MultiWayIf #-}

data  Device = Device {kwh :: Float, hourDay :: Float, maxCom::Float} deriving (Show)

dev = Device { kwh= 50.6, hourDay = 10.0, maxCom = 100}
-- Unpaking the values from dev:
--  kwh dev
--  hourDay dev 
--  maxCom dev
f :: Device -> IO ()
f Device {kwh=0}     = putStrLn "Hourly Consumption: should be bigger than zero"
f Device {hourDay=0} = putStrLn "Hours of Daily Use: should be bigger than zero" 
f Device {maxCom=0}  = putStrLn "Maximum Monthly Consumption: should be bigger than zero"
f Device {kwh = kw, hourDay = day, maxCom = maxCom} =

    let monthly = kw * day * 30
        msg = "\nMontly : " ++ (show monthly) ++ " KW" ++
              "\nMaximum: " ++ (show maxCom)  ++ " KW" ++ "\n"
    in
    if  | monthly > maxCom -> putStrLn $ "[** Bigger **]"  ++ msg
        | monthly < maxCom -> putStrLn $ "[** Smaller **]" ++ msg
        | otherwise ->        putStrLn $ "[** Equal **]"   ++ msg

{-
checksComsumpion :: Float -> Float -> Float -> String
checksComsumpion 0 _ _ = "Hourly Consumption: should be bigger than zero"
checksComsumpion _ 0 _ = "Hours of Daily Use: should be bigger than zero" 
checksComsumpion _ _ 0 = "Maximum Monthly Consumption: should be bigger than zero"
checksComsumpion hkw hday maxCom =
    let monthly = hkw * hday * 30
        msg = show monthly ++ " " ++ show maxCom
    in
    if  | monthly > maxCom -> msg ++ " -> Bigger"
        | monthly < maxCom -> msg ++ " -> Smaller"
        | otherwise ->        msg ++ " -> Equal"
-}

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.

f' :: Device -> IO ()
f' Device {kwh=0}     = putStrLn "Hourly Consumption: should be bigger than zero"
f' Device {hourDay=0} = putStrLn "Hours of Daily Use: should be bigger than zero" 
f' Device {maxCom=0}  = putStrLn "Maximum Monthly Consumption: should be bigger than zero"
f' Device {kwh = kw, hourDay = day, maxCom = maxCom} =

    let monthly = kw * day * 30
        msg = "\nMontly : " ++ (show monthly) ++ " KW" ++
              "\nMaximum: " ++ (show maxCom)  ++ " KW" ++ "\n"
        exSa ix = (!!) ["Excess : ", "Savings: "] ix ++ (show . abs) (monthly - maxCom) ++ " KW"
    in
    if  | monthly > maxCom -> putStrLn $ "[** Bigger **]"  ++ msg ++ (exSa 0)
        | monthly < maxCom -> putStrLn $ "[** Smaller **]" ++ msg ++ (exSa 1)
        | otherwise ->        putStrLn $ "[** Equal **]"   ++ msg


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

-- cylinder's surface area based on its height and radius:

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

-- 
-- This was a small 2nd degree equation example using let I just made to test, I hope it might help..
-- Note: It's so fresh that I didn't have enough time to do more test cases
   
bhaskara a b c =
    let delta   = (b)^2 - 4*a*c
        xs      = ((-(b) + (sqrt delta)) / (2*a), (-(b) - (sqrt delta)) / (2*a))
        proof x = (a)*(x^2) + (b)*x + (c)
    in
    if | delta > 0 && and [proof (fst(xs)) == 0, proof (snd(xs)) == 0] -> show xs
       | delta < 0 && proof (fst(xs)) == 0                             -> show $ fst(xs)
       | otherwise -> show "There are no roots in the set of real numbers."

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.

f4 :: (Ord a, Show a, Fractional a) => a -> a -> Maybe [Char] 
f4 _ 0 = Just "The division is not possible" 
f4 x y | q <= 1    = Just $ show q
       | otherwise = Nothing
    where q = x / y

f4' :: (Ord a, Show a, Fractional a) => a -> a -> Maybe [Char] 
f4' _ 0 = Just "The division is not possible" 
f4' x y = 
    if q <= 1 
        then Just $ show q
        else Nothing
    where q = x / y

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of (squares for the product and quotient)
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
-- f5 x y = (+) ((*) (x^2) (y^2)) ((/) x y) 

f5 :: (Eq a, Show a, Floating a) => a -> a -> String
f5 x y = 
    let sqrtProd = show (sqrt xyProd) where xyProd = x * y
    in  show $ (+) (read sqrtProd) (read sqrtQuot)
    where
        sqrtQuot 
               | y /= 0 = let xyQuot = (/) x y 
                          in show (sqrt xyQuot)
               |otherwise  = "The division is not possible"