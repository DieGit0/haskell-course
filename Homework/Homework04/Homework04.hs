-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

f :: [(a, [b])] -> b
f [(_,[_,c]),(_)] = c  -- 4

f' :: [(a, [b])] -> [b]
f' [(_,(_:c)),(_)] = c -- [4]

--let [(_,[_,c]),(_)] = nested in f c  -- 4 
--let [(a,[b,c]),(d,e)] = nested in c  -- 4
--let [(a,(b:c)),(d,e)] = nested in c  -- [4]

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
lstInt  = [1,2,3,4]
lstBool = [True,False,True] 
lstChar = ['A','B','C'] 
lstStr = ["ABC","DEF","G1$"] 

-- let [a,b,c,d] = [1,2,3,4] in [a,b] -- [1,2] | tem que casar todos os elementos se nao da erro
-- let (a:b:_) = [1,2,3,4] in [a,b] -- [1,2]
-- Exemplo Interessante: let (a:b:_) = ['1','2','3','4'] in [(read [a]) + (read [b])] --> [3]

getOnlyLess3th :: [a] -> [a]
getOnlyLess3th (a:b:_) = [a,b] -- if the list has 3 OR MORE elements, it REMOVES them (returning just up to 2th ones(?))
getOnlyLess3th (a:_)   = [a]   
getOnlyLess3th  _      = []    -- it does nothing

getOnlyLess3th' lst = case lst of
    (a:b:_) ->  Just [a,b]
    (a:_)   ->  Just [a] 
    lst     ->  Nothing

--g  (_:_:_:xs) = [1,2,3,4] in xs -- > [4]

--g' [_,_,_,xs] = [1,2,3,4] in xs -- > 4

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together

sum3Elems :: (Int, Int, Int) -> Int
sum3Elems (a,b,c) = a + b + c

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
isEmpityLst:: [a] -> Bool   
isEmpityLst [] = True
isEmpityLst _  = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.

-- **OBS o pattern tail' [_:xs] = xs apenar serve para strings, nao serve para numeros, ex: "ABC" -> "BC".
-- Já [1,2,3] retorna erro     
--  let [_:xs, zs] = ["123", 456"] in xs -- [23]
--  let [_:xs, zs] = ["123", 456"] in xs -- [23]

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)

takeInt :: Int -> Int
takeInt n = case (even n) of
    True -> n + 1
    _    -> n
