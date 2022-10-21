
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.

f1 :: Floating a => a -> a -> a -> a
f1 x y z = x ** (y/z) -- 1 2 0 -> 1 ** (2/0) = 1.0

f2 :: Floating a => a -> a -> a -> a
f2 x y z = sqrt (x/y - z)

f3 :: Show a => Char -> a -> [Char] -> [Char]
f3 x y z = x:(show y ++ z) -- 'a' have to be a instance of Show, show returns a String, so z must be a String that can be concatenated too, therefore x must be a char to be concatenated into a list of Strings

f4 :: (Ord a) => a -> a -> Bool -> [Bool]
f4 x y z = [x > y] ++ [z] -- 'a' must be an instance of Ord to be inferred its ordering in relation to another element of the same type

f5 :: Eq a => [a] -> [a] -> [a] -> Bool
f5 x y z = x == (y ++ z) -- 'a' must have a instance of EQ to be comparable

main = do
    print $ f1 1 2 0 
    print $ f2 200 2 0 
    print $ f3 'C' 3 "PO"
    print $ f4 10 20 True
    print $ f5 "Haskell" "Hask" "ell"

-- Question 2
-- Are really all variables in Haskell immutable? Try googling for the answer.
-- Ans:
-- By default haskell is a pure functionl language, so by definition of this terms variables cannot be changed
-- However without side effects we could not do so much, so I think that we can do some mutabibility with Monads to be able to handle side effects and
-- doing more flexible computations

-- Question 3
-- Why should we define type signatures of functions? How can they help you? How can they help others?
-- ans:
-- Signatures are a good practice to restrict the values that can be entered into a function, ensuring more consistency for input and output data
---In addition, in a bigprint context it helps the development team to have a general and systematic view of the behavior of the application and its external integrations, 
-- as well as it can help teams that could provide support or refactor the code


-- Question 4
-- Why should you define type signatures for variables? How can they help you?
-- Ans: To constraint the nature of this kind of data in order to avoid common errors and provid more control
--  data provid a better documentation


-- Question 5
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
-- Ans:
-- Yeah, here just some few examples bellow:
-- show gets a 'a' and returns a String
--  show (20::Int) ++ "22" == "2022"
-- read gets a String and returns a 'a'
--  read "10" + 10 == 20
--  (read :: String-> Float) "10" == 10.0
-- fromEnum '@' == 65 
-- fromEnum '\xA' == 10

-- Question 6
-- How would you write the prod function from our lesson so that it works for Int and Double? Does the code compile?
-- prod :: Num a => a -> a -> a
-- prod x y = x * y

-- Yeah this works =)

-- Question 7
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
-- Ans: yeah!
-- A common example could be:
-- A String is a Listo of Chars, so a List of Strings is a (List of Lists) 
-- "ABC" : [] == ["ABC"] :: [[Char]]
-- "ABC" : "DEF" : "GHI" : [] :: [[Char]]
-- List of Numbers
-- [123] : [456] : [] :: Num a => [[a]]
-- General Example
-- [] : [] == [[]] :: Bool (True)
