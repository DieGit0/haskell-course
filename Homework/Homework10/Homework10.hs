-- Continuing with the logistics software of the lesson:
--  1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
--   	- First, write down the `Container` type class and its instances, same as we did in the lesson
--   	  (try to do it without looking and check at the end or if you get stuck).
--   	- Then, add a function called `unwrap` that gives you back the value inside a container.

data Box a       = Empty          | Has a          deriving Show
data Present t a = EmptyPresent t | PresentFor t a deriving Show

class Container c where
  isEmpty  :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace  :: c a -> b -> c b
  unwrap   :: c a -> Maybe a

-- Container Box instance:
instance Container Box where
  isEmpty  Empty     = True 
  isEmpty   _        = False
  contains (Has x) y = x == y
  contains  _      _ = False
  replace   _      y = Has y
  unwrap (Has x)     = x 
  unwrap Empty       = Empty -- ** Error: Or x Or Box
-- Maybe:
--  unwrap (Has x)  = Just x   -- unwrap (Has 100) |> Just 100 
--  unwrap Empty    = Nothing  -- unwrap Empty     |> Nothing
-- ============================

-- Container Present instance:
instance Container (Present t) where
    isEmpty  (EmptyPresent _)    = True   -- isEmpty (EmptyPresent "voce")
    isEmpty   _                  = False  -- isEmpty (PresentFor "voce" "PC")

    contains (PresentFor  _ a) b = a == b -- contains (PresentFor "Antonio" "Saúde") "Saúde"
    contains  _                _ = False  -- contains (EmptyPresent "Marco) "Saúde"

    replace  (PresentFor   t _) b = PresentFor t b -- replace (PresentFor "You" "Azar") "Mega-Sena"
    replace  (EmptyPresent t)   b = PresentFor t b 

    -- Maybe
    unwrap   (PresentFor  _ a)   = Just a  -- unwrap (PresentFor "Me" "Saude e Dinheiro e Paz") 
    unwrap   (EmptyPresent _ )   = Nothing -- unwrap (EmptyPresent "Someone else")

--  2. Create an instance for the `MailedBox` data type.
--  	- The MailedBox data type represents a box sent through the mail.
--  	- The parameter `t` is a tag with a person's identifier
--  	- The parameter `d` is the person's details (address,etc).
--  	- The parameter `a` is the content of the MailedBox

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a deriving Show

-- MailBoxTo    "Tag" ("Adress", 10) "Message..."
-- EmptyMailBox "Tag" ("Adress",10)

--  Using Box
-- MailBoxTo "Tag" "Adress" (Has "Message")
-- MailBoxTo "Tag" "Adress" (Empty)

instance Container (MailedBox t d) where
   isEmpty  (EmptyMailBox _ _  ) = True   -- isEmpty (EmptyMailBox "Tag" ("Adress",10))
   isEmpty  (MailBoxTo    _ _ _) = False  -- isEmpty (MailBoxTo "Tag" ("Adress",18) "Message...")

   contains (MailBoxTo _ _ a) b  = a == b -- contains (MailBoxTo "Tag" ("Adress",26) "Message...") "Message..."

   replace  (MailBoxTo t d a) b  = MailBoxTo t d b -- replace (MailBoxTo "Tag" ("Adress",12) "Hi") "Hello"

   unwrap   (MailBoxTo _ _ a)    = Just a  -- unwrap (MailBoxTo "Tag" ("Adress",10) "Message...")
                                           -- unwrap (MailBoxTo "Tag" "Adress" (Has "Message")) --Box
   unwrap   (EmptyMailBox _ _)   = Nothing -- unwrap (EmptyMailBox "Tag" ("Adress",10))
                                           -- unwrap MailBoxTo "Tag" "Adress" (Empty) --Box
-- ======================================================================================================================

-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Middle |Senior | Manager | Chief deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

type Address = String

data Salary = USD Double | EUR Double deriving (Show, Eq, Ord)

convert :: Salary -> Salary
convert (USD x) = EUR (x * 0.9374)
convert (EUR x) = USD (x * 1.0667)

data Relationship
  = Contractor Position Experience Salary Address
  | Employee   Position Experience Salary Address
    deriving Show

--Tests:
p1 = (Junior == Senior)
p2 = (Middle < Intern)
p3 = show Manager

e1 = (Programming /= Managing)
e2 = (Leading < Programming)
e3 = show Programming

con1 = Contractor Manager Managing (EUR 16000.0) "Santa Claus St. 26"    
con2 = Contractor Chief   Leading   (USD 20000.0) "Santa Claus St. 26"  
emp1 = Employee   Middle  Programming (EUR 4200.0) "Santa Claus St. 26" 
emp2 = Employee   Senior  Programming (USD 8600.0) "Santa Claus St. 26" 
-- =================================================================================

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } 
    deriving Show

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- -- Team memeber experience in years
newtype Exp = Exp Double deriving (Show)
--
-- -- Team memeber data
type TeamMember = (String, Exp) 
--
-- -- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]
--
-- -- Function to check the combined experience of the team
-- -- This function applied to `team` using GHCi should work

-- {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
instance (Num Exp) where
 (+) (Exp x) (Exp y) = Exp (x + y)       -- (+) (Exp 9) (Exp 1) | (-) (Exp 9) (Exp 1)
 (*) (Exp x) (Exp y) = Exp (x * y)       -- (+) (Exp 9) (Exp 2)
 negate (Exp x) = (*) (Exp x) (Exp (-1)) -- negate (Exp 9) | negate (Exp (-9))
 fromInteger x = Exp (fromInteger x)     -- fromInteger 1 :: Exp
 abs    (Exp x) | x > 0     = Exp x      -- abs $ negate (Exp 9) 
                | otherwise = Exp (negate x)
 signum (Exp x) | x > 0     =    1  -- signum (Exp 10) 
                | otherwise =  (-1) -- signum (Exp (-10)) 

combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0 

combineExp' :: [TeamMember] -> Exp
combineExp' xs = Exp $ foldr (\(_,(Exp n)) -> (+) n) 0 xs

combineExp'' :: [TeamMember] -> Exp 
combineExp'' [(st,(exp ))]  = (+) (exp) (Exp 0) 
combineExp'' ((st, exp):xs) = (+) (exp) (combineExp'' xs)

combineExp''' :: [TeamMember] -> Exp
combineExp''' [(st,(Exp x))]  = (+) (Exp x) (Exp 0) 
combineExp''' ((st,Exp x):xs) = (+) (Exp x) (combineExp'' xs)

f :: Exp -> Double; f (Exp d) = (d :: Double)

main :: IO()
main = do
  print $ combineExp team
  print $ combineExp' team
  print $ combineExp'' team
  print $ combineExp''' team