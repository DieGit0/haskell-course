{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

data Move = GoForward | GoLeft | GoRight deriving (Show,Eq)

data Tree a = FoundExit | Passage a (Tree a) (Tree a) (Tree a) deriving (Eq,Show)

forest:: Tree Int
forest = Passage 3
  (Passage 4 -- Left
    (Passage 3 FoundExit (Passage 2 FoundExit FoundExit FoundExit) FoundExit)
    (Passage 1 (Passage 2 FoundExit FoundExit FoundExit) FoundExit FoundExit)
    (Passage 2 (Passage 5 FoundExit FoundExit FoundExit) FoundExit (Passage 4 FoundExit FoundExit FoundExit)) -- [GoLeft, GoRight, GoForward  ]
  )
  (Passage 5 -- Center
    (Passage 2 
      (Passage 4 FoundExit FoundExit FoundExit) 
       FoundExit 
      (Passage 1 FoundExit FoundExit FoundExit) 
    )
    (Passage 3 FoundExit (Passage 1 FoundExit FoundExit FoundExit) FoundExit)
    (Passage 1 FoundExit FoundExit FoundExit)
  )
    (Passage 2 -- Right
      (Passage 4 
        (Passage 1 FoundExit FoundExit FoundExit) 
        (FoundExit) -- [GoRight, GoLeft, GoForward  ]
        (Passage 5 FoundExit FoundExit FoundExit)
      )
    (Passage 9 FoundExit FoundExit FoundExit)
    (Passage 10 FoundExit FoundExit FoundExit)
  )

move :: Tree Int -> Move -> (Int,Tree Int)
move (FoundExit)     _           = (0,FoundExit)
move (Passage x l _ _) GoLeft    = (x,l) 
move (Passage x _ f _) GoForward = (x,f)
move (Passage x _ _ r) GoRight   = (x,r) 

move' :: Tree a -> Move -> Either (Tree a) (a,Tree a)
move'  (FoundExit)  _              = Left FoundExit
move'  (Passage x l _ _) GoLeft    = Right (x,l) -- Left l
move'  (Passage x _ f _) GoForward = Right (x,f)
move'  (Passage x _ _ r) GoRight   = Right (x,r) -- Left r

opForward = (move' forest GoForward) >>= \(n,fr) -> Right (10-n,fr) 

type Stamina = Int
stamina = 10 :: Stamina

solveForest ::(Stamina,Tree Int) -> [Move] -> String 
solveForest   (_, FoundExit) _       = "YOU'VE FOUND THE EXIT!!" 
solveForest   (st,  _      ) []      = "You have " ++ (show st) ++ " stamina" 
solveForest   (st, fo) (step:moves)             
                | continue           = solveForest (st-x, f) moves
                | otherwise          = "You ran out of stamina and died -.-!"
          where (x,f)    = move fo step  -- (Int,Tree Int)
                continue = (st-x) > 0 

solveForest' ::(Stamina,Tree Int) -> [Move] -> String -- (Int,Tree Int)
solveForest'   (_, FoundExit) _       = showMsg (stamina,FoundExit)
solveForest'   (st,  _      ) []      = showMsg (st, Passage 0 FoundExit FoundExit FoundExit)   
solveForest'   (st, fo) (step:moves)             
                | continue           = solveForest' (st-x, f) moves
                | otherwise          = showMsg (st, Passage 0 FoundExit FoundExit FoundExit)    
          where (x,f)    = move fo step  -- (Int,Tree Int)
                continue = (st-x) > 0 -- Bool

showMsg :: (Stamina,Tree Int) -> String
showMsg (_,FoundExit)  = "YOU'VE FOUND THE EXIT!!"
showMsg (st, _) 
            | st > 0    = "You have " ++ (show st) ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
            | otherwise = "You ran out of stamina and died -.-!"

-- * Test Cases:
-- solveForest (stamina, forest) []
-- solveForest (stamina, FoundExit ) []

-- r = Right (5,Passage 7 (Passage 8 FoundExit FoundExit FoundExit) (Passage 9 FoundExit FoundExit FoundExit) (Passage 10 FoundExit FoundExit FoundExit))
-- r >>= \(n,tr) -> return n 
-- do (n,tr) <- r;  return n



