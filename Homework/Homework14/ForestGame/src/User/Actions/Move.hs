
module User.Actions.Move where

--import Forest.Level1  (Forest(..))
import Forest.Level2  (Forest(..))

data AvailableMoves = GoLeft | GoFoward | GoRight deriving (Show, Read)

move :: (Num a) => (a, Forest a) -> AvailableMoves -> (a, Forest a)
move (s, Exit) _                 = (s, Exit)
move (s, Trail a l _ _) GoLeft   = (s - a, l)
move (s, Trail a _ f _) GoFoward = (s - a, f)
move (s, Trail a _ _ r) GoRight  = (s - a, r)