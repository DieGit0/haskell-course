module Forest.Level2 where

data Forest a = Exit | Trail a (Forest a) (Forest a) (Forest a) deriving Show

level2forest :: (Ord a, Num a) => Forest a
level2forest =
    Trail 
      3_000
        (Trail 
          7_000
           (Trail 3_000 Exit Exit Exit)
           (Trail 4_000 Exit Exit Exit)
           (Trail 5_000 Exit Exit Exit)
        )
        (Trail 
          3_000
           (Trail 3_000 Exit Exit Exit)
           (Trail 9_000 Exit Exit Exit)
           (Trail 5_000 Exit Exit Exit)
        )
        (Trail 
          5_000 
           (Trail 3_000 Exit Exit Exit)
           (Trail 4_000 Exit Exit Exit)
           (Trail 1_000 Exit Exit Exit)
        )