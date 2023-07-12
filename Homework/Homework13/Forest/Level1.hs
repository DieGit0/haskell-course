module Forest.Level1 where

data Forest a = Exit | Trail a (Forest a) (Forest a) (Forest a) deriving Show

levelforest :: (Ord a, Num a) => Forest a
levelforest =
    Trail 
      3 
        (Trail 
          7 
           (Trail 3 Exit Exit Exit)
           (Trail 4 Exit Exit Exit)
           (Trail 5 Exit Exit Exit)
        )
        (Trail 
          3 
           (Trail 3 Exit Exit Exit)
           (Trail 9 Exit Exit Exit)
           (Trail 5 Exit Exit Exit)
        )
        (Trail 
          5 
           (Trail 3 Exit Exit Exit)
           (Trail 4 Exit Exit Exit)
           (Trail 1 Exit Exit Exit)
        )