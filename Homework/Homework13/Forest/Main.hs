module Forest.Main where

--import Forest.Level1
--import Forest.User.Actions.Move
import Forest.Level1 (Forest (..), levelforest)
import Forest.User.Actions.Move(AvailableMoves(..), move)

-- >>> levelforest
-- Trail 3 (Trail 7 (Trail 3 Exit Exit Exit) (Trail 4 Exit Exit Exit) (Trail 5 Exit Exit Exit)) (Trail 3 (Trail 3 Exit Exit Exit) (Trail 9 Exit Exit Exit) (Trail 5 Exit Exit Exit)) (Trail 5 (Trail 3 Exit Exit Exit) (Trail 4 Exit Exit Exit) (Trail 1 Exit Exit Exit))
--  
-- >>> foldl move (10, levelforest) [GoFoward, GoLeft, GoFoward]
-- (1,Exit)
--
--    print $ foldl move (10, levelforest) [GoFoward, GoLeft, GoFoward]

main :: IO ()
main = do
    putStrLn "You are traped in a Forest, try to scape! Remember that you lose stamina every time you take a step."
    gameLoop (10,levelforest)
    where 
        gameLoop (_, Exit)          = putStrLn "CONGRATULAIONS YOU HAVE FOUND THE EXIT!!!"
        gameLoop (s, _   ) | s <= 0 = putStrLn "You ran out the Stamina and Died -.-"
        gameLoop (s, forest)        = do
            putStrLn $ "You have " ++ show s ++ " Stamina, and you're still inside the Forest. Choose a Path:\n"
            selectedMove <- getLine
            gameLoop $ move (s, forest) (read selectedMove :: AvailableMoves)


