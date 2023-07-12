{-# LANGUAGE TypeApplications #-}

module Main where

import System.Random (randomRIO)
import Forest.Level2 (Forest (..), level2forest)
import User.Actions.Move (AvailableMoves(..), move)
import User.Actions.Battle ( battle )
import qualified Control.Monad

main :: IO ()
main = do
    statingStamina <- randomRIO @Int (10_000, 20_000)
    putStrLn "====================================================================================================="
    putStrLn "You are traped in a Forest, try to scape! Remember that you lose stamina every time you take a step.\n"
    gameLoop (statingStamina, level2forest)
    where
        gameLoop (_, Exit)          = putStrLn "CONGRATULAIONS YOU HAVE FOUND THE EXIT!!!"
        gameLoop (s, _   ) | s <= 0 = putStrLn "You ran out the Stamina and Died -.-"
        gameLoop (s, forest)        = do
          let continueLoop = do
                putStrLn "====================================================================================================="
                putStrLn $ "You have " ++ show s ++ " Stamina, and you're still inside the Forest. Choose a Path:\nGoLeft | GoFoward | GoRight"
                putStrLn "====================================================================================================="
                selectedMove <- getLine
                gameLoop $ move (s, forest) (read @AvailableMoves selectedMove)
          battleDice <- randomRIO @Int (1, 2)
          case battleDice of
            2 -> do
                 r <- battle -- returns an IO Bool
--                 Control.Monad.when r continueLoop
                 if r then continueLoop else return ()
            _ -> continueLoop


