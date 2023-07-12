{-# LANGUAGE TypeApplications #-}

module User.Actions.Battle (battle) where

import System.Random (randomRIO)
import System.Process (system)

continueLoop :: Int -> Int -> Int -> Int -> IO Bool
continueLoop gh ga uh ua = do
  _ <- system "clear"
  putStrLn "========================================================================"
  putStrLn   "You've encountered a Golem! Choose an action: Fight or RunAway?\n"
  putStrLn $ "The Golem has " ++ show gh ++ " health and " ++ show ga ++ " attack."
  putStrLn $ "You have " ++ show uh ++ " health and " ++ show ua ++ " attack"
  putStrLn "========================================================================"
  putStrLn "Choose an action:" -- \nFight   [1]\nRunAway [2]"
  putStrLn "Fight   [1]"
  putStrLn "RunAway [2]"
  putStr "=>"
  choose <- getLine
  case choose of
    "1" -> do
      damage      <- randomRIO @Int (1, 10)
      golemDamage <- randomRIO @Int (2, 3) 
      userDamage  <- randomRIO @Int (2, 3)

      --putStrLn $ "\nDamage:     " ++ show damage
      --putStrLn $ "Golem Damage: " ++ show golemDamage
      --putStrLn $ "User  Damage: " ++ show userDamage
      let gh_ = gh - damage * userDamage  -- User  Attack Power against Golem
          uh_ = uh - damage * golemDamage -- Golem Attack Power agaisnt User
          ga_ = ga -1
          ua_ = ua -1
      --putStrLn $ "New Golem health: " ++ show gh_
      --putStrLn $ "New User  health: " ++ show uh_
      --putStrLn $ "New Golem attack: " ++ show ga_
      --putStrLn $ "New User  attack: " ++ show ua_
      --_ <- system "sleep 2"

      if (gh_ <= 0 && uh_ > 0) || (uh_ > 0 && ga_ == 0)
        then do
          putStrLn "\nYou've won the battle!"
          putStrLn "The Golem is either dead or he can no longer attack you! O/ [CASE 1]\n"
          putStrLn "Back to the Forest:"
          return True
        else if uh_ <= 0 || ua_ == 0
          then do
            putStrLn "\nYou Died"
            putStrLn "You was dead or can no longer attack!"
            return False
          else continueLoop gh_  ga_ uh_ ua_
    "2" -> do
      escape <- randomRIO (True, False)
      if escape
        then do
          putStrLn "\nYou've managed to run away\n"  -- ++ show escape
          putStrLn "Back to the Forest:"
          return True
        else do
          putStrLn "You've failed to run away! And the Golem hit you!" -- ++ show escape

          damage      <- randomRIO (1, 10) :: IO Int
          golemDamage <- randomRIO (2, 3)  :: IO Int 
          let uh_ = uh - damage * golemDamage
              ga_ = ga - 1 -- The Golem spends one attack

          --putStrLn $ "Golem health:     " ++ show gh
          --putStrLn $ "New Golem attack: " ++ show ga_
          --putStrLn $ "New User health:  " ++ show uh_

          --_ <- system "sleep 3"

          if uh_ <= 0 
            then do putStrLn $ "\nUser health: " ++ show uh_
                    putStrLn   "You Died  -.-"
                    return False
            else if ga_ == 0 
                then do putStrLn "\nYou survived, hey, hey! Although the Golem attacks you again, it cannot attack you anymore"
                        putStrLn "Back to the Forest:"
                        return True                      
          else
           continueLoop gh ga_ uh_ ua
    _ -> do
      putStrLn "[Invalid option]"
      return False

battle :: IO Bool
battle = do
  gh <- randomRIO @Int (1, 100) 
  ga <- randomRIO @Int (1, 10) 
  uh <- randomRIO @Int (1, 100) 
  ua <- randomRIO @Int (1, 10) 
  continueLoop gh ga uh ua
