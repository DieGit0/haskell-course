
-- Question 1
-- Write a multiline comment below.
{-
  Hello
  Haskell
  Rules!
-}
-- Question 2
-- Write a function that takes a value and multiplies it by 3. Test that it works using GHCi.
multBy3 = (*) 3 -- f x = x * 3

-- Question 3
-- Write a function that calculates the area of a circle. Test that it works using GHCi.
ciArea r = pi * r^2

-- Question 4
-- Write a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
cyVol r h = ciArea r * h
-- Test that it works using GHCi.
--  that's works

-- Question 5
-- Write a function that checks if the volume of a cylinder is greater than or equal to 42. Test that it works using GHCi.
checkVol r h = cyVol r h >= 42

-- OBS se estiver abrindo direto no windows o caminho é : cd \\wsl.localhost\Ubuntu\home\dgmat\IHaskell\iohk\haskell-course\homework>
main = do 
    print $ multBy3 10 -- 30
    print $ ciArea 4    -- 50.26
    print $ cyVol 4 2   -- 201.06
    print $ checkVol 4 0.8355 -- > False (41.99681059318836
    print $ checkVol 4 0.8356 -- > True  (42.0018371414341