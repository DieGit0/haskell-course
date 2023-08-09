module FTry (writeFileSafe, readFileSafe, readIntEither, readInt) where

import Control.Exception (try, SomeException)
import Data.Char (isDigit, isSpace)

  -- Safe Functions using Try
  -- try :: GHC.Exception.Type.Exception e => IO a -> IO (Either e a)
writeFileSafe :: FilePath -> String -> IO ()
writeFileSafe fn _ | all isSpace fn = putStrLn ( "\n** The file must have a NAME **")
writeFileSafe fn content = do
  result <- try (writeFile fn content) :: IO (Either SomeException ()) -- writeFile :: FilePath -> String -> IO ()
  case result of
    Left  _ -> putStrLn "\n** Error while save file **"
    Right _ -> putStrLn "[File Created Succefully]"

readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe fn = do
  result <- try (readFile fn) :: IO (Either SomeException String) -- readFile :: FilePath -> IO String
  case result of
    Left  _       -> putStrLn "\n** Error while loading file **" >> return Nothing
    Right content -> return (Just content)

readIntEither :: String -> Either String Int
readIntEither str | (null isNum && null notNum) = Left "\n** Void Input **" 
            | length isNum /= length str  = Left "\n** Only Integer Numbers, please **" 
            | otherwise                   = Right (read isNum * 1) -- all inputed String must be Integers
  where (isNum, notNum) = (filter isDigit str, filter (not . isDigit) str)

readInt :: Read a => String -> Maybe a
readInt str = case (reads str) of
               [(num,"")] -> Just num
               _          -> Nothing

r = getLine >>= \s -> return $ (reads s) :: IO [(Integer, String)]