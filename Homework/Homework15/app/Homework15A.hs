module Main where

import Control.Exception (SomeException, IOException, try, catch)

-- NOTE: The both modules (FTry and FCatch) have the same function names, so they can be interchangeable for testing.
import FTry (writeFileSafe, readFileSafe, readIntEither, readInt)
--import FCatch (writeFileSafe, readFileSafe, handleIOException, readInt)

import Data.Char (isDigit)

printTodoItem :: (Int, String) -> IO ()
printTodoItem (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
  putStrLn ""
  putStrLn "Current TODO list:"
  foldr (\x k -> printTodoItem x >> k) (return ()) (zip [0 ..] todos)
  command <- getLine
  interpretCommand command todos

delete :: Int -> [a] -> [a]
delete 0 (_ : as) = as
delete _ [] = []
delete n (a : as) = a : delete (n - 1) as

interpretCommand :: String -> [String] -> IO ()
interpretCommand cmd todos = case cmd of
  "q" -> return ()

  ('+' : ' ' : todo) -> prompt (todo : todos)

-- NOTE: readIntEither from Try Module
  -- ('-' : ' ' : num)  -> case (readIntEither num) of
  --                       Right n -> if n >= 0 && n < length todos
  --                                   then prompt $ delete n todos
  --                                   else do
  --                                         putStrLn "\n** The given number is outside the range of the TODO-List **"
  --                                         prompt todos
  --                       Left err -> putStrLn err >> prompt todos

  ('-' : ' ' : numStr) ->
    case readInt numStr of
      Just n -> if n >= 0 && n < length todos
                  then prompt $ delete n todos
                  else do
                        putStrLn "\n** The given number is out of range of the TODO-List **"
                        prompt todos
      Nothing -> do
        putStrLn "** Invalid item number **"
        prompt todos                      
  ('s' : ' ' : fn)   -> writeFileSafe fn (show todos) >> prompt todos

  ('l' : ' ' : fn)   -> do result <- readFileSafe fn
                           case result of
                            Just content  -> prompt (read content)
                            Nothing       -> prompt todos

  _ -> do
    putStrLn ("Invalid command: `" ++ cmd ++ "`")
    prompt todos

printCommands :: IO ()
printCommands = do
  putStrLn "Commands:"
  putStrLn "+ <Item Name>   - Add a TODO entry"
  putStrLn "- <Item Number> - Delete the numbered entry"
  putStrLn "s <File Name>   - Save the current list of TODOs"
  putStrLn "l <File Name>   - Load the saved list of TODOs"
  putStrLn "q               - Quit without saving"

main :: IO ()
main = do
  printCommands
  prompt []