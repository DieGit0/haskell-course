{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where
import Control.Exception (try,catch, SomeException)
import Data.Char (isDigit, isUpper, isLower)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.
mbls :: [Maybe Integer]
mbls = concat $ replicate 1000000 [Just 1, Just 2, Nothing, Just 3, Nothing, Just 5]

--List Comprehention
catMaybes ::   [Maybe a] ->   [a]
catMaybes ls = [x | Just x <- ls]

-- Recursion
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just x :xs) = x:catMaybes' xs
catMaybes' (Nothing:xs) =   catMaybes' xs

catMaybesTR :: [Maybe a] -> [a]
catMaybesTR [] = []
catMaybesTR (Nothing:xs) = catMaybesTR xs
catMaybesTR (Just x :xs) = catMbT xs [x]
    where catMbT :: [Maybe a] -> [a] -> [a]
          catMbT []           acc = reverse   acc -- foldr (\a b -> a:b) [] acc
          catMbT (Nothing:ts) acc = catMbT ts acc
          catMbT (Just t :ts) acc = catMbT ts $! (t:acc)

len :: [a] -> Int
len []     = 0 
len (_:xs) = 1 + len xs

len2 :: [a] -> Int
len2 []     = 0 
len2 (_:xs) = lenT xs 1
    where     lenT [] n     = n 
              lenT (_:xs) n = lenT xs $! (n+1)

len3 :: [a] -> Int
len3 xs = lenTR xs 0
    where lenTR [] n     = n 
          lenTR (_:xs) n = lenTR xs $! (n+1)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = sumTR xs x -- sum' xs [] => *** Exception: ...
        where sumTR [] r     = r 
              sumTR (n:ns) r = sumTR ns $! (n+r)

-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.

-- Try :: GHC.Exception.Type.Exception e => IO a -> IO (Either e a)
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fn = do r <- try (readFile fn) :: IO (Either SomeException String) 
                      case r of
                        Left _  -> putStrLn "Error While Read the File" >> return Nothing
                        Right s -> return (Just s)
-- ======================================================================================
-- 
-- Catch :: GHC.Exception.Type.Exception e => IO a -> (e -> IO a) -> IO a
readFileMaybe' :: FilePath -> IO (Maybe String)
readFileMaybe' fn = catch (Just <$> readFile fn) handleIOException

handleIOException :: SomeException -> IO (Maybe a)
handleIOException _ = do
  putStrLn "An error occurred while accessing the file."
  return Nothing
-- =======================================================================================
    
-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

newtype PasswordError = PwsError String deriving Show

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough str | length str > 10 = Left $ PwsError "The password must be at least 10 characters long"
                       | otherwise       = Right  str

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit str -- | length (filter isDigit str) >= 1 = Right str
                     -- | not (null (filter isDigit str))  = Right str
                     | any isDigit str = Right str
                     | otherwise       = Left $ PwsError "The password must contain at least one digit"

passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase str | any isUpper str = Right  str
                         | otherwise       = Left $ PwsError "The password must contain at least one uppercase letter"

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase str | any isLower str = Right  str
                         | otherwise       = Left $ PwsError "The password must contain at least one lowercase letter"

passwordRequirements :: String -> Either PasswordError String
passwordRequirements str = do
                    --    passwordLongEnough   str
                    --    passwordHasDigit     str
                    --    passwordHasUppercase str
                    --    passwordHasLowercase str
                          passwordLongEnough str >>= 
                           passwordHasDigit      >>= 
                            passwordHasUppercase >>= 
                             passwordHasLowercase
-- =========================================================

data PasswordError2 = LongEnough | NoDigit | NoUppercase | NoLowercase deriving Show

passwordLongEnough2 :: String -> Either PasswordError2 String
passwordLongEnough2 str | length str > 10 = Left  LongEnough
                        | otherwise       = Right str

passwordHasDigit2 :: String -> Either PasswordError2 String
passwordHasDigit2 str | any isDigit str = Right str
                      | otherwise       = Left  NoDigit

passwordHasUppercase2 :: String -> Either PasswordError2 String
passwordHasUppercase2 str | any isUpper str = Right str
                          | otherwise       = Left  NoUppercase

passwordHasLowercase2 :: String -> Either PasswordError2 String
passwordHasLowercase2 str | any isLower str = Right str
                          | otherwise       = Left  NoLowercase

passwordRequirements2 :: String -> Either PasswordError2 String
passwordRequirements2 str = 
                          passwordLongEnough2 str >>= passwordHasDigit2 >>= passwordHasUppercase2 >>= passwordHasLowercase2
                          --passwordHasLowercase2 =<< passwordHasUppercase2 =<< passwordHasDigit2 =<< passwordLongEnough2 str
