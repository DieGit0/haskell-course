import Data.List
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, listDirectory,  getCurrentDirectory, getHomeDirectory, doesPathExist)
import Text.XHtml-- (thead) -- :set -package xhtml ou cabal install xhtml-3000 --lib
import Data.Tree
import System.IO
import Control.Monad --import Data.Matrix as MX(forM, forM_)
--import Data.Matrix 

-- :show linker
-- :show paths

{-
We imported some functions that you'll need to complete the homework.
FilePath is just a synonym for String. Although, make sure to follow the standard path
representation when using them (https://en.wikipedia.org/wiki/Path_(computing).
getCPUTime    :: IO Integer
doesFileExist :: FilePath -> IO Bool
listDirectory :: FilePath -> IO [FilePath]
You can hover over the functions to know what they do.
-}

{-
-- Question 1 --
Define an IO action that counts the number of files in the current directory
and prints it to the terminal inside a string message.
-}
-- import System.Directory as SD
-- SD. + [TAB]

-- ** You may use this function to create some files into current directory **
createFile = do
       putStrLn "How many files do you want to create into the Current Directory (digit 1-9)?"
       c <- getChar
       let n = (read [c]) :: Int
       f n
       where 
        f 0 = putStrLn "\n[All files created]\n"
        f i = f (i-1) >> writeFile ("./file-0" ++ (show i)) "Haskell Rocks"    

listFiles' :: IO ()
listFiles' = do
         getCurrentDirectory >>= 
            listDirectory    >>= 
                \qtd -> putStrLn $ "Total files and Dirs': " ++ show (length qtd)

listFiles :: IO ()
listFiles = do
        lstDir <- listDirectory "."
        let x  =  length lstDir
        putStrLn $ "Total files and Dirs: " ++ (show x)

{-
-- Question 2 --
Write an IO action that asks the user to type something, then writes the message
to a file called msg.txt, and after that, it reads the text from the msg.txt
file and prints it back. Use the writeFile and readFile functions.
-}

createMsg :: IO ()
createMsg = do
  msg <- getLine -- "Hello I'm an IO String :)"
  filePath <- getCurrentDirectory >>= \path -> return (path ++ "/msg.txt")
  writeFile filePath msg
  putStrLn "[msg.txt file created]"
  putStrLn "File content:"
  readFile  filePath >>= putStrLn 

{-
-- Context for Questions 3 and 4 --
In cryptography, prime numbers (positive integers only divisible by themselves and 1) play a fundamental
role in providing unbreakable mathematical structures. These structures, in turn, are leveraged to
establish secure encryption.
But, generating primes is a computational straining problem, as we will measure in the following exercise.
This is because, to know whether a number is a prime number, you first need to know all the previous primes
and then check that they are not a divisor of this number. So, this problem gets bigger and bigger!
Our lead cryptographer provided us with 3 different algorithms (primes1, primes2, and primes3). All three
correctly produce a list of all the prime numbers until a limit (that we provide as a parameter).
Our job is not to understand these algorithms but to measure which is the fastest and print the largest
prime number below our limit. Do it step by step, starting with question 3.
-}

primes1 :: Integer -> [Integer]
primes1 m = sieve [2 .. m]
 where
  sieve [] = []
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes2 :: Integer -> [Integer]
primes2 m = sieve [2 .. m]
 where
  sieve (x : xs) = x : sieve (xs \\ [x, x + x .. m])
  sieve [] = []

primes3 :: Integer -> [Integer]
primes3 m = turner [2 .. m]
 where
  turner [] = []
  turner (p : xs) = p : turner [x | x <- xs, x < p * p || rem x p /= 0]

{-
-- Question 3 --
Define an IO action that takes an IO action as input and calculates the time it takes to execute.
Use the getCPUTime :: IO Integer function to get the CPU time before and after the IO action.
The CPU time here is given in picoseconds (which is 1/1000000000000th of a second).
-}
-- ** getCPUTime >>= \t1 -> getCPUTime >>= \t2 -> return $ t2- t1
timeIO :: IO a -> IO ()
timeIO io = do
  t1 <- getCPUTime
  _ <- io
  t2 <- getCPUTime
  let elapsedTime = fromIntegral (t2 - t1) / 1000000000000
  print $ elapsedTime

-- timeIO getLine
{-
-- Question 4 --
Write an action that retrieves a value from the standard input, parses it as an integer,
and compares the time all three algorithms take to produce the largest prime before the
limit. Print the number and time to the standard output.
-}
-- do x <- getLine; let n = (read x + 1) in print n
-- do x <- getLine; let n = (read x)::Int in print n

benchmark :: IO ()
benchmark = 
  do
    x <- getLine
    let n = (read x) :: Integer
    (timeIO . print . last . primes1) n 
    (timeIO . print . last . primes2) n
    (timeIO . print . last . primes3) n

benchmark' :: IO ()
benchmark' = 
  do
    x <- getLine
    let n = (read x) :: Integer
    
    t1 <- getCPUTime
    print $ primes1 n
    t2 <- getCPUTime 

    putStrLn ""

    t3 <- getCPUTime
    print $ primes2 n
    t4 <- getCPUTime 

    putStrLn ""

    t5 <- getCPUTime
    print $ primes3 n
    t6 <- getCPUTime  

    putStrLn ""

    putStrLn "[Elapsed Time:]"
    putStrLn $ "Prime1: " ++ show (fromIntegral (t2 - t1) / 1000000000000)
    putStrLn $ "Prime2: " ++ show (fromIntegral (t4 - t3) / 1000000000000)
    putStrLn $ "Prime3: " ++ show (fromIntegral (t6 - t5) / 1000000000000)

main = do
  createFile
  listFiles
  listFiles'
  putStrLn "\nType a message:"
  createMsg
  putStrLn "\nTimeIO:"
  timeIO getLine
  putStrLn "\nBenchmarck - type a Integer:"
  benchmark
  putStrLn "\nBenchmarck - All primes printed - type a Integer:"
  benchmark'



{-
 -- Question 5 -- EXTRA CREDITS -- (In case the previous ones were too easy)
Write a program that prints the directory tree structure from the current folder.
Below you can see an example output of how such a structure looks like:
.
├── foo1.hs
├── foo2.hs
├── bar1
    ├── bar2
    ├── foo3.hs
    ├── foo4.hs
    └── bar3
        └── foo5.hs
└── bar5
    ├── bar6
    ├── foo6.hs
    └── foo7.hs
HINT: You can use the function doesFileExist, which takes in a FilePath and returns
True if the argument file exists and is not a directory, and False otherwise.
-}
-- getCurrentDirectory >>= listDirectory >>= \z -> putStr $ show $ toHtml z
-- getCurrentDirectory >>= listDirectory >>= print . toHtmlFromList
folderTree = getCurrentDirectory >>= listDirectory >>= print . toHtml

-- data HtmlTree = HtmlLeaf Html | HtmlNode Html [HtmlTree] Html
-- treeHtml :: [String] -> HtmlTree -> Html
-- https://hackage.haskell.org/package/tree-view-0.5.1/docs/Data-Tree-View.html#v:htmlTree
d  = drawTree $ Node "Add" [Node "Sub" [Node "3" [], Node "Mul" [Node "1" [], Node "2" []]], Node "4" []]
d' = putStr $ drawTree $ Node "Add" [Node "Sub" [Node "3" [], Node "Mul" [Node "1" [], Node "2" []]], Node "4" []]
dirPlusFile = fmap ( "./" ++ ) ["msg.txt","file-01","file-02","dir-01","escrita_arq.hs","homework.hs","file-03","op_direta_arq.hs"]