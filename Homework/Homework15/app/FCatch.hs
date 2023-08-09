module FCatch (writeFileSafe, readFileSafe, handleIOException, readInt) where 

import Control.Exception (IOException, catch)
import Data.Char (isSpace)

-- Safe Functions using CATCH
-- catch
--   :: GHC.Exception.Type.Exception e => IO a -> (e -> IO a) -> IO a

 -- map ( \i -> toEnum i ::Char ) [0..65]
-- "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@A"

writeFileSafe :: FilePath -> String -> IO (Maybe ())
writeFileSafe fn _ | all isSpace fn = return <$> putStrLn ( "\n** The File must have a NAME **")
writeFileSafe fn content = (Just <$> writeFile fn content) `catch` handleIOException

readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe fn = (Just <$> readFile fn) `catch` handleIOException
 
handleIOException :: IOException -> IO (Maybe a)
handleIOException _ = do
  putStrLn "An error occurred while accessing the file."
  return Nothing

readInt :: String -> Maybe Int
readInt s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
