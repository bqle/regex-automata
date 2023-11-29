module RandomString where
import System.Random
import Data.Char (chr)
import Control.Monad (replicateM)
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.ByteArray.Encoding (convertToBase, Base(Base16))

-- Generate a random string of a fixed length
randomString :: Int -> IO String
randomString len = Control.Monad.replicateM len randomChar

-- Generate a random character
randomChar :: IO Char
randomChar = do
    randomInt <- randomRIO (97, 122)  -- ASCII range for lowercase letters (a-z)
    return $ chr randomInt

-- Random UUID
randomUUID :: IO String
randomUUID = Control.Monad.replicateM 3 randomChar

-- -- Hash String
hashString :: String -> String
hashString input =
  let hashed :: Digest SHA256
      hashed = hash (C8.pack input)
  in show (convertToBase Base16 hashed :: ByteString)

-- Example
main :: IO ()
main = do
  randStr <- randomUUID
  putStrLn randStr
  putStrLn (hashString "hash me a random string please")
  
-- >>> main
