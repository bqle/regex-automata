module RandomString where
import System.Random
import Data.Char (chr)
import Control.Monad (replicateM)

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

-- Example: Generating a random string of length 5
main :: IO ()
main = do
  randStr <- randomUUID
  putStrLn randStr
  randStr <- randomUUID
  putStrLn randStr
  randStr <- randomUUID
  putStrLn randStr
  randStr <- randomUUID
  putStrLn randStr
  
