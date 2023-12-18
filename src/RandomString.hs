module RandomString where
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.ByteArray.Encoding (convertToBase, Base(Base16))

-- Hash String
hashString :: String -> String
hashString input =
  let hashed :: Digest SHA256
      hashed = hash (C8.pack input)
  in show (convertToBase Base16 hashed :: ByteString)
