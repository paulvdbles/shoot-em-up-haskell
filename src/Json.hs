module Json where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Model                (Score, Scores, unScore)

instance ToJSON Score where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Score

scoreFile :: FilePath
scoreFile = "scores.json"

getJSON :: IO BS.ByteString
getJSON = BS.readFile scoreFile

-- TODO add type here
writeJSON :: Scores -> IO ()
writeJSON s = BS.writeFile scoreFile (encode (unScore s))

loadJSON :: IO (Either String [Score])
loadJSON = eitherDecode <$> getJSON
