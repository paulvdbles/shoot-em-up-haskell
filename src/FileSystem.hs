module FileSystem where

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

writeScoreFile :: Scores -> IO ()
writeScoreFile s = BS.writeFile scoreFile (encode (unScore s))

readScoreFile :: IO (Either String [Score])
readScoreFile = eitherDecode <$> getJSON
