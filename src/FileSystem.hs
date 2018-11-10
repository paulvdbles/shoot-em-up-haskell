{-# LANGUAGE OverloadedStrings #-}
module FileSystem where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Model

instance FromJSON Score
instance ToJSON Score where
  toEncoding = genericToEncoding defaultOptions

getFile :: FilePath -> IO BS.ByteString
getFile = BS.readFile

scoreFile :: FilePath
scoreFile = "scores.json"

writeScoreFile :: Scores -> IO ()
writeScoreFile s = BS.writeFile scoreFile (encode (unScore s))

readScoreFile :: IO [Score]
readScoreFile = do
  sf <- eitherDecode <$> getFile scoreFile
  return (getEitherScore sf)

getEitherScore :: Either String [Score] -> [Score]
getEitherScore (Right s) = s
getEitherScore (Left err) = [Score {playerName = "Cannot load scores", playerScore = 0}]
