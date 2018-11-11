{-# LANGUAGE OverloadedStrings #-}
module FileSystem where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Model
import           System.Directory     (getTemporaryDirectory, renameFile)

instance FromJSON Score
instance ToJSON Score where
  toEncoding = genericToEncoding defaultOptions

getFile :: FilePath -> IO BS.ByteString
getFile = BS.readFile

scoreFile :: FilePath
scoreFile = "scores.json"

writeScoreFile :: [Score] -> IO ()
writeScoreFile s = do
  tmpDir <- getTemporaryDirectory
  let tmpFile = tmpDir ++ "/" ++ scoreFile
  BS.writeFile tmpFile (encode s)
  renameFile tmpFile scoreFile

readScoreFile :: IO [Score]
readScoreFile = do
  sf <- eitherDecode <$> getFile scoreFile
  return (getEitherScore sf)

getEitherScore :: Either String [Score] -> [Score]
getEitherScore (Right s) = s
getEitherScore (Left err) = [Score {playerName = "Cannot load scores", playerScore = 0}]
