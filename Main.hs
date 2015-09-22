module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Text.Parsec
import Text.Parsec.String
import Text.Printf
import System.Environment
import System.Exit
import qualified Data.Map.Strict as Map

data Ranking = Ranking
  { rRank :: Int
  , rTeam :: String
  } deriving (Eq, Show, Read)

data Results = Results
  { rDivision :: String
  , rAverage  :: Double
  , rStdDev   :: Double
  } deriving (Eq, Show, Read)

afcEast, afcWest, afcSouth, afcNorth :: String
afcEast = "AFC East"
afcWest = "AFC West"
afcSouth = "AFC South"
afcNorth = "AFC North"

nfcEast, nfcWest, nfcSouth, nfcNorth :: String
nfcEast = "NFC East"
nfcWest = "NFC West"
nfcSouth = "NFC South"
nfcNorth = "NFC North"

divisions :: Map.Map String String
divisions = Map.fromList
  [ ("Patriots", afcEast)
  , ("Jets",     afcEast)
  , ("Dolphins", afcEast)
  , ("Bills",    afcEast)

  , ("Bengals",  afcNorth)
  , ("Browns",   afcNorth)
  , ("Steelers", afcNorth)
  , ("Ravens",   afcNorth)

  , ("Broncos",  afcWest)
  , ("Raiders",  afcWest)
  , ("Chargers", afcWest)
  , ("Chiefs",   afcWest)

  , ("Jaguars",    afcSouth)
  , ("Titans",     afcSouth)
  , ("Texans",     afcSouth)
  , ("Colts",      afcSouth)

  , ("Falcons",    nfcSouth)
  , ("Panthers",   nfcSouth)
  , ("Buccaneers", nfcSouth)
  , ("Saints",     nfcSouth)

  , ("Packers",    nfcNorth)
  , ("Vikings",    nfcNorth)
  , ("Lions",      nfcNorth)
  , ("Bears",      nfcNorth)

  , ("Cowboys",    nfcEast)
  , ("Redskins",   nfcEast)
  , ("Giants",     nfcEast)
  , ("Eagles",     nfcEast)

  , ("Cardinals",  nfcWest)
  , ("Rams",       nfcWest)
  , ("49ers",      nfcWest)
  , ("Seahawks",   nfcWest)
  ]

ranking :: Parser Ranking
ranking = do
  rank <- many1 digit
  many space
  team <- many1 alphaNum
  newline
  return $ Ranking (read rank :: Int) team

parser :: FilePath -> IO [Ranking]
parser file =
  parseFromFile (many ranking) file >>= either report return where
    report err = do
      print err
      exitFailure

groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort kvs = Map.toList (Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs])

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

stdDev :: [Int] -> Double
stdDev xs = sqrt $ sum (map f xs) / genericLength xs where
  f x = (realToFrac x - average xs) ** 2

rankings :: FilePath -> IO [Results]
rankings file = do
  rankings' <- parser file
  return $ sortBy (comparing rAverage) $ map (uncurry g) $ groupSort $ map f rankings' where
    f (Ranking r t) = (fromJust $ Map.lookup t divisions, r)
    g d r = Results d (average r) (stdDev r)

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \file -> do
    rankings' <- rankings file
    printf "%-10s %10s %10s\n" ("Division" :: String) ("Mean" :: String) ("Std Dev" :: String)
    printf "%-10s %10s %10s\n" ("--------" :: String) ("----" :: String) ("-------" :: String)
    forM_ rankings' $ \ranking' ->
      printf "%-10s %10.2f %10.2f\n" (rDivision ranking') (rAverage ranking') (rStdDev ranking')
