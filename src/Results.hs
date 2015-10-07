module Results
  ( Results (..)
  , results
  ) where

import Data.List
import Data.Maybe
import Data.Ord
import Text.Parsec
import Text.Parsec.String
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
  [ ("PATRIOTS",   afcEast)
  , ("JETS",       afcEast)
  , ("DOLPHINS",   afcEast)
  , ("BILLS",      afcEast)

  , ("BENGALS",    afcNorth)
  , ("BROWNS",     afcNorth)
  , ("STEELERS",   afcNorth)
  , ("RAVENS",     afcNorth)

  , ("BRONCOS",    afcWest)
  , ("RAIDERS",    afcWest)
  , ("CHARGERS",   afcWest)
  , ("CHIEFS",     afcWest)

  , ("JAGUARS",    afcSouth)
  , ("TITANS",     afcSouth)
  , ("TEXANS",     afcSouth)
  , ("COLTS",      afcSouth)

  , ("FALCONS",    nfcSouth)
  , ("PANTHERS",   nfcSouth)
  , ("BUCCANEERS", nfcSouth)
  , ("SAINTS",     nfcSouth)

  , ("PACKERS",    nfcNorth)
  , ("VIKINGS",    nfcNorth)
  , ("LIONS",      nfcNorth)
  , ("BEARS",      nfcNorth)

  , ("COWBOYS",    nfcEast)
  , ("REDSKINS",   nfcEast)
  , ("GIANTS",     nfcEast)
  , ("EAGLES",     nfcEast)

  , ("CARDINALS",  nfcWest)
  , ("RAMS",       nfcWest)
  , ("49ERS",      nfcWest)
  , ("SEAHAWKS",   nfcWest)
  ]

ranking :: Parser Ranking
ranking = do
  _ <- many space
  rank <- many1 digit
  _ <- many space
  team <- many1 alphaNum
  _ <- newline
  return $ Ranking (read rank) team

parser :: FilePath -> IO [Ranking]
parser file =
  parseFromFile (count (length $ Map.keys divisions) ranking) file >>= either report return where
    report err = print err >> exitFailure

groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort kvs = Map.toList (Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs])

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

stdDev :: [Int] -> Double
stdDev xs = sqrt $ sum (map f xs) / genericLength xs where
  f x = (realToFrac x - average xs) ** 2

results :: FilePath -> IO [Results]
results file = do
  rankings <- parser file
  return $ (sortBy (comparing rAverage) $ map (uncurry g) $ ((groupSort $ map e rankings) ++ (groupSort $ map f rankings))) where
    e r = (take 3 $ fromJust $ Map.lookup (rTeam r) divisions, rRank r)
    f r = (fromJust $ Map.lookup (rTeam r) divisions, rRank r)
    g d r = Results d (average r) (stdDev r)
