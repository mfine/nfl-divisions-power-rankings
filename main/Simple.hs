import Control.Monad
import Data.List
import Data.Ord
import Results
import System.Environment
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \file -> do
    printf "%-10s %10s %10s\n" ("Division" :: String) ("Mean" :: String) ("Std Dev" :: String)
    printf "%-10s %10s %10s\n" ("--------" :: String) ("----" :: String) ("-------" :: String)
    rs <- results file
    forM_ (sortBy (comparing rAverage) rs) $ \r ->
      printf "%-10s %10.2f %10.2f\n" (rDivision r) (rAverage r) (rStdDev r)
