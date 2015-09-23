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
    rs <- results file
    forM_ (sortBy (comparing rDivision) rs) $ \r ->
      printf "%s,%.2f,%.2f\n" (rDivision r) (rAverage r) (rStdDev r)
