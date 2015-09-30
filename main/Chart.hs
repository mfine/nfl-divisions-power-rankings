import Control.Monad
import Data.List
import Results
import System.Environment
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

g :: String -> Integer
g = read . take 2 . drop 5

main :: IO ()
main = do
  args <- getArgs
  rs <- forM args $ \file -> do
    r <- results file
    return (map (\x -> (file, x)) r)
  let ss = sortBy (\(aw, ar) (bw, br) -> compare (rDivision ar) (rDivision br)) (msum rs)
  let ts = groupBy (\(aw, ar) (bw, br) -> rDivision ar == rDivision br) ss
  toFile def "RESULTS-average.png" $ do
    layout_title .= "NFL Power Rankings by Divisions (Average)"
    forM_ ts $ \t ->
      plot (line (rDivision $ snd $ head t) [map (\(w, r) -> (g w, rAverage r)) t])
  toFile def "RESULTS-std-dev.png" $ do
    layout_title .= "NFL Power Rankings by Divisions (Standard Deviation)"
    forM_ ts $ \t ->
      plot (line (rDivision $ snd $ head t) [map (\(w, r) -> (g w, rStdDev r)) t])
