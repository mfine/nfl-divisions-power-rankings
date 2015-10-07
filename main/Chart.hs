import Control.Monad
import Data.List
import Data.Ord
import Results
import System.Environment
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = do
  args <- getArgs
  rs <- forM args $ \file -> do
    r <- results file
    return (map (\x -> (g file, x)) r)
  let ss = sortBy (comparing (rDivision . snd)) (msum rs)
  let ts = groupBy (\a b -> rDivision (snd a) == rDivision (snd b)) ss
  toFile def "RESULTS-average.png" $ do
    layout_title .= "NFL Power Rankings by Divisions (Average)"
    colors'
    forM_ ts $ \t ->
      plot $ line (rDivision $ snd $ head t) [map (\(w, r) -> (w, rAverage r)) t]
  toFile def "RESULTS-std-dev.png" $ do
    layout_title .= "NFL Power Rankings by Divisions (Standard Deviation)"
    colors'
    forM_ ts $ \t ->
      plot $ line (rDivision $ snd $ head t) [map (\(w, r) -> (w, rStdDev r)) t] where
        g :: String -> Integer
        g = read . take 2 . drop 5
        colors' =
          setColors
            [ opaque blue
            , opaque red
            , opaque cyan
            , opaque green
            , opaque yellow
            , opaque gray
            , opaque pink
            , opaque black
            , opaque purple
            , opaque orange
            ]

