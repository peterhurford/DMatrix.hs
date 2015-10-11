module Main where
import qualified Text.CSV as CSV
import qualified Data.List as L
import DMatrix

handleError csv = putStrLn "error parsing"

doWork :: [[String]] -> IO ()
doWork csv = do
  print "Petal.Length"
  print $ map strToFloat $ row "Petal.Length" csv
  print $ (!!1) $ headers csv
  print $ map strToFloat $ index 1 csv
  print $ (!!2) $ head csv
  print $ map strToFloat $ index 2 csv
  print $ (!!3) $ head csv
  print $ map strToFloat $ index 3 csv
  print $ (!!4) $ head csv
  print $ map strToFloat $ index 4 csv
  print $ (!!5) $ head csv
  print $ L.nub $ index 5 csv
  print $ factorize $ index 5 csv

main :: IO ()
main = do
  let file_name = "data/iris.csv"
  input <- readFile file_name
  let csv = CSV.parseCSV file_name input
  either handleError doWork csv
