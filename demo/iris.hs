module Main where
import qualified Text.CSV as CSV
import qualified Data.List as L
import qualified DMatrix as D

handleError csv = putStrLn "error parsing"

doWork :: [[String]] -> IO ()
doWork csv = do
  print "Petal.Length"
  print $ map D.strToFloat $ D.row "Petal.Length" csv
  print $ (!!1) $ D.headers csv
  print $ map D.strToFloat $ D.index 1 csv
  print $ (!!2) $ head csv
  print $ map D.strToFloat $ D.index 2 csv
  print $ (!!3) $ head csv
  print $ map D.strToFloat $ D.index 3 csv
  print $ (!!4) $ head csv
  print $ map D.strToFloat $ D.index 4 csv
  print $ (!!5) $ head csv
  print $ L.nub $ D.index 5 csv
  print $ D.factorize $ D.index 5 csv

main :: IO ()
main = do
  let file_name = "data/iris.csv"
  input <- readFile file_name
  let csv = CSV.parseCSV file_name input
  either handleError doWork csv
