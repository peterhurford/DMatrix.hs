module Main where
import qualified Text.CSV as CSV
import qualified Data.List as L
import qualified DMatrix as D

handleError csv = putStrLn "error parsing"

doWork :: [[String]] -> IO ()
doWork csv = do
  let df = D.DataFrame { D.headers = head csv, D.body = D.listBody csv }
  print "ID"
  print $ map D.strToFloat $ D.column "ID" df
  print "Petal.Length"
  print $ map D.strToFloat $ D.column "Petal.Length" df
  print $ (!!1) $ D.headers df
  print $ map D.strToFloat $ D.index 1 df
  print $ (!!2) $ D.headers df
  print $ map D.strToFloat $ D.index 2 df
  print $ (!!3) $ D.headers df
  print $ map D.strToFloat $ D.index 3 df
  print $ (!!4) $ D.headers df
  print $ map D.strToFloat $ D.index 4 df
  print $ (!!5) $ D.headers df
  print $ D.unique $ D.index 5 df
  print $ D.factorize $ D.index 5 df
  print "The Whole Thing (deparsed)"
  print $ L.transpose $ D.body df
  print "The Whole Thing (parsed)"
  print $ D.toDMatrix df
  print "The Whole Thing (dummies)"
  print $ D.dummyDf $ L.transpose $ D.body df

main :: IO ()
main = do
  let file_name = "data/iris.csv"
  input <- readFile file_name
  let csv = CSV.parseCSV file_name input
  either handleError doWork csv
