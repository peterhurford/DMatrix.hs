import qualified Text.CSV as CSV
import qualified Data.List as L
import qualified Data.List.Utils as LU

strToFloat :: String -> Float
strToFloat = read

headers :: [[String]] -> [String]
headers = head

body :: [[String]] -> [[String]]
body xs = tail $ init xs

index :: Int -> [[String]] -> [String]
index index xs = map (!! index) (body xs)

indexByRow :: String -> [[String]] -> Maybe Int
indexByRow row xs = L.elemIndex h xs
                  where h = headers xs 

row :: String -> [[String]] -> [String]
row row xs = case indexByRow row xs of
  Just i -> index i xs
  Nothing -> []

factorize :: [String] -> [Float]
factorize xs = map strToFloat $
  map (LU.replace "setosa" "0") $
  map (LU.replace "versicolor" "1") $
  map (LU.replace "virginica" "2") xs

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
  let file_name = "iris.csv"
  input <- readFile file_name
  let csv = CSV.parseCSV file_name input
  either handleError doWork csv
